(* Two translations, chosen per effect: evidence passing when every handler
   resumes in tail position, continuations otherwise. *)

type error =
  { span : Ast.span
  ; message : string
  }

exception Unsupported of error

let unsupported span fmt =
  Printf.ksprintf (fun message -> raise (Unsupported { span; message })) fmt

type effects =
  { owner : (string, string) Hashtbl.t (* operation -> effect *)
  ; operations : (string, string list) Hashtbl.t (* effect -> operations *)
  ; delimited : (string, unit) Hashtbl.t (* effects needing continuations *)
  ; op_ty : (string, Types.ty) Hashtbl.t (* operation -> its function type *)
  }

let evidence_name op = Ast.generated [ "ev"; op ]

(* Returning out of one travels back through the handler that resumed into it,
   which only a continuation can do — and handling the effect discharged the
   enclosing function's row. *)
let no_return = Ast.generated [ "cps"; "no-return" ]
let continuation = Ast.generated [ "cps"; "k" ]
let counter = ref 0

let fresh prefix =
  incr counter;
  Ast.generated [ prefix; string_of_int !counter ]

(* Sorted, so caller and callee agree without communicating. *)
let evidence_of_row info (row : Types.row) =
  row
  |> List.map fst
  |> List.sort_uniq String.compare
  |> List.concat_map (fun label ->
    match Hashtbl.find_opt info.operations label with
    | Some ops -> ops
    | None -> [])
  |> List.sort_uniq String.compare

let row_of (t : Types.ty) =
  match t with
  | Types.Fn (_, _, row) -> row
  | _ -> []

let is_effectful info (t : Types.ty) = evidence_of_row info (row_of t) <> []

let evidence_ty info op =
  match Hashtbl.find_opt info.op_ty op with
  | Some t -> t
  | None -> Types.Unit

(* The type gains the evidence parameters too, or it describes the wrong
   arity. *)
let rec widen info (t : Types.ty) =
  match t with
  | Types.Fn (params, ret, row) ->
    Types.Fn
      ( List.map (widen info) params @ List.map (evidence_ty info) (evidence_of_row info row)
      , widen info ret
      , row )
  | other -> other

let is_delimited info (row : Types.row) =
  List.exists (fun (label, _) -> Hashtbl.mem info.delimited label) row

let node span it : Ast.cps_stmt = { Ast.it; span; ann = Types.Unit }
let var span ty name : Ast.cps_expr = { Ast.it = `Var name; span; ann = ty }

let ignored span : Ast.cps_expr = { Ast.it = `Bool false; span; ann = Types.Bool }

let call ?(result = Types.Unit) span callee args =
  let callee_ty =
    Types.Fn (List.map (fun (a : Ast.cps_expr) -> a.Ast.ann) args, result, [])
  in
  node
    span
    (`Expr { Ast.it = `Call (var span callee_ty callee, args); span; ann = result })

let fn_decl span name params body =
  node
    span
    (`Fn
      ( name
      , List.map (fun p -> { Ast.name = p; ty = None }) params
      , { Ast.ret = None; row = None; comptime = [] }
      , body ))

(* ---- tail-resumptive detection ---- *)

let rec count pick (body : Ast.reflected_stmt list) =
  List.fold_left (fun n s -> n + count_in pick s) 0 body

and count_in pick (s : Ast.reflected_stmt) =
  (if pick s then 1 else 0)
  +
  match s.Ast.it with
  | `Block body | `Fn (_, _, _, body) | `Run (body, _) -> count pick body
  | `If (_, t, e) ->
    count_in pick t + (match e with Some e -> count_in pick e | None -> 0)
  | `While (_, body) -> count_in pick body
  | `Match (_, cases) ->
    List.fold_left (fun n (_, body) -> n + count pick body) 0 cases
  | `Defer inner -> count_in pick inner
  | _ -> 0

let is_resume (s : Ast.reflected_stmt) =
  match s.Ast.it with
  | `Resume _ -> true
  | _ -> false

(* Left alone, its value never reaches the continuation. *)
let rec holds_return (s : Ast.reflected_stmt) =
  match s.Ast.it with
  | `Return _ -> true
  | `Block body -> List.exists holds_return body
  | `If (_, t, e) -> holds_return t || Option.fold ~none:false ~some:holds_return e
  | `While (_, body) -> holds_return body
  | `Match (_, cases) -> List.exists (fun (_, body) -> List.exists holds_return body) cases
  | `Defer inner -> holds_return inner
  | _ -> false

let is_return (s : Ast.reflected_stmt) =
  match s.Ast.it with
  | `Return _ -> true
  | _ -> false

(* A `ctl` arm resuming exactly once, last, behaves like a `fn` arm. Koka
   calls this bind-inversion. *)
let tail_resumptive (body : Ast.reflected_stmt list) =
  match List.rev body with
  | ({ Ast.it = `Resume value; _ } as last) :: earlier
    when count is_resume body = 1 && count is_return body = 0 ->
    Some (List.rev ({ last with Ast.it = `Return value } :: earlier))
  | _ -> None

(* "Needs no continuation" rather than "resumes in tail position": a `final ctl`
   never resumes, and what it needs is an unwind. *)
let arm_needs_no_continuation (a : Ast.reflected_stmt Ast.arm) =
  match a.Ast.arm_kind with
  | Ast.Op_fn | Ast.Op_final -> true
  | Ast.Op_ctl -> tail_resumptive a.Ast.arm_body <> None

let handlers_are_tail_resumptive handlers =
  List.for_all
    (fun (h : Ast.reflected_stmt Ast.handler) ->
      List.for_all arm_needs_no_continuation h.Ast.arms)
    handlers

(* ---- expressions ---- *)

let convert_body : (effects -> Ast.reflected_stmt list -> Ast.cps_stmt list) ref =
  ref (fun _ _ -> [])

let rec expr info (e : Ast.reflected_expr) : Ast.cps_expr =
  let widened = ref e.Ast.ann in
  let it : Ast.cps_expr_kind =
    match e.Ast.it with
    | #Ast.lit as l -> l
    (* A call site passes the same arguments whichever it reached. *)
    | `Lambda (params, signature, body) ->
      let row = row_of e.Ast.ann in
      if is_delimited info row
      then
        unsupported
          e.Ast.span
          "A function value cannot perform an effect whose handler resumes yet."
      else (
        let evidence =
          evidence_of_row info row
          |> List.map (fun op -> { Ast.name = evidence_name op; ty = None })
        in
        widened := widen info e.Ast.ann;
        `Lambda (params @ evidence, signature, !convert_body info body))
    | `Var name ->
      (* A bare reference would escape with the wrong arity. *)
      if is_effectful info e.Ast.ann
      then
        unsupported e.Ast.span "'%s' performs effects and cannot be used as a value yet." name
      else `Var name
    | `Call (callee, args) ->
      let args = List.map (expr info) args in
      (match callee.Ast.it with
       | `Var name when Hashtbl.mem info.owner name ->
         let ty =
           Types.Fn
             (List.map (fun (a : Ast.cps_expr) -> a.Ast.ann) args, e.Ast.ann, [])
         in
         `Call (var callee.Ast.span ty (evidence_name name), args)
       | _ ->
         let evidence =
           evidence_of_row info (row_of callee.Ast.ann)
           |> List.map (fun op -> var e.Ast.span (evidence_ty info op) (evidence_name op))
         in
         let callee =
           match callee.Ast.it with
           | `Var name ->
             { Ast.it = `Var name
             ; span = callee.Ast.span
             ; ann = widen info callee.Ast.ann
             }
           | _ -> expr info callee
         in
         `Call (callee, args @ evidence))
    | #Ast.arrays as a -> (Ast.map_arrays (expr info) a :> Ast.cps_expr_kind)
    | #Ast.strings as s -> (Ast.map_strings (expr info) s :> Ast.cps_expr_kind)
    | #Ast.vars as v -> (Ast.map_vars (expr info) v :> Ast.cps_expr_kind)
    | #Ast.ops as o -> (Ast.map_ops (expr info) o :> Ast.cps_expr_kind)
    | #Ast.logic as l -> (Ast.map_logic (expr info) l :> Ast.cps_expr_kind)
    | #Ast.tuple as t -> (Ast.map_tuple (expr info) t :> Ast.cps_expr_kind)
    | #Ast.record as r -> (Ast.map_record (expr info) r :> Ast.cps_expr_kind)
    | #Ast.variant_lit as v -> (Ast.map_variant_lit (expr info) v :> Ast.cps_expr_kind)
  in
  { Ast.it; span = e.Ast.span; ann = !widened }

let rec suspends info (e : Ast.reflected_expr) =
  match e.Ast.it with
  | `Lambda _ | #Ast.lit | `Var _ -> false
  | `Call (callee, args) ->
    (match callee.Ast.it with
     | `Var name when Hashtbl.mem info.owner name ->
       Hashtbl.mem info.delimited (Hashtbl.find info.owner name)
     | _ -> is_delimited info (row_of callee.Ast.ann))
    || List.exists (suspends info) args
  | `Assign (_, v) | `Unop (_, v) -> suspends info v
  | `Binop (_, a, b) | `And (a, b) | `Or (a, b) ->
    suspends info a || suspends info b
  | `Tuple items | `Array_lit items -> List.exists (suspends info) items
  | `Array_new (a, b) | `Array_get (a, b) -> suspends info a || suspends info b
  | `Array_set (target, index, v) ->
    suspends info target || suspends info index || suspends info v
  | `Str_get (a, b) -> suspends info a || suspends info b
  | `Array_len t | `Str_len t | `Tuple_get (t, _) | `Field (t, _) -> suspends info t
  | `Record_lit fields | `Variant (_, fields) ->
    List.exists (fun (_, v) -> suspends info v) fields
  | `Field_assign (r, _, v) -> suspends info r || suspends info v

let rec suspends_stmt info (s : Ast.reflected_stmt) =
  match s.Ast.it with
  | `Expr e | `Return (Some e) | `Var_decl (_, _, Some e) -> suspends info e
  | `Block body -> List.exists (suspends_stmt info) body
  | `If (c, t, e) ->
    suspends info c
    || suspends_stmt info t
    || (match e with
        | Some e -> suspends_stmt info e
        | None -> false)
  | `While (c, body) -> suspends info c || suspends_stmt info body
  | `Resume _ -> true
  | `Run (_, handlers) -> not (handlers_are_tail_resumptive handlers)
  | `Match (scrutinee, cases) ->
    suspends info scrutinee
    || List.exists (fun (_, body) -> List.exists (suspends_stmt info) body) cases
  | `Defer inner -> suspends_stmt info inner
  | _ -> false

let rec extract info (e : Ast.reflected_expr)
  : (Ast.reflected_expr * (string -> Ast.reflected_expr)) option
  =
  let rebuild it : Ast.reflected_expr = { e with Ast.it = it } in
  match e.Ast.it with
  | #Ast.lit | `Var _ | `Lambda _ -> None
  | `Call (_, args) when suspends info e && not (List.exists (suspends info) args) ->
    Some (e, fun name -> { Ast.it = `Var name; span = e.Ast.span; ann = e.Ast.ann })
  | `Call (callee, args) -> extract_list info args (fun args -> rebuild (`Call (callee, args)))
  | `Tuple items -> extract_list info items (fun items -> rebuild (`Tuple items))
  | `Array_lit items -> extract_list info items (fun items -> rebuild (`Array_lit items))
  | `Array_new (length, fill) ->
    extract_list info [ length; fill ] (function
      | [ length; fill ] -> rebuild (`Array_new (length, fill))
      | _ -> assert false)
  | `Array_get (target, index) ->
    extract_list info [ target; index ] (function
      | [ target; index ] -> rebuild (`Array_get (target, index))
      | _ -> assert false)
  | `Array_set (target, index, v) ->
    extract_list info [ target; index; v ] (function
      | [ target; index; v ] -> rebuild (`Array_set (target, index, v))
      | _ -> assert false)
  | `Array_len target ->
    extract info target |> Option.map (fun (c, f) -> c, fun n -> rebuild (`Array_len (f n)))
  | `Str_len target ->
    extract info target |> Option.map (fun (c, f) -> c, fun n -> rebuild (`Str_len (f n)))
  | `Str_get (target, index) ->
    extract_list info [ target; index ] (function
      | [ target; index ] -> rebuild (`Str_get (target, index))
      | _ -> assert false)
  | `Tuple_get (t, i) ->
    extract info t |> Option.map (fun (c, f) -> c, fun n -> rebuild (`Tuple_get (f n, i)))
  | `Field (t, label) ->
    extract info t |> Option.map (fun (c, f) -> c, fun n -> rebuild (`Field (f n, label)))
  | `Record_lit _ | `Field_assign _ | `Variant _ ->
    if suspends info e
    then unsupported e.Ast.span "An effect inside a record is not supported yet."
    else None
  | `Assign (name, v) ->
    extract info v |> Option.map (fun (c, f) -> c, fun n -> rebuild (`Assign (name, f n)))
  | `Unop (op, v) ->
    extract info v |> Option.map (fun (c, f) -> c, fun n -> rebuild (`Unop (op, f n)))
  | `Binop (op, a, b) ->
    extract_list info [ a; b ] (function
      | [ a; b ] -> rebuild (`Binop (op, a, b))
      | _ -> assert false)
  (* Hoisting out of `and`/`or` would evaluate the right side unconditionally. *)
  | `And (a, b) | `Or (a, b) ->
    if suspends info a || suspends info b
    then unsupported e.Ast.span "An effect inside 'and'/'or' is not supported yet."
    else None

and extract_list info items rebuild =
  let rec loop before = function
    | [] -> None
    | item :: after ->
      (match extract info item with
       | Some (c, f) -> Some (c, fun name -> rebuild (List.rev before @ [ f name ] @ after))
       | None -> loop (item :: before) after)
  in
  loop [] items

(* ---- continuation-passing form ---- *)

(* [ret] is what a `return` calls, [k] what the next statement runs under. They
   part company inside a branch, where the join resumes the rest of the body. *)
let rec cps info ret k ~at (stmts : Ast.reflected_stmt list) : Ast.cps_stmt list =
  match stmts with
  (* Reported against the construct that held them rather than nowhere. *)
  | [] -> [ call at k [ ignored at ] ]
  | s :: rest ->
    let span = s.Ast.span in
    (match s.Ast.it with
     | `Return value ->
       let value =
         match value with
         | Some v -> v
         | None -> { Ast.it = `Bool false; span; ann = Types.Unit }
       in
       (match extract info value with
        | Some (c, rebuild) -> sequence info span c (fun name ->
            cps info ret k ~at:span [ { s with Ast.it = `Return (Some (rebuild name)) } ])
        | None ->
          if String.equal ret no_return
          then
            unsupported
              span
              "A 'return' out of a 'run' block is not supported yet when its \
               handler resumes."
          else [ call span ret [ expr info value ] ])
     (* The arm keeps running afterwards: multi-shot falls out. *)
     | `Resume value ->
       let value =
         match value with
         | Some v -> expr info v
         | None -> ignored span
       in
       call span continuation [ value ] :: cps info ret k ~at:span rest
     (* Converted code leaves a scope by calling the continuation, by
        returning, or by an unwind. The first two are calls, so the deferred
        statement wraps each — which is what runs it once per exit when a
        handler resumes twice. The flag keeps an unwind reaching this frame
        afterwards from running it again. *)
     | `Defer inner ->
       let armed = fresh "armed" in
       let flag value : Ast.cps_expr =
         { Ast.it = (if value then `Bool true else `Bool false); span; ann = Types.Bool }
       in
       let disarm =
         node span (`Expr { Ast.it = `Assign (armed, flag false); span; ann = Types.Bool })
       in
       (* The deferred statement, then whatever this exit was on its way to. *)
       let leaving finish handed =
         if not (suspends_stmt info inner)
         then Option.to_list (stmt info inner) @ [ call span finish [ handed ] ]
         else (
           let after = fresh "resumed" in
           fn_decl span after [ fresh "x" ] [ call span finish [ handed ] ]
           :: cps info no_return after ~at:span [ inner ])
       in
       let wrapper finish =
         let name = fresh "leave" in
         let value = fresh "x" in
         name, fn_decl span name [ value ] (disarm :: leaving finish (var span Types.Unit value))
       in
       let ret', wrapped_ret =
         (* Nothing to wrap: a `return` here is already rejected. *)
         if String.equal ret no_return
         then no_return, []
         else (
           let name, declaration = wrapper ret in
           name, [ declaration ])
       in
       let k', wrapped_k = wrapper k in
       (node span (`Var_decl (armed, None, Some (flag true)))
        :: wrapped_ret)
       @ [ wrapped_k
         ; node
             span
             (`On_unwind
               ( cps info ret' k' ~at:span rest
                 (* No continuation here, so one performing an effect cannot run. *)
               , (if suspends_stmt info inner
                  then []
                  else
                    [ node
                        span
                        (`If
                          ( var span Types.Bool armed
                          , node span (`Block (disarm :: Option.to_list (stmt info inner)))
                          , None ))
                    ]) ))
         ]
     | `Expr e when suspends info e ->
       (match extract info e with
        | Some (c, rebuild) ->
          sequence info span c (fun name ->
            match (rebuild name).Ast.it with
            | `Var _ -> cps info ret k ~at:span rest
            | _ -> cps info ret k ~at:span ({ s with Ast.it = `Expr (rebuild name) } :: rest))
        | None -> unsupported span "This effect cannot be sequenced yet.")
     | `Var_decl (name, _, Some e) when suspends info e ->
       (match extract info e with
        | Some (c, rebuild) ->
          let bound = ref name in
          let build tmp =
            match (rebuild tmp).Ast.it with
            | `Var _ ->
              bound := name;
              cps info ret k ~at:span rest
            | _ ->
              bound := tmp;
              cps info ret k ~at:span ({ s with Ast.it = `Var_decl (name, None, Some (rebuild tmp)) } :: rest)
          in
          let tmp = fresh "v" in
          let body = build tmp in
          let next = fresh "k" in
          fn_decl span next [ !bound ] body :: invoke info span next c
        | None -> unsupported span "This effect cannot be sequenced yet.")
     | `If (cond, then_branch, else_branch) when suspends_stmt info s || holds_return s ->
       let join = fresh "join" in
       let branch b = node span (`Block (cps info ret join ~at:span [ b ])) in
       fn_decl span join [ fresh "x" ] (cps info ret k ~at:span rest)
       :: controlled_by info span cond (fun cond ->
            [ node
                span
                (`If
                  ( cond
                  , branch then_branch
                  , Some
                      (match else_branch with
                       | Some e -> branch e
                       (* Without this the join is never reached. *)
                       | None -> call span join [ ignored span ]) ))
            ])
     | `Match (scrutinee, cases) when suspends_stmt info s || holds_return s ->
       let join = fresh "join" in
       fn_decl span join [ fresh "x" ] (cps info ret k ~at:span rest)
       :: controlled_by info span scrutinee (fun scrutinee ->
            [ node
                span
                (`Match
                  ( scrutinee
                  , List.map (fun (pattern, body) -> pattern, cps info ret join ~at:span body) cases ))
            ])
     | `Block body when suspends_stmt info s || holds_return s ->
       let next = fresh "k" in
       [ fn_decl span next [ fresh "x" ] (cps info ret k ~at:span rest)
       ; node span (`Block (cps info ret next ~at:span body))
       ]
     (* The body's "what runs next" is the loop itself, so resuming carries
        on with the next iteration and resuming twice runs it twice. *)
     | `While (cond, body) when suspends_stmt info s ->
       let again = fresh "loop"
       and after = fresh "after" in
       [ fn_decl span after [ fresh "x" ] (cps info ret k ~at:span rest)
       ; (* Once per iteration, so extracting it goes inside the continuation
            the loop re-enters. *)
         fn_decl
           span
           again
           [ fresh "x" ]
           (controlled_by info span cond (fun cond ->
              [ node
                  span
                  (`If
                    ( cond
                    , node span (`Block (cps info ret again ~at:span [ body ]))
                    , Some (call span after [ ignored span ]) ))
              ]))
       ; call span again [ ignored span ]
       ]
     | `Run (body, handlers) when not (handlers_are_tail_resumptive handlers) ->
       run info ret span k handlers body rest
     (* The body may still return out of the converted function it stands in,
        so what follows is its continuation rather than a later statement. *)
     | `Run (body, handlers)
       when List.exists (fun b -> suspends_stmt info b || holds_return b) body ->
       let after = fresh "after" in
       let scope = fresh "scope" in
       fn_decl span after [ fresh "x" ] (cps info ret k ~at:span rest)
       :: (direct_arms info span ~scope handlers
           @ [ node
                 span
                 (`Scope
                   ( scope
                   , cps info ret after ~at:span body
                   (* An abort skipped the body's own continuation. *)
                   , [ call span after [ ignored span ] ] )) ])
     | _ ->
       (match stmt info s with
        | Some s -> s :: cps info ret k ~at:span rest
        | None -> cps info ret k ~at:span rest))

(* Evaluated before the statement it controls rather than inside it. *)
and controlled_by info span (cond : Ast.reflected_expr) build =
  if not (suspends info cond)
  then build (expr info cond)
  else (
    match extract info cond with
    | Some (c, rebuild) ->
      sequence info span c (fun name -> build (expr info (rebuild name)))
    | None -> unsupported span "This effect cannot be sequenced yet.")

and sequence info span c build =
  let name = fresh "v" in
  let next = fresh "k" in
  fn_decl span next [ name ] (build name) :: invoke info span next c

and invoke info span next (c : Ast.reflected_expr) : Ast.cps_stmt list =
  match c.Ast.it with
  | `Call (callee, args) ->
    let args = List.map (expr info) args in
    let target, evidence =
      match callee.Ast.it with
      | `Var name when Hashtbl.mem info.owner name -> evidence_name name, []
      | `Var name ->
        ( name
        , evidence_of_row info (row_of callee.Ast.ann)
          |> List.map (fun op -> var span (evidence_ty info op) (evidence_name op)) )
      | _ -> unsupported span "Only a named function may perform an effect here."
    in
    [ call span target (args @ evidence @ [ var span Types.Unit next ]) ]
  | _ -> unsupported span "This effect cannot be sequenced yet."

(* An arm that never calls it abandons the rest of the body: abort. *)
and run info ret span k handlers body rest : Ast.cps_stmt list =
  let after = fresh "after" in
  let finished = fresh "finished" in
  let arms =
    List.concat_map
      (fun (h : Ast.reflected_stmt Ast.handler) ->
        List.map
          (fun (a : Ast.reflected_stmt Ast.arm) ->
            let arm_body =
              match a.Ast.arm_kind with
              | Ast.Op_fn ->
                cps info continuation continuation ~at:span a.Ast.arm_body
              | Ast.Op_ctl | Ast.Op_final ->
                cps info finished finished ~at:span a.Ast.arm_body
            in
            fn_decl span (evidence_name a.Ast.arm_name) (a.Ast.arm_params @ [ continuation ]) arm_body)
          h.Ast.arms)
      handlers
  in
  (* What follows the block runs once after the handler is done, not once
     per resumption. *)
  (fn_decl span after [ fresh "x" ] (cps info ret k ~at:span rest)
   :: fn_decl span finished [ fresh "x" ] []
   :: arms)
  @ cps info ret finished ~at:span body
  @ [ call span after [ ignored span ] ]

(* ---- evidence-only translation ---- *)

and stmt info (s : Ast.reflected_stmt) : Ast.cps_stmt option =
  let keep it = Some { Ast.it; span = s.Ast.span; ann = s.Ast.ann } in
  match s.Ast.it with
  | `Effect_decl _ | `Type_decl _ -> None
  | `Match (scrutinee, cases) ->
    keep
      (`Match
        ( expr info scrutinee
        , List.map (fun (p, body) -> p, List.map (block info) body) cases ))
  | `Resume _ -> unsupported s.Ast.span "'resume' outside a handler."
  | `Fn (name, params, signature, body) ->
    let row = row_of s.Ast.ann in
    let evidence =
      evidence_of_row info row |> List.map (fun op -> { Ast.name = evidence_name op; ty = None })
    in
    if is_delimited info row
    then
      keep
        (`Fn
          ( name
          , params @ evidence @ [ { Ast.name = continuation; ty = None } ]
          , signature
          , cps info continuation continuation ~at:s.Ast.span body ))
    else keep (`Fn (name, params @ evidence, signature, sequence_body info body))
  | `Run (body, handlers) when not (handlers_are_tail_resumptive handlers) ->
    unsupported
      s.Ast.span
      "A 'run' whose handler resumes is not supported yet in this position."
  | `Run (body, handlers) ->
    let scope = fresh "scope" in
    keep
      (`Block
        (direct_arms info s.Ast.span ~scope handlers
         (* The statements after the block are still statements. *)
         @ [ node s.Ast.span (`Scope (scope, sequence_body info body, [])) ]))
  | #Ast.stmts as st ->
    keep (Ast.map_stmts (expr info) (block info) st :> Ast.cps_stmt_kind)

(* Each arm is a function of the operation's parameters alone. *)
and direct_arms info span ~scope handlers =
  List.concat_map
    (fun (h : Ast.reflected_stmt Ast.handler) ->
      List.map
        (fun (a : Ast.reflected_stmt Ast.arm) ->
          let converted =
            match a.Ast.arm_kind with
            | Ast.Op_fn -> sequence_body info a.Ast.arm_body
            | Ast.Op_ctl -> sequence_body info (Option.get (tail_resumptive a.Ast.arm_body))
            (* Whatever it did, it does not go back. *)
            | Ast.Op_final -> sequence_body info a.Ast.arm_body @ [ node span (`Abort scope) ]
          in
          fn_decl span (evidence_name a.Ast.arm_name) a.Ast.arm_params converted)
        h.Ast.arms)
    handlers

and block info (s : Ast.reflected_stmt) : Ast.cps_stmt =
  match stmt info s with
  | Some s -> s
  | None -> node s.Ast.span (`Block [])

and sequence_body info (stmts : Ast.reflected_stmt list) : Ast.cps_stmt list =
  match stmts with
  | [] -> []
  (* A `return` among them returns from the function they are in. *)
  | { Ast.it = `Run (body, handlers); span; _ } :: rest
    when not (handlers_are_tail_resumptive handlers) ->
    let nothing = fresh "nothing" in
    (fn_decl span nothing [ fresh "x" ] []
     :: run info no_return span nothing handlers body [])
    @ sequence_body info rest
  (* A `run` deeper down — in a branch, a loop or an arm — needs a
     continuation just the same. *)
  | s :: rest when suspends_stmt info s ->
    let nothing = fresh "nothing" in
    (fn_decl s.Ast.span nothing [ fresh "x" ] []
     :: cps info no_return nothing ~at:s.Ast.span [ s ])
    @ sequence_body info rest
  | s :: rest ->
    (match stmt info s with
     | Some s -> s :: sequence_body info rest
     | None -> sequence_body info rest)

(* ---- entry point ---- *)

let () = convert_body := sequence_body

let collect (p : Ast.reflected_stmt list) =
  let info =
    { owner = Hashtbl.create 16
    ; operations = Hashtbl.create 8
    ; delimited = Hashtbl.create 8
    ; op_ty = Hashtbl.create 16
    }
  in
  List.iter
    (fun (s : Ast.reflected_stmt) ->
      match s.Ast.it with
      | `Effect_decl (name, _, ops) ->
        Hashtbl.replace
          info.operations
          name
          (List.map (fun (o : Ast.op_decl) -> o.Ast.op_name) ops |> List.sort String.compare);
        List.iter
          (fun (o : Ast.op_decl) -> Hashtbl.replace info.owner o.Ast.op_name name)
          ops
      | _ -> ())
    p;
  (* A row says which effects a function performs, not which handler catches
     them, so one aborting handler delimits that effect everywhere. *)
  let rec scan (s : Ast.reflected_stmt) =
    (match s.Ast.it with
     | `Run (_, handlers) when not (handlers_are_tail_resumptive handlers) ->
       List.iter
         (fun (h : Ast.reflected_stmt Ast.handler) ->
           Hashtbl.replace info.delimited h.Ast.handled ())
         handlers
     | _ -> ());
    match s.Ast.it with
    | `Block body | `Fn (_, _, _, body) -> List.iter scan body
    | `If (_, t, e) ->
      scan t;
      Option.iter scan e
    | `While (_, body) -> scan body
    | `Run (body, handlers) ->
      List.iter scan body;
      List.iter
        (fun (h : Ast.reflected_stmt Ast.handler) ->
          List.iter
            (fun (a : Ast.reflected_stmt Ast.arm) -> List.iter scan a.Ast.arm_body)
            h.Ast.arms)
        handlers
    | `Match (_, cases) -> List.iter (fun (_, body) -> List.iter scan body) cases
    | `Defer inner -> scan inner
    | _ -> ()
  in
  List.iter scan p;
  let rec harvest_expr (e : Ast.reflected_expr) =
    match e.Ast.it with
    | `Call (callee, args) ->
      (match callee.Ast.it with
       | `Var name when Hashtbl.mem info.owner name ->
         Hashtbl.replace info.op_ty name callee.Ast.ann
       | _ -> harvest_expr callee);
      List.iter harvest_expr args
    | `Assign (_, v) | `Unop (_, v) -> harvest_expr v
    | `Binop (_, a, b) | `And (a, b) | `Or (a, b) ->
      harvest_expr a;
      harvest_expr b
    | _ -> ()
  in
  let rec harvest (s : Ast.reflected_stmt) =
    (match s.Ast.it with
     | `Expr e | `Return (Some e) | `Var_decl (_, _, Some e) -> harvest_expr e
     | `If (c, _, _) | `While (c, _) -> harvest_expr c
     | `Resume (Some e) -> harvest_expr e
     | `Match (scrutinee, _) -> harvest_expr scrutinee
     | _ -> ());
    match s.Ast.it with
    | `Block body | `Fn (_, _, _, body) -> List.iter harvest body
    | `If (_, t, e) ->
      harvest t;
      Option.iter harvest e
    | `While (_, body) -> harvest body
    | `Run (body, handlers) ->
      List.iter harvest body;
      List.iter
        (fun (h : Ast.reflected_stmt Ast.handler) ->
          List.iter
            (fun (a : Ast.reflected_stmt Ast.arm) -> List.iter harvest a.Ast.arm_body)
            h.Ast.arms)
        handlers
    | `Match (_, cases) -> List.iter (fun (_, body) -> List.iter harvest body) cases
    | `Defer inner -> harvest inner
    | _ -> ()
  in
  List.iter harvest p;
  info

let program (p : Ast.reflected_stmt list) : (Ast.cps_stmt list, error) result =
  counter := 0;
  let info = collect p in
  try Ok (sequence_body info p) with
  | Unsupported e -> Error e
