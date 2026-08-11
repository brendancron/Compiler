(* Effect constructs become closures and calls, which is why the interpreter has
   no handler stack.

   Two translations, chosen per effect. An effect whose handlers all resume in
   tail position needs only *evidence passing*: a function whose row mentions it
   gains one parameter per operation, performing an operation calls that
   parameter, and `run`/`handle` binds the arms as local functions. Nothing is
   restructured.

   An effect with a handler that aborts or resumes more than once needs
   *continuations*. Everything following an operation becomes a closure, which
   the arm receives. Resuming is calling it — twice, if the handler likes — and
   falling off the end of an arm abandons it, which is abort. *)

type error =
  { span : Ast.span
  ; message : string
  }

exception Unsupported of error

let unsupported span fmt =
  Printf.ksprintf (fun message -> raise (Unsupported { span; message })) fmt

type effects =
  { owner : (string, string) Hashtbl.t (* operation -> effect *)
  ; kind : (string, Ast.op_kind) Hashtbl.t
  ; operations : (string, string list) Hashtbl.t (* effect -> operations *)
  ; delimited : (string, unit) Hashtbl.t (* effects needing continuations *)
  ; op_ty : (string, Types.ty) Hashtbl.t (* operation -> its function type *)
  }

let evidence_name op = "__op_" ^ op
let continuation = "__k"
let counter = ref 0

let fresh prefix =
  incr counter;
  Printf.sprintf "__%s%d" prefix !counter

(* Sorted and deduplicated, so a caller and a callee agree on the order without
   having to communicate. *)
let evidence_of_row info (row : Types.row) =
  row
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

(* Evidence parameters are appended to a function, so its type gains them too.
   Leaving the original type in place would describe a function of the wrong
   arity. *)
let widen info (t : Types.ty) =
  match t with
  | Types.Fn (params, ret, row) ->
    Types.Fn (params @ List.map (evidence_ty info) (evidence_of_row info row), ret, row)
  | other -> other

(* Whether reaching this row can suspend, which forces the caller into
   continuation-passing form too. *)
let is_delimited info (row : Types.row) = List.exists (Hashtbl.mem info.delimited) row

let node span it : Ast.cps_stmt = { Ast.it; span; ann = Types.Unit }
let var span ty name : Ast.cps_expr = { Ast.it = `Var name; span; ann = ty }

(* There is no unit literal, and the value is always discarded. *)
let ignored span : Ast.cps_expr = { Ast.it = `Bool false; span; ann = Types.Bool }

(* The callee's type is recovered from what it is being handed: a continuation
   or an evidence function takes exactly these arguments and produces
   [result]. *)
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
      , { Ast.ret = None; row = None }
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
  | _ -> 0

let is_resume (s : Ast.reflected_stmt) =
  match s.Ast.it with
  | `Resume _ -> true
  | _ -> false

let is_return (s : Ast.reflected_stmt) =
  match s.Ast.it with
  | `Return _ -> true
  | _ -> false

(* A `ctl` arm that resumes exactly once, as the last thing it does, behaves
   like a `fn` arm: the resumed value is what the operation returns. Koka calls
   this bind-inversion. *)
let tail_resumptive (body : Ast.reflected_stmt list) =
  match List.rev body with
  | ({ Ast.it = `Resume value; _ } as last) :: earlier
    when count is_resume body = 1 && count is_return body = 0 ->
    Some (List.rev ({ last with Ast.it = `Return value } :: earlier))
  | _ -> None

let arm_is_tail_resumptive (a : Ast.reflected_stmt Ast.arm) =
  match a.Ast.arm_kind with
  | Ast.Op_fn -> true
  | Ast.Op_ctl -> tail_resumptive a.Ast.arm_body <> None

let handlers_are_tail_resumptive handlers =
  List.for_all
    (fun (h : Ast.reflected_stmt Ast.handler) ->
      List.for_all arm_is_tail_resumptive h.Ast.arms)
    handlers

(* ---- expressions ---- *)

let rec expr info (e : Ast.reflected_expr) : Ast.cps_expr =
  let it : Ast.cps_expr_kind =
    match e.Ast.it with
    | #Ast.lit as l -> l
    | `Var name ->
      (* A bare reference to an effectful function would escape with the wrong
         arity, since its evidence is only known at call sites. *)
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
    | #Ast.vars as v -> (Ast.map_vars (expr info) v :> Ast.cps_expr_kind)
    | #Ast.ops as o -> (Ast.map_ops (expr info) o :> Ast.cps_expr_kind)
    | #Ast.logic as l -> (Ast.map_logic (expr info) l :> Ast.cps_expr_kind)
    | #Ast.indexing as i -> (Ast.map_indexing (expr info) i :> Ast.cps_expr_kind)
    | #Ast.array_lit as a -> (Ast.map_array_lit (expr info) a :> Ast.cps_expr_kind)
  in
  { Ast.it; span = e.Ast.span; ann = e.Ast.ann }

let rec suspends info (e : Ast.reflected_expr) =
  match e.Ast.it with
  | #Ast.lit | `Var _ -> false
  | `Call (callee, args) ->
    (match callee.Ast.it with
     | `Var name when Hashtbl.mem info.owner name ->
       Hashtbl.mem info.delimited (Hashtbl.find info.owner name)
     | _ -> is_delimited info (row_of callee.Ast.ann))
    || List.exists (suspends info) args
  | `Assign (_, v) | `Unop (_, v) -> suspends info v
  | `Binop (_, a, b) | `And (a, b) | `Or (a, b) | `Index (a, b) ->
    suspends info a || suspends info b
  | `Index_assign (a, b, c) ->
    suspends info a || suspends info b || suspends info c
  | `Array_lit items -> List.exists (suspends info) items

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
  | _ -> false

(* Pull the first suspending call out of [e], with a rebuild that puts a
   variable in its place. *)
let rec extract info (e : Ast.reflected_expr)
  : (Ast.reflected_expr * (string -> Ast.reflected_expr)) option
  =
  let rebuild it : Ast.reflected_expr = { e with Ast.it = it } in
  match e.Ast.it with
  | #Ast.lit | `Var _ -> None
  | `Call (_, args) when suspends info e && not (List.exists (suspends info) args) ->
    Some (e, fun name -> { Ast.it = `Var name; span = e.Ast.span; ann = e.Ast.ann })
  | `Call (callee, args) -> extract_list info args (fun args -> rebuild (`Call (callee, args)))
  | `Index (a, b) ->
    extract_list info [ a; b ] (function
      | [ a; b ] -> rebuild (`Index (a, b))
      | _ -> assert false)
  | `Index_assign (a, b, c) ->
    extract_list info [ a; b; c ] (function
      | [ a; b; c ] -> rebuild (`Index_assign (a, b, c))
      | _ -> assert false)
  | `Array_lit items ->
    extract_list info items (fun items -> rebuild (`Array_lit items))
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

(* [k] names the function to invoke with this block's result. *)
let rec cps info k (stmts : Ast.reflected_stmt list) : Ast.cps_stmt list =
  match stmts with
  | [] ->
    let span = { Ast.line = 0; col = 0 } in
    [ call span k [ ignored span ] ]
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
            cps info k [ { s with Ast.it = `Return (Some (rebuild name)) } ])
        | None -> [ call span k [ expr info value ] ])
     (* The arm keeps running afterwards, which is what makes multi-shot fall
        out: two `resume`s call the continuation twice. *)
     | `Resume value ->
       let value =
         match value with
         | Some v -> expr info v
         | None -> ignored span
       in
       call span continuation [ value ] :: cps info k rest
     | `Expr e when suspends info e ->
       (match extract info e with
        | Some (c, rebuild) ->
          sequence info span c (fun name ->
            match (rebuild name).Ast.it with
            (* The statement was nothing but the call. *)
            | `Var _ -> cps info k rest
            | _ -> cps info k ({ s with Ast.it = `Expr (rebuild name) } :: rest))
        | None -> unsupported span "This effect cannot be sequenced yet.")
     | `Var_decl (name, _, Some e) when suspends info e ->
       (match extract info e with
        | Some (c, rebuild) ->
          let bound = ref name in
          let build tmp =
            match (rebuild tmp).Ast.it with
            (* The resumed value is the whole initializer, so bind it directly. *)
            | `Var _ ->
              bound := name;
              cps info k rest
            | _ ->
              bound := tmp;
              cps info k ({ s with Ast.it = `Var_decl (name, None, Some (rebuild tmp)) } :: rest)
          in
          let tmp = fresh "v" in
          let body = build tmp in
          let next = fresh "k" in
          fn_decl span next [ !bound ] body :: invoke info span next c
        | None -> unsupported span "This effect cannot be sequenced yet.")
     | `If (cond, then_branch, else_branch) when suspends_stmt info s ->
       let join = fresh "join" in
       let branch b = node span (`Block (cps info join [ b ])) in
       [ fn_decl span join [ fresh "x" ] (cps info k rest)
       ; node
           span
           (`If
             ( expr info cond
             , branch then_branch
             , Some
                 (match else_branch with
                  | Some e -> branch e
                  (* Without this the join is never reached. *)
                  | None -> call span join [ ignored span ]) ))
       ]
     | `Block body when suspends_stmt info s ->
       let next = fresh "k" in
       [ fn_decl span next [ fresh "x" ] (cps info k rest)
       ; node span (`Block (cps info next body))
       ]
     | `While _ when suspends_stmt info s ->
       unsupported span "An effect inside a loop is not supported yet."
     | `Run (body, handlers) when not (handlers_are_tail_resumptive handlers) ->
       run info span k handlers body rest
     | _ ->
       (match stmt info s with
        | Some s -> s :: cps info k rest
        | None -> cps info k rest))

(* Bind [c]'s result to a fresh name and continue with [build]. *)
and sequence info span c build =
  let name = fresh "v" in
  let next = fresh "k" in
  fn_decl span next [ name ] (build name) :: invoke info span next c

(* Perform [c], handing it [next] as its continuation. *)
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

(* `run { body } handle e { arms }` installs the arms and gives the body a
   continuation that resumes execution after the block. An arm that never calls
   its own continuation therefore abandons the rest of the body: abort. *)
and run info span k handlers body rest : Ast.cps_stmt list =
  let after = fresh "after" in
  let arms =
    List.concat_map
      (fun (h : Ast.reflected_stmt Ast.handler) ->
        List.map
          (fun (a : Ast.reflected_stmt Ast.arm) ->
            let arm_body =
              match a.Ast.arm_kind with
              (* Auto-resume: the arm's value is what the operation returns. *)
              | Ast.Op_fn -> cps info continuation a.Ast.arm_body
              | Ast.Op_ctl -> cps info after a.Ast.arm_body
            in
            fn_decl span (evidence_name a.Ast.arm_name) (a.Ast.arm_params @ [ continuation ]) arm_body)
          h.Ast.arms)
      handlers
  in
  (fn_decl span after [ fresh "x" ] (cps info k rest) :: arms) @ cps info after body

(* ---- evidence-only translation ---- *)

and stmt info (s : Ast.reflected_stmt) : Ast.cps_stmt option =
  let keep it = Some { Ast.it; span = s.Ast.span; ann = s.Ast.ann } in
  match s.Ast.it with
  | `Effect_decl _ -> None
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
          , cps info continuation body ))
    else keep (`Fn (name, params @ evidence, signature, sequence_body info body))
  | `Run (body, handlers) ->
    let arms =
      List.concat_map
        (fun (h : Ast.reflected_stmt Ast.handler) ->
          List.map
            (fun (a : Ast.reflected_stmt Ast.arm) ->
              let body =
                match a.Ast.arm_kind with
                | Ast.Op_fn -> a.Ast.arm_body
                | Ast.Op_ctl -> Option.get (tail_resumptive a.Ast.arm_body)
              in
              fn_decl s.Ast.span (evidence_name a.Ast.arm_name) a.Ast.arm_params (sequence_body info body))
            h.Ast.arms)
        handlers
    in
    keep (`Block (arms @ sequence_body info body))
  | #Ast.stmts as st -> keep (Ast.map_stmts (expr info) (block info) st)

and block info (s : Ast.reflected_stmt) : Ast.cps_stmt =
  match stmt info s with
  | Some s -> s
  | None -> node s.Ast.span (`Block [])

(* Walk a statement list, switching into continuation-passing form at the first
   `run` that needs it — everything after that run becomes its continuation. *)
and sequence_body info (stmts : Ast.reflected_stmt list) : Ast.cps_stmt list =
  match stmts with
  | [] -> []
  | ({ Ast.it = `Run (body, handlers); span; _ } as s) :: rest
    when not (handlers_are_tail_resumptive handlers) ->
    ignore s;
    let done_ = fresh "done" in
    fn_decl span done_ [ fresh "x" ] (sequence_body info rest)
    :: run info span done_ handlers body []
  | s :: rest ->
    (match stmt info s with
     | Some s -> s :: sequence_body info rest
     | None -> sequence_body info rest)

(* ---- entry point ---- *)

let collect (p : Ast.reflected_stmt list) =
  let info =
    { owner = Hashtbl.create 16
    ; kind = Hashtbl.create 16
    ; operations = Hashtbl.create 8
    ; delimited = Hashtbl.create 8
    ; op_ty = Hashtbl.create 16
    }
  in
  List.iter
    (fun (s : Ast.reflected_stmt) ->
      match s.Ast.it with
      | `Effect_decl (name, ops) ->
        Hashtbl.replace
          info.operations
          name
          (List.map (fun (o : Ast.op_decl) -> o.Ast.op_name) ops |> List.sort String.compare);
        List.iter
          (fun (o : Ast.op_decl) ->
            Hashtbl.replace info.owner o.Ast.op_name name;
            Hashtbl.replace info.kind o.Ast.op_name o.Ast.op_kind)
          ops
      | _ -> ())
    p;
  (* Whole-program: a function's row says which effects it may perform, not
     which handler catches them, so one aborting handler makes the effect
     delimited everywhere. Every effect on the same `run` goes with it, so that
     all of its arms share one calling convention. *)
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
    | _ -> ()
  in
  List.iter harvest p;
  info

let program (p : Ast.reflected_stmt list) : (Ast.cps_stmt list, error) result =
  counter := 0;
  let info = collect p in
  try Ok (sequence_body info p) with
  | Unsupported e -> Error e
