(* Effect constructs become closures and calls, which is why the interpreter has
   no handler stack.

   Evidence passing: a function whose row is non-empty gains one parameter per
   operation in that row, an operation call becomes a call to that parameter,
   and `run`/`handle` binds the arms as local functions and threads them into
   every effectful call in its body.

   Only `fn` operations are handled. `ctl` needs a real continuation, and
   row-polymorphic functions need monomorphization first — the arity of the
   evidence depends on the row a function is instantiated at. *)

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
  }

let evidence_name op = "__op_" ^ op

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

let var span name : Ast.cps_expr = { Ast.it = `Var name; span; ann = Types.Unit }

(* How many times an arm can resume, and where. Only the last-statement case is
   compilable without a continuation. *)
let rec resumes (body : Ast.reflected_stmt list) =
  List.fold_left (fun n s -> n + resumes_in s) 0 body

and resumes_in (s : Ast.reflected_stmt) =
  match s.Ast.it with
  | `Resume _ -> 1
  | `Block body -> resumes body
  | `If (_, t, e) -> resumes_in t + (match e with Some e -> resumes_in e | None -> 0)
  | `While (_, body) -> resumes_in body
  | `Fn (_, _, _, body) | `Run (body, _) -> resumes body
  | _ -> 0

let rec returns (body : Ast.reflected_stmt list) =
  List.fold_left (fun n s -> n + returns_in s) 0 body

and returns_in (s : Ast.reflected_stmt) =
  match s.Ast.it with
  | `Return _ -> 1
  | `Block body -> returns body
  | `If (_, t, e) -> returns_in t + (match e with Some e -> returns_in e | None -> 0)
  | `While (_, body) -> returns_in body
  | _ -> 0

(* A `ctl` arm that resumes exactly once, as the last thing it does, behaves
   like a `fn` arm: the resumed value is simply what the operation returns. Koka
   calls this bind-inversion. Anything else needs a real continuation. *)
let tail_resumptive (body : Ast.reflected_stmt list) =
  match List.rev body with
  | ({ Ast.it = `Resume value; _ } as last) :: earlier
    when resumes body = 1 && returns body = 0 ->
    let return : Ast.reflected_stmt =
      { Ast.it = `Return value; span = last.Ast.span; ann = last.Ast.ann }
    in
    Some (List.rev (return :: earlier))
  | _ -> None

let rec expr info (e : Ast.reflected_expr) : Ast.cps_expr =
  let it : Ast.cps_expr_kind =
    match e.Ast.it with
    | #Ast.lit as l -> l
    | `Var name ->
      (* A bare reference to an effectful function would escape with the wrong
         arity, since its evidence is only known at call sites. *)
      if is_effectful info e.Ast.ann
      then
        unsupported
          e.Ast.span
          "'%s' performs effects and cannot be used as a value yet."
          name
      else `Var name
    | `Call (callee, args) ->
      let args = List.map (expr info) args in
      (match callee.Ast.it with
       (* Performing an operation is a call to the evidence for it. *)
       (* A tail-resumptive `ctl` has the same calling convention as a `fn`, and
          a handler that is not tail-resumptive is rejected at its own site. *)
       | `Var name when Hashtbl.mem info.owner name ->
         `Call (var callee.Ast.span (evidence_name name), args)
       | _ ->
         let evidence =
           evidence_of_row info (row_of callee.Ast.ann)
           |> List.map (fun op -> var e.Ast.span (evidence_name op))
         in
         (* Naming a function in order to call it is not escaping, so it skips
            the check in the [`Var] case. *)
         let callee =
           match callee.Ast.it with
           | `Var name ->
             { Ast.it = `Var name; span = callee.Ast.span; ann = callee.Ast.ann }
           | _ -> expr info callee
         in
         `Call (callee, args @ evidence))
    | #Ast.vars as v -> (Ast.map_vars (expr info) v :> Ast.cps_expr_kind)
    | #Ast.ops as o -> (Ast.map_ops (expr info) o :> Ast.cps_expr_kind)
    | #Ast.logic as l -> (Ast.map_logic (expr info) l :> Ast.cps_expr_kind)
  in
  { Ast.it; span = e.Ast.span; ann = e.Ast.ann }

let rec stmt info (s : Ast.reflected_stmt) : Ast.cps_stmt option =
  let keep it : Ast.cps_stmt option =
    Some { Ast.it; span = s.Ast.span; ann = s.Ast.ann }
  in
  match s.Ast.it with
  (* Declaring an effect binds operations for the checker only. *)
  | `Effect_decl _ -> None
  | `Resume _ -> unsupported s.Ast.span "'resume' is not supported yet."
  | `Fn (name, params, signature, body) ->
    let evidence =
      evidence_of_row info (row_of s.Ast.ann)
      |> List.map (fun op -> { Ast.name = evidence_name op; ty = None })
    in
    keep (`Fn (name, params @ evidence, signature, List.map (block info) body))
  | `Run (body, handlers) ->
    let arms =
      List.concat_map
        (fun (h : Ast.reflected_stmt Ast.handler) ->
          List.map
            (fun (a : Ast.reflected_stmt Ast.arm) ->
              let body =
                match a.Ast.arm_kind with
                | Ast.Op_fn -> a.Ast.arm_body
                | Ast.Op_ctl ->
                  (match tail_resumptive a.Ast.arm_body with
                   | Some body -> body
                   | None ->
                     unsupported
                       s.Ast.span
                       "'%s' does not resume in tail position, which needs a continuation."
                       a.Ast.arm_name)
              in
              { Ast.it =
                  `Fn
                    ( evidence_name a.Ast.arm_name
                    , List.map (fun p -> { Ast.name = p; ty = None }) a.Ast.arm_params
                    , { Ast.ret = None; row = None }
                    , List.map (block info) body )
              ; span = s.Ast.span
              ; ann = Types.Unit
              })
            h.Ast.arms)
        handlers
    in
    (* The arms are ordinary functions in the block the body runs in, so the
       body's calls can pass them along by name. *)
    keep (`Block (arms @ List.map (block info) body))
  | #Ast.stmts as st -> keep (Ast.map_stmts (expr info) (block info) st)

(* A nested statement has to produce something; only [`Effect_decl] erases, and
   the grammar does not allow one here. *)
and block info (s : Ast.reflected_stmt) : Ast.cps_stmt =
  match stmt info s with
  | Some s -> s
  | None -> { Ast.it = `Block []; span = s.Ast.span; ann = s.Ast.ann }

let collect (p : Ast.reflected_stmt list) =
  let info =
    { owner = Hashtbl.create 16
    ; kind = Hashtbl.create 16
    ; operations = Hashtbl.create 8
    }
  in
  List.iter
    (fun (s : Ast.reflected_stmt) ->
      match s.Ast.it with
      | `Effect_decl (name, ops) ->
        Hashtbl.replace
          info.operations
          name
          (List.map (fun (o : Ast.op_decl) -> o.Ast.op_name) ops
           |> List.sort String.compare);
        List.iter
          (fun (o : Ast.op_decl) ->
            Hashtbl.replace info.owner o.Ast.op_name name;
            Hashtbl.replace info.kind o.Ast.op_name o.Ast.op_kind)
          ops
      | _ -> ())
    p;
  info

let program (p : Ast.reflected_stmt list) : (Ast.cps_stmt list, error) result =
  let info = collect p in
  try Ok (List.filter_map (stmt info) p) with
  | Unsupported e -> Error e
