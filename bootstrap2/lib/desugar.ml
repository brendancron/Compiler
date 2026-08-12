open Ast

type error =
  { span : span
  ; message : string
  }

exception Error of error

(* Handlers declared with `handler name : effect { ... }`, inlined into the
   `run` blocks that name them. *)
let declared : (string, stmt handler) Hashtbl.t = Hashtbl.create 8

let rec expr (e : expr) : desugared_expr =
  let sp = e.span in
  let it : desugared_expr_kind =
    match e.it with
    | #lit as l -> l
    | #vars as v -> (map_vars expr v :> desugared_expr_kind)
    | #ops as o -> (map_ops expr o :> desugared_expr_kind)
    | #logic as l -> (map_logic expr l :> desugared_expr_kind)
    | #compound as c -> (map_compound expr c :> desugared_expr_kind)
    | #indexing as i -> (map_indexing expr i :> desugared_expr_kind)
    | #tuple as t -> (map_tuple expr t :> desugared_expr_kind)
    | #record as r -> (map_record expr r :> desugared_expr_kind)
    | #nominal as n -> (map_nominal expr n :> desugared_expr_kind)
    | #collection as c -> (map_collection expr c :> desugared_expr_kind)
    | #method_call as m -> (map_method_call expr m :> desugared_expr_kind)
    | #reflect as r -> (map_reflect expr r :> desugared_expr_kind)
  in
  { it; span = sp; ann = () }

let rec stmt (s : stmt) : desugared_stmt =
  let sp = s.span in
  let it : desugared_stmt_kind =
    match s.it with
    (* for (init; cond; step) body  ⇒  { init; while (cond) { body; step; } } *)
    | `For (init, cond, step, body) ->
      let cond =
        match cond with
        (* An absent condition has no location of its own. *)
        | None -> at sp (`Bool true)
        | Some c -> expr c
      in
      let body =
        let body = stmt body in
        match step with
        | None -> body
        | Some st ->
          let step = expr st in
          (* Synthesized nodes take the span of the source they stand in for, so
             an error in the step is not reported at the `for`. *)
          { it = `Block [ body; { it = `Expr step; span = step.span; ann = () } ]
          ; span = body.span
          ; ann = ()
          }
      in
      let loop = at sp (`While (cond, body)) in
      `Block
        (match init with
         | Some i -> [ stmt i; loop ]
         | None -> [ loop ])
    | #stmts as s -> (map_stmts expr stmt s :> desugared_stmt_kind)
    | #effects as e -> (map_effects expr stmt (clause sp) e :> desugared_stmt_kind)
    | #type_defs as t -> t
    | #matching as m -> (map_matching expr stmt m :> desugared_stmt_kind)
    | #op_defs as o -> (map_op_defs stmt o :> desugared_stmt_kind)
    | #method_defs as m -> (map_method_defs stmt Fun.id m :> desugared_stmt_kind)
    (* Declarations vanish; only the inlined copies survive. *)
    | `Handler_decl _ -> `Block []
  in
  { it; span = sp; ann = () }

and clause span (c : stmt handler_clause) : desugared_stmt handler =
  match c with
  | Inline h -> map_handler stmt h
  | Named name ->
    (match Hashtbl.find_opt declared name with
     | Some h -> map_handler stmt h
     | None ->
       raise (Error { span; message = Printf.sprintf "Unknown handler '%s'." name }))

let program (p : program) : (desugared_stmt list, error) result =
  Hashtbl.reset declared;
  List.iter
    (fun (s : stmt) ->
      match s.it with
      | `Handler_decl (name, h) -> Hashtbl.replace declared name h
      | _ -> ())
    p;
  try Ok (List.map stmt p) with
  | Error e -> Result.Error e
