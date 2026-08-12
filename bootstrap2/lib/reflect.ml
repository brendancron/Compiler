(* Replaces `typeof(e)` with the string form of e's type. Runs after checking
   because that is when the annotation exists, and before evaluation so the
   interpreter never sees the node. *)

let rec expr (e : Ast.resolved_expr) : Ast.reflected_expr =
  let it : Ast.reflected_expr_kind =
    match e.Ast.it with
    | `Typeof inner -> `Str (Types.string_of_ty inner.Ast.ann)
    | #Ast.lit as l -> l
    | #Ast.vars as v -> (Ast.map_vars expr v :> Ast.reflected_expr_kind)
    | #Ast.ops as o -> (Ast.map_ops expr o :> Ast.reflected_expr_kind)
    | #Ast.logic as l -> (Ast.map_logic expr l :> Ast.reflected_expr_kind)
    | #Ast.indexing as i -> (Ast.map_indexing expr i :> Ast.reflected_expr_kind)
    | #Ast.tuple as t -> (Ast.map_tuple expr t :> Ast.reflected_expr_kind)
    | #Ast.record as r -> (Ast.map_record expr r :> Ast.reflected_expr_kind)
    | #Ast.array_lit as a -> (Ast.map_array_lit expr a :> Ast.reflected_expr_kind)
  in
  { Ast.it; span = e.Ast.span; ann = e.Ast.ann }

let rec stmt (s : Ast.resolved_stmt) : Ast.reflected_stmt =
  let it : Ast.reflected_stmt_kind =
    match s.Ast.it with
    | #Ast.stmts as st -> (Ast.map_stmts expr stmt st :> Ast.reflected_stmt_kind)
    | #Ast.effects as e -> (Ast.map_effects expr stmt (Ast.map_handler stmt) e :> Ast.reflected_stmt_kind)
    | #Ast.type_defs as t -> t
  in
  { Ast.it; span = s.Ast.span; ann = s.Ast.ann }

let program (p : Ast.resolved_stmt list) : Ast.reflected_stmt list = List.map stmt p
