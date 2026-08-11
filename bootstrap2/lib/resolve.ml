(* Turns every construct whose meaning depends on a type into a concrete
   primitive or call, using the types the checker inferred.

   Today that is compound assignment. `x += v` becomes `x = x + v` unless the
   type supplies an in-place entry, and no type does yet. Operators, indexing,
   and collection literals join it here as the registry grows. *)

let rec expr (e : Ast.typed_expr) : Ast.resolved_expr =
  let span = e.Ast.span
  and ann = e.Ast.ann in
  let it : Ast.resolved_expr_kind =
    match e.Ast.it with
    (* No in-place entry exists, so it derives from the plain operator. The
       target's type is the node's own, which is what the rebuilt read of it
       carries. *)
    | `Compound (op, name, v) ->
      let target : Ast.resolved_expr = { Ast.it = `Var name; span; ann } in
      let combined : Ast.resolved_expr =
        { Ast.it = `Binop (op, target, expr v); span; ann }
      in
      `Assign (name, combined)
    | #Ast.lit as l -> l
    | #Ast.vars as v -> (Ast.map_vars expr v :> Ast.resolved_expr_kind)
    | #Ast.ops as o -> (Ast.map_ops expr o :> Ast.resolved_expr_kind)
    | #Ast.logic as l -> (Ast.map_logic expr l :> Ast.resolved_expr_kind)
    | #Ast.reflect as r -> (Ast.map_reflect expr r :> Ast.resolved_expr_kind)
  in
  { Ast.it; span; ann }

let rec stmt (s : Ast.typed_stmt) : Ast.resolved_stmt =
  let it : Ast.resolved_stmt_kind =
    match s.Ast.it with
    | #Ast.stmts as st -> (Ast.map_stmts expr stmt st :> Ast.resolved_stmt_kind)
    | #Ast.effects as e ->
      (Ast.map_effects expr stmt (Ast.map_handler stmt) e :> Ast.resolved_stmt_kind)
  in
  { Ast.it; span = s.Ast.span; ann = s.Ast.ann }

let program (p : Ast.typed_stmt list) : Ast.resolved_stmt list = List.map stmt p
