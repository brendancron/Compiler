open Ast

let rec expr (e : expr) : desugared_expr =
  let sp = e.span in
  let it : desugared_expr_kind =
    match e.it with
    (* x op= v  ⇒  x = x op v.  [sp] is the span of `x` itself, since the parser
       gives an assignment its target's span. *)
    | `Compound (op, name, v) ->
      `Assign (name, at sp (`Binop (op, at sp (`Var name), expr v)))
    | #lit as l -> l
    | #vars as v -> (map_vars expr v :> desugared_expr_kind)
    | #ops as o -> (map_ops expr o :> desugared_expr_kind)
    | #logic as l -> (map_logic expr l :> desugared_expr_kind)
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
  in
  { it; span = sp; ann = () }

let program (p : program) : desugared_stmt list = List.map stmt p
