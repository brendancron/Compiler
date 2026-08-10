(* Stage 0 → stage 1: eliminate the [loops] layer. Only [`For] is mentioned;
   every other construct rides through on the generic traversals. *)

open Ast

let rec expr (e : expr) : dexpr =
  let sp = e.span in
  let it : dexpr_kind =
    match e.it with
    (* x op= v  ⇒  x = x op v *)
    | `Compound (op, name, v) ->
      `Assign (name, at sp (`Binop (op, at sp (`Var name), expr v)))
    | #lit as l -> l
    | #vars as v -> (map_vars expr v :> dexpr_kind)
    | #ops as o -> (map_ops expr o :> dexpr_kind)
    | #logic as l -> (map_logic expr l :> dexpr_kind)
  in
  { it; span = sp }

let rec stmt (s : stmt) : dstmt =
  let sp = s.span in
  let it : dstmt_kind =
    match s.it with
    | `For (init, cond, step, body) ->
      (* for (init; cond; step) body
         ⇒ { init; while (cond) { body; step; } } *)
      let cond =
        match cond with
        | Some c -> expr c
        | None -> at sp (`Bool true)
      in
      let body =
        match step with
        | None -> stmt body
        | Some st -> at sp (`Block [ stmt body; at sp (`Expr (expr st)) ])
      in
      let loop = at sp (`While (cond, body)) in
      `Block
        (match init with
         | Some i -> [ stmt i; loop ]
         | None -> [ loop ])
    | #stmts as s -> (map_stmts expr stmt s :> dstmt_kind)
  in
  { it; span = sp }

let program (p : program) : dstmt list = List.map stmt p
