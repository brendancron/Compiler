type span =
  { line : int
  ; col : int
  }

type 'a node =
  { it : 'a
  ; span : span
  }

let at span it = { it; span }

type unop =
  | Neg (* - *)
  | Not (* ! *)

type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Equal
  | Not_equal
  | Less
  | Less_equal
  | Greater
  | Greater_equal

type lit =
  [ `Num of float
  | `Str of string
  | `Bool of bool
  ]

type 'e vars =
  [ `Var of string
  | `Assign of string * 'e
  ]

type 'e slots =
  [ `Slot of int
  | `Assign_slot of int * 'e
  ]

type 'e ops =
  [ `Unop of unop * 'e
  | `Binop of binop * 'e * 'e
  | `Call of 'e * 'e list
  ]

type 'e logic =
  [ `And of 'e * 'e
  | `Or of 'e * 'e
  ]

(* `x += e`, `x -= e`, `x++`, `x--` — all four are one shape: read a variable,
   apply a binop, store it back. Desugaring rewrites them to [`Assign]. *)
type 'e compound = [ `Compound of binop * string * 'e ]

type ('e, 's) stmts =
  [ `Expr of 'e
  | `Var_decl of string * 'e option
  | `Block of 's list
  | `If of 'e * 's * 's option
  | `While of 'e * 's
  | `Fn of string * string list * 's list
  | `Return of 'e option
  ]

type ('e, 's) loops =
  [ `For of 's option * 'e option * 'e option * 's ]

type expr = expr_kind node

and expr_kind =
  [ lit
  | expr vars
  | expr ops
  | expr logic
  | expr compound
  ]

type stmt = stmt_kind node
and stmt_kind = [ (expr, stmt) stmts | (expr, stmt) loops ]

type program = stmt list

type dexpr = dexpr_kind node

and dexpr_kind =
  [ lit
  | dexpr vars
  | dexpr ops
  | dexpr logic
  ]

type dstmt = dstmt_kind node
and dstmt_kind = (dexpr, dstmt) stmts

let map_vars (f : 'a -> 'b) (e : 'a vars) : 'b vars =
  match e with
  | `Var name -> `Var name
  | `Assign (name, v) -> `Assign (name, f v)

let map_ops (f : 'a -> 'b) (e : 'a ops) : 'b ops =
  match e with
  | `Unop (op, a) -> `Unop (op, f a)
  | `Binop (op, a, b) -> `Binop (op, f a, f b)
  | `Call (callee, args) -> `Call (f callee, List.map f args)

let map_logic (f : 'a -> 'b) (e : 'a logic) : 'b logic =
  match e with
  | `And (a, b) -> `And (f a, f b)
  | `Or (a, b) -> `Or (f a, f b)

let map_compound (f : 'a -> 'b) (e : 'a compound) : 'b compound =
  match e with
  | `Compound (op, name, v) -> `Compound (op, name, f v)

let map_stmts (fe : 'e1 -> 'e2) (fs : 's1 -> 's2) (s : ('e1, 's1) stmts)
  : ('e2, 's2) stmts
  =
  match s with
  | `Expr e -> `Expr (fe e)
  | `Var_decl (name, init) -> `Var_decl (name, Option.map fe init)
  | `Block body -> `Block (List.map fs body)
  | `If (c, t, e) -> `If (fe c, fs t, Option.map fs e)
  | `While (c, body) -> `While (fe c, fs body)
  | `Fn (name, params, body) -> `Fn (name, params, List.map fs body)
  | `Return e -> `Return (Option.map fe e)

let map_loops (fe : 'e1 -> 'e2) (fs : 's1 -> 's2) (s : ('e1, 's1) loops)
  : ('e2, 's2) loops
  =
  match s with
  | `For (init, cond, step, body) ->
    `For (Option.map fs init, Option.map fe cond, Option.map fe step, fs body)

let binop_of_token : Token.kind -> binop option = function
  | Token.Plus -> Some Add
  | Token.Minus -> Some Sub
  | Token.Star -> Some Mul
  | Token.Slash -> Some Div
  | Token.Equal_equal -> Some Equal
  | Token.Bang_equal -> Some Not_equal
  | Token.Less -> Some Less
  | Token.Less_equal -> Some Less_equal
  | Token.Greater -> Some Greater
  | Token.Greater_equal -> Some Greater_equal
  | _ -> None

let unop_of_token : Token.kind -> unop option = function
  | Token.Minus -> Some Neg
  | Token.Bang -> Some Not
  | _ -> None

let span_of_token (t : Token.t) = { line = t.line; col = t.col }
