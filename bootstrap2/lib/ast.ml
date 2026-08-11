type span =
  { line : int
  ; col : int
  }

(* ['ann] is [unit] until type checking, [Types.ty] after. The fragments below
   are parameterized over their child node type, so each tree costs a few lines
   rather than a re-declaration of every constructor. *)
type ('a, 'ann) node =
  { it : 'a
  ; span : span
  ; ann : 'ann
  }

let at span it = { it; span; ann = () }
let annotated span ann it = { it; span; ann }

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

(* What the parser records when an annotation is written. They are optional
   everywhere; inference fills in the rest. *)
type type_expr = (type_expr_kind, unit) node

and type_expr_kind =
  | Ty_name of string
  (* The row is the written one, so an omitted `<...>` means pure. *)
  | Ty_fn of type_expr list * type_expr * string list

type param =
  { name : string
  ; ty : type_expr option
  }

(* [row = None] leaves the effect row to inference; [Some labels] closes it. *)
type signature =
  { ret : type_expr option
  ; row : string list option
  }

type op_kind =
  | Op_fn (* resumes automatically with the arm's value *)
  | Op_ctl (* resumes only via `resume`, and may do so more than once *)

type op_decl =
  { op_name : string
  ; op_kind : op_kind
  ; op_params : param list
  ; op_ret : type_expr option
  }

type 's handler =
  { handled : string
  ; arms : 's arm list
  }

and 's arm =
  { arm_name : string
  ; arm_kind : op_kind
  ; arm_params : string list
  ; arm_body : 's list
  }

type lit =
  [ `Int of int
  | `Float of float
  | `Str of string
  | `Bool of bool
  ]

type 'e vars =
  [ `Var of string
  | `Assign of string * 'e
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

type 'e compound = [ `Compound of binop * string * 'e ]

(* Eliminated by the CPS pass, which is why the interpreter has no handler
   stack: by the time it runs, these are ordinary closures and calls. *)
type ('e, 's) effects =
  [ `Effect_decl of string * op_decl list
  | `Run of 's list * 's handler list
  | `Resume of 'e option
  ]

(* Eliminated by [Reflect], which needs the checker's annotations to do it. *)
type 'e reflect = [ `Typeof of 'e ]

type ('e, 's) stmts =
  [ `Expr of 'e
  | `Var_decl of string * type_expr option * 'e option
  | `Block of 's list
  | `If of 'e * 's * 's option
  | `While of 'e * 's
  | `Fn of string * param list * signature * 's list
  | `Return of 'e option
  ]

type ('e, 's) loops =
  [ `For of 's option * 'e option * 'e option * 's ]

type expr = (expr_kind, unit) node

and expr_kind =
  [ lit
  | expr vars
  | expr ops
  | expr logic
  | expr compound
  | expr reflect
  ]

type stmt = (stmt_kind, unit) node

and stmt_kind =
  [ (expr, stmt) stmts
  | (expr, stmt) loops
  | (expr, stmt) effects
  ]

type program = stmt list

(* No [`Compound], no [`For]. *)
type desugared_expr = (desugared_expr_kind, unit) node

and desugared_expr_kind =
  [ lit
  | desugared_expr vars
  | desugared_expr ops
  | desugared_expr logic
  | desugared_expr reflect
  ]

type desugared_stmt = (desugared_stmt_kind, unit) node

and desugared_stmt_kind =
  [ (desugared_expr, desugared_stmt) stmts
  | (desugared_expr, desugared_stmt) effects
  ]

(* Same constructors, every node carrying a resolved type. *)
type typed_expr = (typed_expr_kind, Types.ty) node

and typed_expr_kind =
  [ lit
  | typed_expr vars
  | typed_expr ops
  | typed_expr logic
  | typed_expr reflect
  ]

type typed_stmt = (typed_stmt_kind, Types.ty) node

and typed_stmt_kind =
  [ (typed_expr, typed_stmt) stmts
  | (typed_expr, typed_stmt) effects
  ]

(* No [`Typeof]: the interpreter cannot be handed one. *)
type reflected_expr = (reflected_expr_kind, Types.ty) node

and reflected_expr_kind =
  [ lit
  | reflected_expr vars
  | reflected_expr ops
  | reflected_expr logic
  ]

type reflected_stmt = (reflected_stmt_kind, Types.ty) node

and reflected_stmt_kind =
  [ (reflected_expr, reflected_stmt) stmts
  | (reflected_expr, reflected_stmt) effects
  ]

(* No effect constructs: the CPS pass has turned them into closures and calls. *)
type cps_expr = (cps_expr_kind, Types.ty) node

and cps_expr_kind =
  [ lit
  | cps_expr vars
  | cps_expr ops
  | cps_expr logic
  ]

type cps_stmt = (cps_stmt_kind, Types.ty) node
and cps_stmt_kind = (cps_expr, cps_stmt) stmts

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

let map_reflect (f : 'a -> 'b) (e : 'a reflect) : 'b reflect =
  match e with
  | `Typeof v -> `Typeof (f v)

let map_stmts (fe : 'e1 -> 'e2) (fs : 's1 -> 's2) (s : ('e1, 's1) stmts)
  : ('e2, 's2) stmts
  =
  match s with
  | `Expr e -> `Expr (fe e)
  | `Var_decl (name, ty, init) -> `Var_decl (name, ty, Option.map fe init)
  | `Block body -> `Block (List.map fs body)
  | `If (c, t, e) -> `If (fe c, fs t, Option.map fs e)
  | `While (c, body) -> `While (fe c, fs body)
  | `Fn (name, params, signature, body) ->
    `Fn (name, params, signature, List.map fs body)
  | `Return e -> `Return (Option.map fe e)

let map_loops (fe : 'e1 -> 'e2) (fs : 's1 -> 's2) (s : ('e1, 's1) loops)
  : ('e2, 's2) loops
  =
  match s with
  | `For (init, cond, step, body) ->
    `For (Option.map fs init, Option.map fe cond, Option.map fe step, fs body)

let map_effects (fe : 'e1 -> 'e2) (fs : 's1 -> 's2) (s : ('e1, 's1) effects)
  : ('e2, 's2) effects
  =
  let arm (a : 's1 arm) : 's2 arm =
    { arm_name = a.arm_name
    ; arm_kind = a.arm_kind
    ; arm_params = a.arm_params
    ; arm_body = List.map fs a.arm_body
    }
  in
  match s with
  | `Effect_decl (name, ops) -> `Effect_decl (name, ops)
  | `Run (body, handlers) ->
    `Run
      ( List.map fs body
      , List.map (fun h -> { handled = h.handled; arms = List.map arm h.arms }) handlers )
  | `Resume e -> `Resume (Option.map fe e)

let binop_of_token : Token.token_type -> binop option = function
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

let unop_of_token : Token.token_type -> unop option = function
  | Token.Minus -> Some Neg
  | Token.Bang -> Some Not
  | _ -> None

let span_of_token (t : Token.token) = { line = t.line; col = t.col }
