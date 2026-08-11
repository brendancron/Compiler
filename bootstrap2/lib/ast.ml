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
  | Ty_app of string * type_expr list
  | Ty_tuple of type_expr list
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

(* Indexing is a primitive: it survives every pass, and [Resolve] only decides
   whether a given operand type keeps it or turns it into a call. *)
type 'e indexing =
  [ `Index of 'e * 'e
  | `Index_assign of 'e * 'e * 'e
  ]

(* Positional, fixed length, mixed types. Both forms are primitives. *)
type 'e tuple =
  [ `Tuple of 'e list
  | `Tuple_get of 'e * int
  ]

(* What the container is comes from context, so this cannot be lowered until
   the checker has run. [Resolve] turns it into an [`Array_lit] or a call. *)
type 'e collection = [ `Collection_lit of 'e list ]

(* The primitive the others are built from. *)
type 'e array_lit = [ `Array_lit of 'e list ]

(* Eliminated by the CPS pass, which is why the interpreter has no handler
   stack: by the time it runs, these are ordinary closures and calls. *)
(* A `run` may name a handler declared elsewhere. Both forms exist in the parsed
   tree; desugaring resolves the named ones, so later trees carry only
   [handler]. *)
type 's handler_clause =
  | Inline of 's handler
  | Named of string

type 's handler_defs = [ `Handler_decl of string * 's handler ]

type ('e, 's, 'h) effects =
  [ `Effect_decl of string * op_decl list
  | `Run of 's list * 'h list
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
  | expr indexing
  | expr tuple
  | expr collection
  | expr reflect
  ]

type stmt = (stmt_kind, unit) node

and stmt_kind =
  [ (expr, stmt) stmts
  | (expr, stmt) loops
  | (expr, stmt, stmt handler_clause) effects
  | stmt handler_defs
  ]

type program = stmt list

(* No [`For]. [`Compound] survives: whether `x += v` updates in place or
   rebuilds depends on the type, so only [Resolve] can decide it. *)
type desugared_expr = (desugared_expr_kind, unit) node

and desugared_expr_kind =
  [ lit
  | desugared_expr vars
  | desugared_expr ops
  | desugared_expr logic
  | desugared_expr compound
  | desugared_expr indexing
  | desugared_expr tuple
  | desugared_expr collection
  | desugared_expr reflect
  ]

type desugared_stmt = (desugared_stmt_kind, unit) node

and desugared_stmt_kind =
  [ (desugared_expr, desugared_stmt) stmts
  | (desugared_expr, desugared_stmt, desugared_stmt handler) effects
  ]

(* Same constructors, every node carrying a resolved type. *)
type typed_expr = (typed_expr_kind, Types.ty) node

and typed_expr_kind =
  [ lit
  | typed_expr vars
  | typed_expr ops
  | typed_expr logic
  | typed_expr compound
  | typed_expr indexing
  | typed_expr tuple
  | typed_expr collection
  | typed_expr reflect
  ]

type typed_stmt = (typed_stmt_kind, Types.ty) node

and typed_stmt_kind =
  [ (typed_expr, typed_stmt) stmts
  | (typed_expr, typed_stmt, typed_stmt handler) effects
  ]

(* No [`Compound]: every operator is now a primitive or a call. *)
type resolved_expr = (resolved_expr_kind, Types.ty) node

and resolved_expr_kind =
  [ lit
  | resolved_expr vars
  | resolved_expr ops
  | resolved_expr logic
  | resolved_expr indexing
  | resolved_expr tuple
  | resolved_expr array_lit
  | resolved_expr reflect
  ]

type resolved_stmt = (resolved_stmt_kind, Types.ty) node

and resolved_stmt_kind =
  [ (resolved_expr, resolved_stmt) stmts
  | (resolved_expr, resolved_stmt, resolved_stmt handler) effects
  ]

(* No [`Typeof]: the interpreter cannot be handed one. *)
type reflected_expr = (reflected_expr_kind, Types.ty) node

and reflected_expr_kind =
  [ lit
  | reflected_expr vars
  | reflected_expr ops
  | reflected_expr logic
  | reflected_expr indexing
  | reflected_expr tuple
  | reflected_expr array_lit
  ]

type reflected_stmt = (reflected_stmt_kind, Types.ty) node

and reflected_stmt_kind =
  [ (reflected_expr, reflected_stmt) stmts
  | (reflected_expr, reflected_stmt, reflected_stmt handler) effects
  ]

(* No effect constructs: the CPS pass has turned them into closures and calls. *)
type cps_expr = (cps_expr_kind, Types.ty) node

and cps_expr_kind =
  [ lit
  | cps_expr vars
  | cps_expr ops
  | cps_expr logic
  | cps_expr indexing
  | cps_expr tuple
  | cps_expr array_lit
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

let map_indexing (f : 'a -> 'b) (e : 'a indexing) : 'b indexing =
  match e with
  | `Index (target, i) -> `Index (f target, f i)
  | `Index_assign (target, i, v) -> `Index_assign (f target, f i, f v)

let map_tuple (f : 'a -> 'b) (e : 'a tuple) : 'b tuple =
  match e with
  | `Tuple items -> `Tuple (List.map f items)
  | `Tuple_get (t, i) -> `Tuple_get (f t, i)

let map_collection (f : 'a -> 'b) (e : 'a collection) : 'b collection =
  match e with
  | `Collection_lit items -> `Collection_lit (List.map f items)

let map_array_lit (f : 'a -> 'b) (e : 'a array_lit) : 'b array_lit =
  match e with
  | `Array_lit items -> `Array_lit (List.map f items)

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

let map_arm (fs : 's1 -> 's2) (a : 's1 arm) : 's2 arm =
  { arm_name = a.arm_name
  ; arm_kind = a.arm_kind
  ; arm_params = a.arm_params
  ; arm_body = List.map fs a.arm_body
  }

let map_handler (fs : 's1 -> 's2) (h : 's1 handler) : 's2 handler =
  { handled = h.handled; arms = List.map (map_arm fs) h.arms }

let map_effects
      (fe : 'e1 -> 'e2)
      (fs : 's1 -> 's2)
      (fh : 'h1 -> 'h2)
      (s : ('e1, 's1, 'h1) effects)
  : ('e2, 's2, 'h2) effects
  =
  match s with
  | `Effect_decl (name, ops) -> `Effect_decl (name, ops)
  | `Run (body, handlers) -> `Run (List.map fs body, List.map fh handlers)
  | `Resume e -> `Resume (Option.map fe e)

let string_of_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Not_equal -> "!="
  | Less -> "<"
  | Less_equal -> "<="
  | Greater -> ">"
  | Greater_equal -> ">="

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
