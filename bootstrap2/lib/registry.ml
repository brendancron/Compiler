type emission =
  | Primitive
  | Call of string

type indexing =
  { get : string
  ; set : string
  }

type entry =
  { (* [None] is whatever the operands are. *)
    result : Types.ty option
  ; emit : emission
  }

type t =
  { (* The variadic constructor a literal of this type becomes. *)
    containers : (string, string) Hashtbl.t
  ; indexed : (string, indexing) Hashtbl.t
  ; constructors : (string, string) Hashtbl.t
  ; exact : (Ast.binop * Types.ty * Types.ty, entry) Hashtbl.t
  ; (* Any two operands of the same type, which cannot be enumerated. *)
    homogeneous : (Ast.binop, entry) Hashtbl.t
  }

let create () =
  { containers = Hashtbl.create 8
  ; indexed = Hashtbl.create 8
  ; constructors = Hashtbl.create 8
  ; exact = Hashtbl.create 64
  ; homogeneous = Hashtbl.create 8
  }

let register_container t name emission = Hashtbl.replace t.containers name emission
let register_indexed t name emission = Hashtbl.replace t.indexed name emission
let indexed t name = Hashtbl.find_opt t.indexed name
let register_constructor t name fn = Hashtbl.replace t.constructors name fn
let constructor t name = Hashtbl.find_opt t.constructors name
let container t name = Hashtbl.find_opt t.containers name

let register t op lhs rhs entry = Hashtbl.replace t.exact (op, lhs, rhs) entry

let register_homogeneous t op entry = Hashtbl.replace t.homogeneous op entry

let find_exact t op lhs rhs = Hashtbl.find_opt t.exact (op, lhs, rhs)

let find t op lhs rhs =
  match Hashtbl.find_opt t.exact (op, lhs, rhs) with
  | Some entry -> Some entry
  | None -> if lhs = rhs then Hashtbl.find_opt t.homogeneous op else None

let result_of entry operand =
  match entry.result with
  | Some ty -> ty
  | None -> operand

let constraint_of (op : Ast.binop) =
  match op with
  | Ast.Add -> Types.Addable
  | Ast.Sub | Ast.Mul | Ast.Div -> Types.Numeric
  | Ast.Less | Ast.Less_equal | Ast.Greater | Ast.Greater_equal -> Types.Numeric
  | Ast.Equal | Ast.Not_equal -> Types.Any

let unresolved_result (op : Ast.binop) operand =
  match op with
  | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div -> operand
  | Ast.Less | Ast.Less_equal | Ast.Greater | Ast.Greater_equal | Ast.Equal
  | Ast.Not_equal -> Types.IBool

let builtins () =
  let t = create () in
  let prim result = { result = Some result; emit = Primitive } in
  let arithmetic = [ Ast.Add; Ast.Sub; Ast.Mul; Ast.Div ] in
  let comparisons =
    [ Ast.Less; Ast.Less_equal; Ast.Greater; Ast.Greater_equal ]
  in
  List.iter
    (fun op ->
      register t op Types.Int Types.Int (prim Types.Int);
      register t op Types.Float Types.Float (prim Types.Float))
    arithmetic;
  register t Ast.Add Types.Str Types.Str (prim Types.Str);
  List.iter
    (fun op ->
      register t op Types.Int Types.Int (prim Types.Bool);
      register t op Types.Float Types.Float (prim Types.Bool))
    comparisons;
  List.iter
    (fun op -> register_homogeneous t op (prim Types.Bool))
    [ Ast.Equal; Ast.Not_equal ];
  t
