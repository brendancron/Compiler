(* One table, two readers: the checker asks for a result type, [Resolve] asks
   what to emit. Two matches that have to agree eventually will not. *)

type emission =
  (* Why `int + int` costs nothing: selection happened here, not at run time. *)
  | Primitive
  | Call of string

type entry =
  { (* [None] means the result is whatever the operands are. *)
    result : Types.ty option
  ; emit : emission
  }

type t =
  { (* What a collection literal builds, by the name of the type it builds. A
       container is a candidate for `[…]` exactly by being in here. *)
    containers : (string, emission) Hashtbl.t
  ; exact : (Ast.binop * Types.ty * Types.ty, entry) Hashtbl.t
  ; (* Operators defined for any two operands of the same type, which cannot be
       enumerated — equality over every type there will ever be. *)
    homogeneous : (Ast.binop, entry) Hashtbl.t
  }

let create () =
  { containers = Hashtbl.create 8
  ; exact = Hashtbl.create 64
  ; homogeneous = Hashtbl.create 8
  }

let register_container t name emission = Hashtbl.replace t.containers name emission
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
  (* Array is the one the others are built from, so it is the one that stands as
     written — and the one an unnarrowed literal defaults to. *)
  register_container t "Array" Primitive;
  register_container t "List" (Call "List__of");
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
