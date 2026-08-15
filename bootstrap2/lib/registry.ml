type emission =
  | Primitive
  | Call of string

(* Declared one at a time, so a type may be readable without being writable. *)
type indexing =
  { get : string option
  ; set : string option
  }

(* [scheme] is the entry's own signature, `(element) -> container`, so the
   element a target holds is recovered by unifying against the result rather
   than by assuming a container takes exactly one type argument. *)
type container =
  { entry : string
  ; scheme : Types.scheme
  }

type entry =
  { (* [None] is whatever the operands are. *)
    result : Types.ty option
  ; emit : emission
  }

type t =
  { (* The variadic constructor a literal of this type becomes. *)
    containers : (string, container) Hashtbl.t
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

let register_container t name entry = Hashtbl.replace t.containers name entry
let entry_for t name =
  match Hashtbl.find_opt t.indexed name with
  | Some entry -> entry
  | None -> { get = None; set = None }

let register_index_get t name fn =
  Hashtbl.replace t.indexed name { (entry_for t name) with get = Some fn }

let register_index_set t name fn =
  Hashtbl.replace t.indexed name { (entry_for t name) with set = Some fn }
let indexed t name = Hashtbl.find_opt t.indexed name
let register_constructor t name fn = Hashtbl.replace t.constructors name fn
let constructor t name = Hashtbl.find_opt t.constructors name
let container t name = Hashtbl.find_opt t.containers name

let container_element t name (target : Types.infer_ty) =
  match Hashtbl.find_opt t.containers name with
  | None -> None
  | Some c ->
    (match Types.repr (Types.instantiate c.scheme) with
     | Types.IFn ([ element ], result, _) ->
       (try
          Types.unify result target;
          Some element
        with
        | Types.Type_error _ -> None)
     | _ -> None)

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
  | Ast.Sub | Ast.Mul | Ast.Div | Ast.Mod -> Types.Numeric
  | Ast.Less | Ast.Less_equal | Ast.Greater | Ast.Greater_equal -> Types.Numeric
  | Ast.Equal | Ast.Not_equal -> Types.Any

let unresolved_result (op : Ast.binop) operand =
  match op with
  | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div | Ast.Mod -> operand
  | Ast.Less | Ast.Less_equal | Ast.Greater | Ast.Greater_equal | Ast.Equal
  | Ast.Not_equal -> Types.IBool

let builtins () =
  let t = create () in
  let prim result = { result = Some result; emit = Primitive } in
  let arithmetic = [ Ast.Add; Ast.Sub; Ast.Mul; Ast.Div; Ast.Mod ] in
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
      register t op Types.Float Types.Float (prim Types.Bool);
      (* A scalar value and an octet both have an order, and code that
         classifies characters is written with it. *)
      register t op Types.Chr Types.Chr (prim Types.Bool);
      register t op Types.Byte Types.Byte (prim Types.Bool))
    comparisons;
  List.iter
    (fun op -> register_homogeneous t op (prim Types.Bool))
    [ Ast.Equal; Ast.Not_equal ];
  t
