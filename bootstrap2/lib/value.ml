type value =
  | Int of int
  | Float of float
  | Str of string
  | Bool of bool
  | Unit
  | Tuple of value list
  (* A contiguous mutable block. Shared rather than copied, so it has
     identity. *)
  | Array of value array
  (* Fields are mutable, so a record has identity. *)
  | Record of (string * value ref) list
  | Variant of string * (string * value) list
  (* One callable form. Whether the body is Cronyx or OCaml is the business of
     whoever built it. *)
  | Fn of fn

and fn =
  { name : string
  ; arity : int option (* [None] is variadic *)
  ; apply : Ast.span -> value list -> value
  }

and env =
  { vars : (string, value ref) Hashtbl.t
  ; parent : env option
  }

type error =
  { span : Ast.span
  ; message : string
  }

exception Runtime_error of error

let fail span fmt =
  Printf.ksprintf (fun message -> raise (Runtime_error { span; message })) fmt

let new_env parent = { vars = Hashtbl.create 16; parent }

let rec lookup env name =
  match Hashtbl.find_opt env.vars name with
  | Some cell -> Some cell
  | None -> Option.bind env.parent (fun parent -> lookup parent name)

let define env name v = Hashtbl.replace env.vars name (ref v)

let type_name = function
  | Tuple _ -> "tuple"
  | Record _ -> "record"
  | Variant _ -> "variant"
  | Int _ -> "int"
  | Float _ -> "float"
  | Str _ -> "string"
  | Bool _ -> "bool"
  | Unit -> "unit"
  | Array _ -> "array"
  | Fn _ -> "fn"

let rec string_of_value = function
  | Array items ->
    "[" ^ String.concat ", " (Array.to_list (Array.map string_of_value items)) ^ "]"
  | Tuple items -> "(" ^ String.concat ", " (List.map string_of_value items) ^ ")"
  | Variant (name, []) -> name
  | Variant (name, fields) ->
    name ^ "(" ^ String.concat ", " (List.map (fun (_, v) -> string_of_value v) fields) ^ ")"
  | Record fields ->
    "{ "
    ^ String.concat ", " (List.map (fun (l, v) -> l ^ ": " ^ string_of_value !v) fields)
    ^ " }"
  | Int n -> string_of_int n
  | Float n -> Token.float_to_string n
  | Str s -> s
  | Bool b -> string_of_bool b
  | Unit -> "unit"
  | Fn f -> Printf.sprintf "<fn %s>" f.name

(* OCaml's structural comparison raises on functional values. *)
let rec values_equal a b =
  match a, b with
  | Array x, Array y -> x == y
  | Tuple x, Tuple y -> List.length x = List.length y && List.for_all2 values_equal x y
  | Record x, Record y -> x == y
  | Variant (n, a), Variant (m, b) ->
    String.equal n m
    && List.length a = List.length b
    && List.for_all2 (fun (_, x) (_, y) -> values_equal x y) a b
  | Int x, Int y -> x = y
  | Float x, Float y -> Float.equal x y
  | Str x, Str y -> String.equal x y
  | Bool x, Bool y -> x = y
  | Unit, Unit -> true
  | _ -> false
