type value =
  | Int of int
  | Float of float
  | Str of string
  | Bool of bool
  | Unit
  | Closure of
      { params : string list
      ; body : Ast.reflected_stmt list
      ; env : env
      }
  | Native of string * int option * (value list -> value) (* None arity = variadic *)

and env =
  { vars : (string, value ref) Hashtbl.t
  ; parent : env option
  }

type error =
  { span : Ast.span
  ; message : string
  }

exception Runtime_error of error
exception Return_value of value * Ast.span

let fail span fmt =
  Printf.ksprintf (fun message -> raise (Runtime_error { span; message })) fmt

let new_env parent = { vars = Hashtbl.create 8; parent }
let define env name v = Hashtbl.replace env.vars name (ref v)

let rec lookup env name =
  match Hashtbl.find_opt env.vars name with
  | Some r -> Some r
  | None ->
    (match env.parent with
     | Some p -> lookup p name
     | None -> None)

let type_name = function
  | Int _ -> "int"
  | Float _ -> "float"
  | Str _ -> "string"
  | Bool _ -> "bool"
  | Unit -> "unit"
  | Closure _ | Native _ -> "fn"

let string_of_value = function
  | Int n -> string_of_int n
  | Float n -> Token.float_to_string n
  | Str s -> s
  | Bool b -> string_of_bool b
  | Unit -> "()"
  | Closure c -> Printf.sprintf "<fn/%d>" (List.length c.params)
  | Native (name, _, _) -> Printf.sprintf "<native %s>" name

(* Conditions must be bool: there is no truthiness in Cronyx. *)
let as_bool span = function
  | Bool b -> b
  | v -> fail span "Expected a bool condition, got %s." (type_name v)

(* Closures are never compared, so this avoids OCaml's structural comparison
   raising on functional values. *)
let values_equal a b =
  match a, b with
  | Int x, Int y -> x = y
  | Float x, Float y -> x = y
  | Str x, Str y -> String.equal x y
  | Bool x, Bool y -> x = y
  | Unit, Unit -> true
  | _ -> false

(* Mixed operands cannot reach here: the type checker rejects `int + float` and
   there is no implicit widening. *)
let eval_binop span (op : Ast.binop) a b =
  match op, a, b with
  | Ast.Add, Int x, Int y -> Int (x + y)
  | Ast.Add, Float x, Float y -> Float (x +. y)
  | Ast.Add, Str x, Str y -> Str (x ^ y)
  | Ast.Sub, Int x, Int y -> Int (x - y)
  | Ast.Sub, Float x, Float y -> Float (x -. y)
  | Ast.Mul, Int x, Int y -> Int (x * y)
  | Ast.Mul, Float x, Float y -> Float (x *. y)
  | Ast.Div, Int _, Int 0 -> fail span "Division by zero."
  | Ast.Div, Int x, Int y -> Int (x / y)
  | Ast.Div, Float x, Float y -> Float (x /. y)
  | Ast.Less, Int x, Int y -> Bool (x < y)
  | Ast.Less, Float x, Float y -> Bool (x < y)
  | Ast.Less_equal, Int x, Int y -> Bool (x <= y)
  | Ast.Less_equal, Float x, Float y -> Bool (x <= y)
  | Ast.Greater, Int x, Int y -> Bool (x > y)
  | Ast.Greater, Float x, Float y -> Bool (x > y)
  | Ast.Greater_equal, Int x, Int y -> Bool (x >= y)
  | Ast.Greater_equal, Float x, Float y -> Bool (x >= y)
  | Ast.Equal, _, _ -> Bool (values_equal a b)
  | Ast.Not_equal, _, _ -> Bool (not (values_equal a b))
  | _ ->
    fail
      span
      "Operator is not defined for %s and %s."
      (type_name a)
      (type_name b)

let rec eval env (e : Ast.reflected_expr) : value =
  let span = e.Ast.span in
  match e.Ast.it with
  | `Int n -> Int n
  | `Float n -> Float n
  | `Str s -> Str s
  | `Bool b -> Bool b
  | `Var name ->
    (match lookup env name with
     | Some r -> !r
     | None -> fail span "Undefined variable '%s'." name)
  | `Assign (name, v) ->
    let value = eval env v in
    (match lookup env name with
     | Some r ->
       r := value;
       value
     | None -> fail span "Undefined variable '%s'." name)
  | `Unop (Ast.Neg, a) ->
    (match eval env a with
     | Int n -> Int (-n)
     | Float n -> Float (-.n)
     | v -> fail span "Cannot negate %s." (type_name v))
  | `Unop (Ast.Not, a) -> Bool (not (as_bool span (eval env a)))
  | `Binop (op, a, b) -> eval_binop span op (eval env a) (eval env b)
  | `And (a, b) ->
    if as_bool span (eval env a) then Bool (as_bool span (eval env b)) else Bool false
  | `Or (a, b) ->
    if as_bool span (eval env a) then Bool true else Bool (as_bool span (eval env b))
  | `Call (callee, args) ->
    let f = eval env callee in
    call span f (List.map (eval env) args)

and call span f args =
  match f with
  | Closure c ->
    let expected = List.length c.params
    and got = List.length args in
    if expected <> got
    then fail span "Expected %d argument(s) but got %d." expected got;
    let frame = new_env (Some c.env) in
    List.iter2 (define frame) c.params args;
    (try
       List.iter (exec frame) c.body;
       Unit
     with
     | Return_value (v, _) -> v)
  | Native (name, arity, fn) ->
    (match arity with
     | Some n when n <> List.length args ->
       fail span "%s expects %d argument(s) but got %d." name n (List.length args)
     | _ -> ());
    fn args
  | v -> fail span "Cannot call %s." (type_name v)

and exec env (s : Ast.reflected_stmt) : unit =
  let span = s.Ast.span in
  match s.Ast.it with
  | `Expr e -> ignore (eval env e)
  | `Var_decl (name, _, init) ->
    let v =
      match init with
      | Some e -> eval env e
      | None -> Unit
    in
    define env name v
  | `Block body ->
    let scope = new_env (Some env) in
    List.iter (exec scope) body
  | `If (cond, then_branch, else_branch) ->
    if as_bool span (eval env cond)
    then exec env then_branch
    else (
      match else_branch with
      | Some st -> exec env st
      | None -> ())
  | `While (cond, body) ->
    while as_bool span (eval env cond) do
      exec env body
    done
  (* The closure captures the env it is declared in, which is the same table the
     name lands in — so recursion works without a separate binding step. *)
  | `Fn (name, params, _, body) ->
    define env name (Closure { params = List.map (fun p -> p.Ast.name) params; body; env })
  | `Return e ->
    let v =
      match e with
      | Some x -> eval env x
      | None -> Unit
    in
    raise (Return_value (v, span))

let globals out =
  let env = new_env None in
  define
    env
    "print"
    (Native
       ( "print"
       , None
       , fun args ->
           out (String.concat " " (List.map string_of_value args));
           out "\n";
           Unit ));
  define
    env
    "str"
    (Native
       ( "str"
       , Some 1
       , function
         | [ v ] -> Str (string_of_value v)
         | _ -> Unit ));
  define env "clock" (Native ("clock", Some 0, fun _ -> Float (Sys.time ())));
  env

(* [out] is where `print` writes; tests capture it into a buffer. *)
let run ?(out = print_string) (program : Ast.reflected_stmt list) : (unit, error) result =
  let env = globals out in
  try
    List.iter (exec env) program;
    Ok ()
  with
  | Runtime_error e -> Error e
  | Return_value (_, span) -> Error { span; message = "'return' outside of a function." }
