open Value

exception Return_value of value * Ast.span

let as_bool span = function
  | Bool b -> b
  | v -> fail span "Expected a bool condition, got %s." (type_name v)

(* Mixed operands cannot reach here: there is no implicit widening. *)
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

let rec eval env (e : Ast.cps_expr) : value =
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
  | `Tuple items -> Tuple (List.map (eval env) items)
  | `Record_lit fields ->
    Record (List.map (fun (l, v) -> l, ref (eval env v)) fields)
  | `Variant (name, fields) ->
    Variant (name, List.map (fun (l, v) -> l, eval env v) fields)
  | `Field (target, label) ->
    (match eval env target with
     | Record fields ->
       (match List.assoc_opt label fields with
        | Some v -> !v
        | None -> fail span "No field '%s'." label)
     | v -> fail span "Cannot take a field of %s." (type_name v))
  | `Field_assign (target, label, v) ->
    let value = eval env v in
    (match eval env target with
     | Record fields ->
       (match List.assoc_opt label fields with
        | Some cell ->
          cell := value;
          value
        | None -> fail span "No field '%s'." label)
     | v -> fail span "Cannot take a field of %s." (type_name v))
  | `Tuple_get (target, index) ->
    (match eval env target with
     | Tuple items -> List.nth items index
     | v -> fail span "Cannot take a field of %s." (type_name v))
and call span f args =
  match f with
  | Fn f ->
    (match f.arity with
     | Some n when n <> List.length args ->
       fail span "%s expects %d argument(s) but got %d." f.name n (List.length args)
     | _ -> ());
    f.apply span args
  | v -> fail span "Cannot call %s." (type_name v)

and exec env (s : Ast.cps_stmt) : unit =
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
    let names = List.map (fun (p : Ast.param) -> p.Ast.name) params in
    define
      env
      name
      (Fn
         { name
         ; arity = Some (List.length names)
         ; apply =
             (fun _ args ->
               let frame = new_env (Some env) in
               List.iter2 (define frame) names args;
               (try
                  List.iter (exec frame) body;
                  Unit
                with
                | Return_value (v, _) -> v))
         })
  | `Return e ->
    let v =
      match e with
      | Some x -> eval env x
      | None -> Unit
    in
    raise (Return_value (v, span))
  | `Match (scrutinee, cases) ->
    let value = eval env scrutinee in
    let bind_case (pattern : Ast.pattern) =
      match pattern, value with
      | Ast.Pat_wild, _ -> Some []
      | Ast.Pat_variant (_, name, payload), Variant (tag, fields)
        when String.equal name tag ->
        Some
          (List.map
             (fun (label, binding) -> binding, List.assoc label fields)
             (Ast.payload_fields payload))
      | _ -> None
    in
    let rec first = function
      | [] -> fail span "No case matched."
      | (pattern, body) :: rest ->
        (match bind_case pattern with
         | None -> first rest
         | Some bindings ->
           let scope = new_env (Some env) in
           List.iter (fun (name, v) -> define scope name v) bindings;
           List.iter (exec scope) body)
    in
    first cases

let run env (program : Ast.cps_stmt list) : (unit, error) result =
  try
    List.iter (exec env) program;
    Ok ()
  with
  | Runtime_error e -> Error e
  | Return_value (_, span) -> Error { span; message = "'return' outside of a function." }
