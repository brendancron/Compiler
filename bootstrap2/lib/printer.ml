(* S-expression dump of the parsed tree. *)

let string_of_binop = Ast.string_of_binop

let string_of_unop : Ast.unop -> string = function
  | Ast.Neg -> "-"
  | Ast.Not -> "!"

let string_of_row = function
  | [] -> ""
  | labels -> Printf.sprintf " <%s>" (String.concat ", " labels)

let rec string_of_type_expr (t : Ast.type_expr) : string =
  match t.Ast.it with
  | Ast.Ty_name name -> name
  | Ast.Ty_app (name, args) ->
    Printf.sprintf "%s<%s>" name (String.concat ", " (List.map string_of_type_expr args))
  | Ast.Ty_fn (params, ret, row) ->
    Printf.sprintf
      "(%s) -> %s%s"
      (String.concat ", " (List.map string_of_type_expr params))
      (string_of_type_expr ret)
      (string_of_row row)

let annotation = function
  | None -> ""
  | Some t -> ": " ^ string_of_type_expr t

let string_of_param (p : Ast.param) = p.Ast.name ^ annotation p.Ast.ty

let rec string_of_expr (e : Ast.expr) : string =
  match e.Ast.it with
  | `Int n -> string_of_int n
  | `Float n -> Token.float_to_string n
  | `Str s -> Printf.sprintf "%S" s
  | `Bool b -> string_of_bool b
  | `Var name -> name
  | `Assign (name, v) -> Printf.sprintf "(set %s %s)" name (string_of_expr v)
  | `Compound (op, name, v) ->
    Printf.sprintf "(%s= %s %s)" (string_of_binop op) name (string_of_expr v)
  | `Unop (op, a) -> Printf.sprintf "(%s %s)" (string_of_unop op) (string_of_expr a)
  | `Binop (op, a, b) ->
    Printf.sprintf
      "(%s %s %s)"
      (string_of_binop op)
      (string_of_expr a)
      (string_of_expr b)
  | `Call (callee, args) ->
    Printf.sprintf
      "(call %s%s)"
      (string_of_expr callee)
      (String.concat "" (List.map (fun a -> " " ^ string_of_expr a) args))
  | `And (a, b) -> Printf.sprintf "(and %s %s)" (string_of_expr a) (string_of_expr b)
  | `Or (a, b) -> Printf.sprintf "(or %s %s)" (string_of_expr a) (string_of_expr b)
  | `Typeof e -> Printf.sprintf "(typeof %s)" (string_of_expr e)
  | `Collection_lit items ->
    Printf.sprintf "[%s]" (String.concat " " (List.map string_of_expr items))
  | `Index (t, i) -> Printf.sprintf "(index %s %s)" (string_of_expr t) (string_of_expr i)
  | `Index_assign (t, i, v) ->
    Printf.sprintf
      "(index-set %s %s %s)"
      (string_of_expr t)
      (string_of_expr i)
      (string_of_expr v)

let opt_expr = function
  | None -> "_"
  | Some e -> string_of_expr e

let rec write_stmt buf indent (s : Ast.stmt) =
  let line fmt = Printf.ksprintf (fun str -> Buffer.add_string buf str) fmt in
  let pad = String.make (indent * 2) ' ' in
  let nested label body =
    line "%s(%s\n" pad label;
    List.iter (write_stmt buf (indent + 1)) body;
    line "%s)\n" pad
  in
  match s.Ast.it with
  | `Expr e -> line "%s%s\n" pad (string_of_expr e)
  | `Var_decl (name, ty, init) ->
    line "%s(var %s%s %s)\n" pad name (annotation ty) (opt_expr init)
  | `Block body -> nested "block" body
  | `If (cond, then_branch, else_branch) ->
    nested
      (Printf.sprintf "if %s" (string_of_expr cond))
      (then_branch :: Option.to_list else_branch)
  | `While (cond, body) -> nested (Printf.sprintf "while %s" (string_of_expr cond)) [ body ]
  | `Fn (name, params, signature, body) ->
    nested
      (Printf.sprintf
         "fn %s (%s)%s%s"
         name
         (String.concat " " (List.map string_of_param params))
         (match signature.Ast.ret with
          | None -> ""
          | Some t -> " -> " ^ string_of_type_expr t)
         (match signature.Ast.row with
          | None -> ""
          | Some labels -> string_of_row labels))
      body
  | `Return value -> line "%s(return %s)\n" pad (opt_expr value)
  | `For (init, cond, step, body) ->
    line "%s(for %s %s\n" pad (opt_expr cond) (opt_expr step);
    List.iter (write_stmt buf (indent + 1)) (Option.to_list init @ [ body ]);
    line "%s)\n" pad
  | `Effect_decl (name, ops) ->
    line
      "%s(effect %s%s)\n"
      pad
      name
      (String.concat "" (List.map (fun (o : Ast.op_decl) -> " " ^ o.Ast.op_name) ops))
  | `Run (body, handlers) ->
    let label (c : Ast.stmt Ast.handler_clause) =
      match c with
      | Ast.Inline h -> " handle " ^ h.Ast.handled
      | Ast.Named name -> " with " ^ name
    in
    let arms (c : Ast.stmt Ast.handler_clause) =
      match c with
      | Ast.Inline h -> List.concat_map (fun a -> a.Ast.arm_body) h.Ast.arms
      | Ast.Named _ -> []
    in
    nested
      (Printf.sprintf "run%s" (String.concat "" (List.map label handlers)))
      (body @ List.concat_map arms handlers)
  | `Resume value -> line "%s(resume %s)\n" pad (opt_expr value)
  | `Handler_decl (name, h) ->
    nested
      (Printf.sprintf "handler %s : %s" name h.Ast.handled)
      (List.concat_map (fun a -> a.Ast.arm_body) h.Ast.arms)

let string_of_program (program : Ast.program) : string =
  let buf = Buffer.create 256 in
  List.iter (write_stmt buf 0) program;
  Buffer.contents buf

(* ---- typed dump ---- *)

let rec string_of_typed_expr (e : Ast.typed_expr) : string =
  let ty = Types.string_of_ty e.Ast.ann in
  let body =
    match e.Ast.it with
    | `Int n -> string_of_int n
    | `Float n -> Token.float_to_string n
    | `Str s -> Printf.sprintf "%S" s
    | `Bool b -> string_of_bool b
    | `Var name -> name
    | `Assign (name, v) -> Printf.sprintf "(set %s %s)" name (string_of_typed_expr v)
    | `Unop (op, a) ->
      Printf.sprintf "(%s %s)" (string_of_unop op) (string_of_typed_expr a)
    | `Binop (op, a, b) ->
      Printf.sprintf
        "(%s %s %s)"
        (string_of_binop op)
        (string_of_typed_expr a)
        (string_of_typed_expr b)
    | `Call (callee, args) ->
      Printf.sprintf
        "(call %s%s)"
        (string_of_typed_expr callee)
        (String.concat "" (List.map (fun a -> " " ^ string_of_typed_expr a) args))
    | `And (a, b) ->
      Printf.sprintf "(and %s %s)" (string_of_typed_expr a) (string_of_typed_expr b)
    | `Or (a, b) ->
      Printf.sprintf "(or %s %s)" (string_of_typed_expr a) (string_of_typed_expr b)
    | `Compound (op, name, v) ->
      Printf.sprintf "(%s= %s %s)" (string_of_binop op) name (string_of_typed_expr v)
    | `Typeof e -> Printf.sprintf "(typeof %s)" (string_of_typed_expr e)
    | `Collection_lit items ->
      Printf.sprintf "[%s]" (String.concat " " (List.map string_of_typed_expr items))
    | `Index (t, i) ->
      Printf.sprintf "(index %s %s)" (string_of_typed_expr t) (string_of_typed_expr i)
    | `Index_assign (t, i, v) ->
      Printf.sprintf
        "(index-set %s %s %s)"
        (string_of_typed_expr t)
        (string_of_typed_expr i)
        (string_of_typed_expr v)
  in
  Printf.sprintf "%s:%s" body ty

let opt_typed_expr = function
  | None -> "_"
  | Some e -> string_of_typed_expr e

let rec write_typed_stmt buf indent (s : Ast.typed_stmt) =
  let line fmt = Printf.ksprintf (fun str -> Buffer.add_string buf str) fmt in
  let pad = String.make (indent * 2) ' ' in
  let nested label body =
    line "%s(%s\n" pad label;
    List.iter (write_typed_stmt buf (indent + 1)) body;
    line "%s)\n" pad
  in
  match s.Ast.it with
  | `Expr e -> line "%s%s\n" pad (string_of_typed_expr e)
  | `Var_decl (name, _, init) -> line "%s(var %s %s)\n" pad name (opt_typed_expr init)
  | `Block body -> nested "block" body
  | `If (cond, then_branch, else_branch) ->
    nested
      (Printf.sprintf "if %s" (string_of_typed_expr cond))
      (then_branch :: Option.to_list else_branch)
  | `While (cond, body) ->
    nested (Printf.sprintf "while %s" (string_of_typed_expr cond)) [ body ]
  | `Fn (name, params, _, body) ->
    nested
      (Printf.sprintf
         "fn %s (%s)"
         name
         (String.concat " " (List.map (fun (p : Ast.param) -> p.Ast.name) params)))
      body
  | `Return value -> line "%s(return %s)\n" pad (opt_typed_expr value)
  | `Effect_decl (name, _) -> line "%s(effect %s)\n" pad name
  | `Run (body, handlers) ->
    nested
      (Printf.sprintf
         "run%s"
         (String.concat "" (List.map (fun h -> " handle " ^ h.Ast.handled) handlers)))
      (body @ List.concat_map (fun h -> List.concat_map (fun a -> a.Ast.arm_body) h.Ast.arms) handlers)
  | `Resume value -> line "%s(resume %s)\n" pad (opt_typed_expr value)

let string_of_typed_program (program : Ast.typed_stmt list) : string =
  let buf = Buffer.create 256 in
  List.iter (write_typed_stmt buf 0) program;
  Buffer.contents buf
