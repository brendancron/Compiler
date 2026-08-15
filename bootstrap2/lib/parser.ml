type error =
  { file : string
  ; line : int
  ; col : int
  ; message : string
  }

type state =
  { tokens : Token.token array
  ; mutable current : int
  ; mutable errors : error list (* reversed *)
  (* A `{` after a match's scrutinee opens the cases, so brace literals are
     suppressed there. Parenthesise to write one. *)
  ; mutable no_brace : bool
  }

(* Unwinds to the enclosing declaration. The error is recorded before raising. *)
exception Parse_error

let peek s = s.tokens.(s.current)
let previous s = s.tokens.(s.current - 1)
let is_at_end s = (peek s).Token.token_type = Token.Eof

let advance s =
  if not (is_at_end s) then s.current <- s.current + 1;
  previous s

let check s token_type = (not (is_at_end s)) && (peek s).Token.token_type = token_type

let matches s token_types =
  if List.exists (check s) token_types then Some (advance s) else None

let error s (tok : Token.token) message =
  s.errors <- { file = tok.Token.file; line = tok.line; col = tok.col; message } :: s.errors;
  Parse_error

let consume s token_type message =
  if check s token_type then advance s else raise (error s (peek s) message)

let consume_identifier s message =
  match (peek s).Token.token_type with
  | Token.Identifier name ->
    ignore (advance s);
    name
  | _ -> raise (error s (peek s) message)

let matches_binop s ops =
  let token_type = (peek s).Token.token_type in
  if List.mem token_type ops
  then (
    match Ast.binop_of_token token_type with
    | Some op ->
      ignore (advance s);
      Some op
    | None -> None)
  else None

(* Panic-mode recovery: skip to what looks like the start of a declaration. *)
let synchronize s =
  ignore (advance s);
  let rec loop () =
    if is_at_end s || (previous s).Token.token_type = Token.Semicolon
    then ()
    else (
      match (peek s).Token.token_type with
      | Token.Fn | Token.Var | Token.For | Token.If | Token.While | Token.Return -> ()
      | _ ->
        ignore (advance s);
        loop ())
  in
  loop ()

(* ---- type annotations ---- *)

let rec type_expr s : Ast.type_expr =
  let tok = peek s in
  let sp = Ast.span_of_token tok in
  match tok.Token.token_type with
  | Token.Left_brace ->
    ignore (advance s);
    let fields =
      if check s Token.Right_brace
      then []
      else (
        let rec loop acc =
          let label = consume_identifier s "Expected a field name." in
          ignore (consume s Token.Colon "Expected ':' after field name.");
          let ty = type_expr s in
          match matches s [ Token.Comma ] with
          | Some _ -> loop ((label, ty) :: acc)
          | None -> List.rev ((label, ty) :: acc)
        in
        loop [])
    in
    ignore (consume s Token.Right_brace "Expected '}' after record fields.");
    Ast.at sp (Ast.Ty_record fields)
  | Token.Identifier name ->
    ignore (advance s);
    (* `geom.Point` — a type belonging to another unit, spelled the way a value
       from one is. The loader resolves the pair; nothing after it sees a dot. *)
    let name =
      if check s Token.Dot
      then (
        ignore (advance s);
        name ^ "." ^ consume_identifier s "Expected a type name after '.'.")
      else name
    in
    if check s Token.Less
    then (
      ignore (advance s);
      let rec loop acc =
        let arg = type_expr s in
        match matches s [ Token.Comma ] with
        | Some _ -> loop (arg :: acc)
        | None -> List.rev (arg :: acc)
      in
      let args = loop [] in
      ignore (consume s Token.Greater "Expected '>' after type arguments.");
      Ast.at sp (Ast.Ty_app (name, args)))
    else Ast.at sp (Ast.Ty_name name)
  | Token.Left_paren ->
    ignore (advance s);
    let items =
      if check s Token.Right_paren
      then []
      else (
        let rec loop acc =
          let p = type_expr s in
          match matches s [ Token.Comma ] with
          | Some _ -> loop (p :: acc)
          | None -> List.rev (p :: acc)
        in
        loop [])
    in
    ignore (consume s Token.Right_paren "Expected ')' after type list.");
    if check s Token.Arrow
    then (
      ignore (advance s);
      (* The row comes first, or `<` after the arrow reads as a type argument. *)
      let row = row_annotation s in
      Ast.at sp (Ast.Ty_fn (items, type_expr s, row)))
    else Ast.at sp (Ast.Ty_tuple items)
  | _ -> raise (error s tok "Expected a type.")

and row_annotation s : string list =
  match matches s [ Token.Less ] with
  | None -> []
  | Some _ ->
    if check s Token.Greater
    then (
      ignore (advance s);
      [])
    else (
      let rec loop acc =
        let label = consume_identifier s "Expected an effect name." in
        match matches s [ Token.Comma ] with
        | Some _ -> loop (label :: acc)
        | None -> List.rev (label :: acc)
      in
      let labels = loop [] in
      ignore (consume s Token.Greater "Expected '>' after effect row.");
      labels)

let comptime_params s : Ast.comptime_param list =
  match matches s [ Token.Less ] with
  | None -> []
  | Some _ ->
    let rec loop acc =
      let name = consume_identifier s "Expected a comptime parameter name." in
      let ty =
        match matches s [ Token.Colon ] with
        | Some _ -> Some (type_expr s)
        | None -> None
      in
      let p = { Ast.cp_name = name; cp_ty = ty } in
      match matches s [ Token.Comma ] with
      | Some _ -> loop (p :: acc)
      | None -> List.rev (p :: acc)
    in
    let params = loop [] in
    ignore (consume s Token.Greater "Expected '>' after comptime parameters.");
    params

let type_params s : string list =
  List.map (fun (p : Ast.comptime_param) -> p.Ast.cp_name) (comptime_params s)

let type_annotation s : Ast.type_expr option =
  match matches s [ Token.Colon ] with
  | Some _ -> Some (type_expr s)
  | None -> None

let signature ?(comptime = []) s : Ast.signature =
  match matches s [ Token.Arrow; Token.Colon ] with
  | None -> { Ast.ret = None; row = None; comptime }
  | Some _ ->
    let row = if check s Token.Less then Some (row_annotation s) else None in
    { Ast.ret = Some (type_expr s); row; comptime }

(* ---- expressions ---- *)

let rec expression s : Ast.expr = assignment s

and assignment s : Ast.expr =
  let left = or_expr s in
  let target op tok =
    let value = assignment s in
    match left.Ast.it, op with
    | `Var name, None -> Ast.at left.Ast.span (`Assign (name, value))
    | `Var name, Some binop -> Ast.at left.Ast.span (`Compound (binop, name, value))
    | `Index (target, i), None ->
      Ast.at left.Ast.span (`Index_assign (target, i, value))
    | `Field (target, label), None ->
      Ast.at left.Ast.span (`Field_assign (target, label, value))
    | _ ->
      ignore (error s tok "Invalid assignment target.");
      left
  in
  match (peek s).Token.token_type with
  | Token.Equal -> target None (advance s)
  | Token.Plus_equal -> target (Some Ast.Add) (advance s)
  | Token.Minus_equal -> target (Some Ast.Sub) (advance s)
  | Token.Star_equal -> target (Some Ast.Mul) (advance s)
  | Token.Slash_equal -> target (Some Ast.Div) (advance s)
  | Token.Percent_equal -> target (Some Ast.Mod) (advance s)
  | _ -> left

and or_expr s : Ast.expr =
  let rec loop left =
    match matches s [ Token.Pipe_pipe ] with
    | Some _ ->
      let right = and_expr s in
      loop (Ast.at left.Ast.span (`Or (left, right)))
    | None -> left
  in
  loop (and_expr s)

and and_expr s : Ast.expr =
  let rec loop left =
    match matches s [ Token.Amp_amp ] with
    | Some _ ->
      let right = equality s in
      loop (Ast.at left.Ast.span (`And (left, right)))
    | None -> left
  in
  loop (equality s)

and binary_level s ops next : Ast.expr =
  let rec loop left =
    match matches_binop s ops with
    | Some op ->
      let right = next s in
      loop (Ast.at left.Ast.span (`Binop (op, left, right)))
    | None -> left
  in
  loop (next s)

and equality s : Ast.expr =
  binary_level s [ Token.Bang_equal; Token.Equal_equal ] comparison

and comparison s : Ast.expr =
  binary_level
    s
    [ Token.Greater; Token.Greater_equal; Token.Less; Token.Less_equal ]
    term

and term s : Ast.expr = binary_level s [ Token.Minus; Token.Plus ] factor
and factor s : Ast.expr = binary_level s [ Token.Slash; Token.Star; Token.Percent ] unary

and unary s : Ast.expr =
  match Ast.unop_of_token (peek s).Token.token_type with
  | Some op ->
    let tok = advance s in
    let right = unary s in
    Ast.at (Ast.span_of_token tok) (`Unop (op, right))
  | None -> postfix s

(* These yield the updated value, so `++x` and `x++` would mean the same
   thing; only the postfix spelling is accepted. *)
and postfix s : Ast.expr =
  let rec loop left =
    let step op tok =
      match left.Ast.it with
      | `Var name ->
        let one = Ast.at (Ast.span_of_token tok) (`Int 1) in
        loop (Ast.at left.Ast.span (`Compound (op, name, one)))
      | _ ->
        ignore (error s tok "Invalid increment target.");
        left
    in
    match (peek s).Token.token_type with
    | Token.Plus_plus -> step Ast.Add (advance s)
    | Token.Minus_minus -> step Ast.Sub (advance s)
    | _ -> left
  in
  loop (call s)

and call s : Ast.expr =
  let rec loop callee =
    match (peek s).Token.token_type with
    | Token.Left_paren ->
      ignore (advance s);
      loop (finish_call s callee)
    | Token.Left_bracket ->
      ignore (advance s);
      let index = expression s in
      ignore (consume s Token.Right_bracket "Expected ']' after index.");
      loop (Ast.at callee.Ast.span (`Index (callee, index)))
    | Token.Less ->
      (match comptime_arguments s with
       | Some comptime_args ->
         ignore (consume s Token.Left_paren "Expected '(' after comptime arguments.");
         loop
           (Ast.at
              callee.Ast.span
              (`Comptime_call (callee, comptime_args, arguments s)))
       | None -> callee)
    | Token.Dot ->
      ignore (advance s);
      (match (peek s).Token.token_type with
       | Token.Int n ->
         ignore (advance s);
         loop (Ast.at callee.Ast.span (`Tuple_get (callee, n)))
       (* A keyword is an ordinary word after a dot, since nothing else may
          stand there — which is what lets a module export `new`. *)
       | _ when Scanner.keyword (peek s).Token.lexeme <> None ->
         let label = (peek s).Token.lexeme in
         ignore (advance s);
         (match matches s [ Token.Left_paren ] with
          | Some _ ->
            loop
              (Ast.at
                 callee.Ast.span
                 (`Method_call (callee, label, label, trailing_lambda s (arguments s))))
          | None -> loop (Ast.at callee.Ast.span (`Field (callee, label))))
       | Token.Identifier _ ->
         let label = (peek s).Token.lexeme in
         ignore (advance s);
         (match matches s [ Token.Left_paren ] with
          | Some _ ->
            loop
              (Ast.at
                 callee.Ast.span
                 (`Method_call (callee, label, label, trailing_lambda s (arguments s))))
          | None -> loop (Ast.at callee.Ast.span (`Field (callee, label))))
       | _ -> raise (error s (peek s) "Expected a field after '.'."))
    | _ -> callee
  in
  loop (primary s)

(* `f<int>(x)` and `a < b > (c)` are the same shape and whitespace cannot
   separate them, so this is accepted only when a call follows. Speculative:
   position and recorded errors are put back when it is not one. *)
and comptime_arguments s : Ast.expr Ast.comptime_arg list option =
  let start = s.current
  and errors = s.errors in
  let restore () =
    s.current <- start;
    s.errors <- errors;
    None
  in
  match
    ignore (advance s);
    let rec loop acc =
      let arg =
        let start = s.current
        and errors = s.errors in
        let as_value () =
          s.current <- start;
          s.errors <- errors;
          (* Below comparison, or the closing '>' reads as an operator. *)
          Ast.Ct_value (unary s)
        in
        match type_expr s with
        | t when check s Token.Comma || check s Token.Greater -> Ast.Ct_type t
        | _ -> as_value ()
        | exception Parse_error -> as_value ()
      in
      match matches s [ Token.Comma ] with
      | Some _ -> loop (arg :: acc)
      | None -> List.rev (arg :: acc)
    in
    let args = loop [] in
    if check s Token.Greater
    then (
      ignore (advance s);
      Some args)
    else None
  with
  | Some args when check s Token.Left_paren -> Some args
  | _ -> restore ()
  | exception Parse_error -> restore ()

(* Assumes the '(' has been consumed; consumes the closing ')'. *)
and arguments s : Ast.expr list =
  let args =
    if check s Token.Right_paren
    then []
    else (
      let rec loop acc =
        let arg = expression s in
        match matches s [ Token.Comma ] with
        | Some _ -> loop (arg :: acc)
        | None -> List.rev (arg :: acc)
      in
      loop [])
  in
  ignore (consume s Token.Right_paren "Expected ')' after arguments.");
  args

(* `f(xs) { it * 2 }` — a block after a call is a one-parameter function value
   passed last, and `it` is what that parameter is called. *)
and trailing_lambda s args : Ast.expr list =
  if not (check s Token.Left_brace && not s.no_brace)
  then args
  else (
    let sp = Ast.span_of_token (peek s) in
    ignore (advance s);
    (* `{ it * 2 }` is the value and `{ var t = it; return t; }` is a body.
       Which one it is takes trying: an expression filling the braces exactly
       is the first, anything else the second. *)
    let mark = s.current
    and errors = s.errors in
    let restore () =
      s.current <- mark;
      s.errors <- errors;
      block s
    in
    let body =
      match expression s with
      | value when check s Token.Right_brace ->
        ignore (advance s);
        [ { Ast.it = `Return (Some value); span = value.Ast.span; ann = () } ]
      | _ -> restore ()
      | exception Parse_error -> restore ()
    in
    args
    @ [ Ast.at
          sp
          (`Lambda
             ( [ { Ast.name = "it"; ty = None } ]
             , { Ast.ret = None; row = None; comptime = [] }
             , body ))
      ])

and finish_call s callee : Ast.expr =
  Ast.at callee.Ast.span (`Call (callee, trailing_lambda s (arguments s)))

(* Assumes the '{' has been consumed; consumes the closing '}'. *)
and record_fields s : (string * Ast.expr) list =
  let fields =
    if check s Token.Right_brace
    then []
    else (
      let rec loop acc =
        let label = consume_identifier s "Expected a field name." in
        ignore (consume s Token.Colon "Expected ':' after field name.");
        let value = expression s in
        match matches s [ Token.Comma ] with
        | Some _ -> loop ((label, value) :: acc)
        | None -> List.rev ((label, value) :: acc)
      in
      loop [])
  in
  ignore (consume s Token.Right_brace "Expected '}' after fields.");
  fields

and starts_lambda s =
  let depth = ref 0
  and at = ref s.current
  and answer = ref false
  and looking = ref true in
  while !looking do
    (match s.tokens.(!at).Token.token_type with
     | Token.Left_paren -> incr depth
     | Token.Right_paren ->
       decr depth;
       if !depth = 0
       then (
         answer := s.tokens.(!at + 1).Token.token_type = Token.Fat_arrow;
         looking := false)
     | Token.Eof -> looking := false
     | _ -> ());
    incr at
  done;
  !answer

and lambda s sp : Ast.expr =
  ignore (consume s Token.Left_paren "Expected '(' before parameters.");
  let params = parameters s in
  ignore (consume s Token.Fat_arrow "Expected '=>' after parameters.");
  let body =
    if check s Token.Left_brace
    then (
      ignore (advance s);
      block s)
    else (
      let e = expression s in
      [ { Ast.it = `Return (Some e); span = e.Ast.span; ann = () } ])
  in
  Ast.at sp (`Lambda (params, { Ast.ret = None; row = None; comptime = [] }, body))

and primary s : Ast.expr =
  let tok = peek s in
  let sp = Ast.span_of_token tok in
  match tok.Token.token_type with
  | Token.Int n ->
    ignore (advance s);
    Ast.at sp (`Int n)
  | Token.Float n ->
    ignore (advance s);
    Ast.at sp (`Float n)
  | Token.String str ->
    ignore (advance s);
    Ast.at sp (`Str (Utf8.decode str))
  | Token.Char c ->
    ignore (advance s);
    Ast.at sp (`Char c)
  | Token.True ->
    ignore (advance s);
    Ast.at sp (`Bool true)
  | Token.False ->
    ignore (advance s);
    Ast.at sp (`Bool false)
  | Token.Identifier name ->
    ignore (advance s);
    Ast.at sp (`Var name)
  | Token.Typeof ->
    ignore (advance s);
    ignore (consume s Token.Left_paren "Expected '(' after 'typeof'.");
    let e = expression s in
    ignore (consume s Token.Right_paren "Expected ')' after typeof operand.");
    Ast.at sp (`Typeof e)
  | Token.Code ->
    ignore (advance s);
    ignore (consume s Token.Left_paren "Expected '(' after 'code'.");
    let e = expression s in
    ignore (consume s Token.Right_paren "Expected ')' after the captured expression.");
    Ast.at sp (`Code e)
  | Token.New ->
    ignore (advance s);
    let name = consume_identifier s "Expected a type name after 'new'." in
    (* `new geom.Point { … }`, matching how the type is written in an
       annotation. `::` still separates a sum type from its variant. *)
    let name =
      if check s Token.Dot
      then (
        ignore (advance s);
        name ^ "." ^ consume_identifier s "Expected a type name after '.'.")
      else name
    in
    if check s Token.Colon_colon
    then (
      ignore (advance s);
      let variant = consume_identifier s "Expected a variant name." in
      let payload =
        match (peek s).Token.token_type with
        | Token.Left_paren ->
          ignore (advance s);
          let rec args acc =
            let e = expression s in
            match matches s [ Token.Comma ] with
            | Some _ -> args (e :: acc)
            | None -> List.rev (e :: acc)
          in
          let items = args [] in
          ignore (consume s Token.Right_paren "Expected ')' after arguments.");
          Ast.P_tuple items
        | Token.Left_brace when not s.no_brace ->
          ignore (advance s);
          Ast.P_fields (record_fields s)
        | _ -> Ast.P_none
      in
      Ast.at sp (`New_variant (name, variant, payload)))
    else (
      let type_args =
        if check s Token.Less
        then (
          ignore (advance s);
          let rec loop acc =
            let t = type_expr s in
            match matches s [ Token.Comma ] with
            | Some _ -> loop (t :: acc)
            | None -> List.rev (t :: acc)
          in
          let args = loop [] in
          ignore (consume s Token.Greater "Expected '>' after type arguments.");
          args)
        else []
      in
      match matches s [ Token.Left_paren ] with
      | Some _ -> Ast.at sp (`New_call (name, type_args, arguments s))
      | None ->
        ignore (consume s Token.Left_brace "Expected '{' after type name.");
        Ast.at sp (`New (name, record_fields s)))
  | Token.Left_brace when not s.no_brace ->
    ignore (advance s);
    Ast.at sp (`Record_lit (record_fields s))
  | Token.Left_bracket ->
    ignore (advance s);
    let items =
      if check s Token.Right_bracket
      then []
      else (
        let rec loop acc =
          let item = expression s in
          match matches s [ Token.Comma ] with
          | Some _ -> loop (item :: acc)
          | None -> List.rev (item :: acc)
        in
        loop [])
    in
    ignore (consume s Token.Right_bracket "Expected ']' after collection items.");
    Ast.at sp (`Collection_lit items)
  | Token.Left_paren when starts_lambda s -> lambda s sp
  | Token.Left_paren ->
    ignore (advance s);
    let saved = s.no_brace in
    s.no_brace <- false;
    let first = expression s in
    s.no_brace <- saved;
    (* A comma is what makes it a tuple; otherwise no node survives the parens. *)
    if check s Token.Comma
    then (
      let rec loop acc =
        match matches s [ Token.Comma ] with
        | Some _ -> loop (expression s :: acc)
        | None -> List.rev acc
      in
      let items = loop [ first ] in
      ignore (consume s Token.Right_paren "Expected ')' after tuple.");
      Ast.at sp (`Tuple items))
    else (
      ignore (consume s Token.Right_paren "Expected ')' after expression.");
      first)
  | _ -> raise (error s tok "Expected an expression.")

(* ---- statements ---- *)

(* One group with the expressions, because a function value is an expression
   whose body is statements. *)
and declaration s : Ast.stmt option =
  try
    let tok = peek s in
    let sp = Ast.span_of_token tok in
    match tok.Token.token_type with
    | Token.Fn ->
      ignore (advance s);
      Some (fn_decl s sp)
    | Token.Var ->
      ignore (advance s);
      Some (var_decl s sp)
    | Token.Effect ->
      ignore (advance s);
      Some (effect_decl s sp)
    | Token.Handler ->
      ignore (advance s);
      Some (handler_decl s sp)
    | Token.Type ->
      ignore (advance s);
      Some (type_decl s sp)
    | Token.Op ->
      ignore (advance s);
      Some (op_decl s sp)
    | Token.Trait ->
      ignore (advance s);
      Some (trait_decl s sp)
    | Token.Impl ->
      ignore (advance s);
      Some (impl_decl s sp)
    | Token.Import ->
      ignore (advance s);
      Some (import_decl s sp)
    | Token.Derive ->
      ignore (advance s);
      let rec traits acc =
        let name = consume_identifier s "Expected a trait name." in
        match matches s [ Token.Comma ] with
        | Some _ -> traits (name :: acc)
        | None -> List.rev (name :: acc)
      in
      let traits = traits [] in
      ignore (consume s Token.For "Expected 'for' after the traits to derive.");
      let target = consume_identifier s "Expected the type to derive for." in
      ignore (consume s Token.Semicolon "Expected ';' after a derive.");
      Some (Ast.at sp (`Derive (traits, target)))
    | Token.Meta ->
      ignore (advance s);
      (* Braces are a block of statements, not part of the form: `meta` runs
         whatever follows it, one statement or many. *)
      if check s Token.Fn
      then (
        ignore (advance s);
        (* `for Trait` is what registers it, so the name written is free to be
           `derive` for every deriver. *)
        (* Every deriver is called `derive`, which is the keyword, so the name
           is read here rather than as an ordinary identifier. *)
        let read_name s =
          match matches s [ Token.Derive ] with
          | Some _ -> "derive"
          | None -> consume_identifier s "Expected function name."
        in
        let before_body s name =
          match matches s [ Token.For ] with
          | None -> name
          | Some _ ->
            Ast.deriver_name (consume_identifier s "Expected a trait name after 'for'.")
        in
        match (fn_decl ~read_name ~before_body s sp).Ast.it with
        | `Fn (name, params, signature, body) ->
          Some (Ast.at sp (`Meta_fn (name, params, signature, body)))
        | _ -> None)
      else if check s Token.Left_brace
      then (
        ignore (advance s);
        Some (Ast.at sp (`Meta (block s))))
      else (
        match declaration s with
        | Some inner -> Some (Ast.at sp (`Meta [ inner ]))
        | None -> None)
    | Token.Gen ->
      ignore (advance s);
      (* Whatever may stand at this position may be generated, so a `gen` takes
         a declaration as readily as a statement. *)
      (match declaration s with
       | Some inner -> Some (Ast.at sp (`Gen inner))
       | None -> None)
    | _ -> Some (statement s)
  with
  | Parse_error ->
    synchronize s;
    None

(* Assumes the '(' has been consumed; consumes the closing ')'. *)
and parameters s : Ast.param list =
  let params =
    if check s Token.Right_paren
    then []
    else (
      let rec loop acc =
        let param_name = consume_identifier s "Expected parameter name." in
        let p = { Ast.name = param_name; ty = type_annotation s } in
        match matches s [ Token.Comma ] with
        | Some _ -> loop (p :: acc)
        | None -> List.rev (p :: acc)
      in
      loop [])
  in
  ignore (consume s Token.Right_paren "Expected ')' after parameters.");
  params

and fn_decl
  ?(read_name =
    fun s ->
      (* `new` is the constructor convention, and in declaration position it
         cannot be read as the operator. No other keyword is allowed here. *)
      match matches s [ Token.New ] with
      | Some _ -> "new"
      | None -> consume_identifier s "Expected function name.")
  ?(before_body = fun _ name -> name)
  s
  sp
  : Ast.stmt
  =
  let name = read_name s in
  let comptime = comptime_params s in
  ignore (consume s Token.Left_paren "Expected '(' after function name.");
  let params = parameters s in
  let signature = signature ~comptime s in
  let name = before_body s name in
  ignore (consume s Token.Left_brace "Expected '{' before function body.");
  Ast.at sp (`Fn (name, params, signature, block s))

and var_decl s sp : Ast.stmt =
  let name = consume_identifier s "Expected variable name." in
  let ty = type_annotation s in
  let init =
    match matches s [ Token.Equal ] with
    | Some _ -> Some (expression s)
    | None -> None
  in
  ignore (consume s Token.Semicolon "Expected ';' after variable declaration.");
  Ast.at sp (`Var_decl (name, ty, init))

(* Assumes the '{' has been consumed; consumes the closing '}'. *)
and block s : Ast.stmt list =
  let rec loop acc =
    if check s Token.Right_brace || is_at_end s
    then List.rev acc
    else (
      match declaration s with
      | Some st -> loop (st :: acc)
      | None -> loop acc)
  in
  let body = loop [] in
  ignore (consume s Token.Right_brace "Expected '}' after block.");
  body

and statement s : Ast.stmt =
  let tok = peek s in
  let sp = Ast.span_of_token tok in
  match tok.Token.token_type with
  | Token.For ->
    ignore (advance s);
    for_stmt s sp
  | Token.If ->
    ignore (advance s);
    if_stmt s sp
  | Token.While ->
    ignore (advance s);
    while_stmt s sp
  | Token.Return ->
    ignore (advance s);
    return_stmt s sp
  | Token.Run ->
    ignore (advance s);
    run_stmt s sp
  | Token.Match ->
    ignore (advance s);
    match_stmt s sp
  | Token.Resume ->
    ignore (advance s);
    resume_stmt s sp
  | Token.Left_brace ->
    ignore (advance s);
    Ast.at sp (`Block (block s))
  | _ -> expr_stmt s sp

and expr_stmt s sp : Ast.stmt =
  let e = expression s in
  ignore (consume s Token.Semicolon "Expected ';' after expression.");
  Ast.at sp (`Expr e)

and if_stmt s sp : Ast.stmt =
  ignore (consume s Token.Left_paren "Expected '(' after 'if'.");
  let cond = expression s in
  ignore (consume s Token.Right_paren "Expected ')' after if condition.");
  let then_branch = statement s in
  let else_branch =
    match matches s [ Token.Else ] with
    | Some _ -> Some (statement s)
    | None -> None
  in
  Ast.at sp (`If (cond, then_branch, else_branch))

and while_stmt s sp : Ast.stmt =
  ignore (consume s Token.Left_paren "Expected '(' after 'while'.");
  let cond = expression s in
  ignore (consume s Token.Right_paren "Expected ')' after while condition.");
  Ast.at sp (`While (cond, statement s))

and for_stmt s sp : Ast.stmt =
  ignore (consume s Token.Left_paren "Expected '(' after 'for'.");
  (* `in` is not a keyword, so the two forms are told apart by lookahead. *)
  let iterates =
    s.current + 1 < Array.length s.tokens
    &&
    match (peek s).Token.token_type, s.tokens.(s.current + 1).Token.token_type with
    | Token.Identifier _, Token.Identifier "in" -> true
    | _ -> false
  in
  if iterates
  then (
    let name = consume_identifier s "Expected a loop variable." in
    ignore (advance s);
    let iterable = expression s in
    ignore (consume s Token.Right_paren "Expected ')' after the iterable.");
    Ast.at sp (`For_in (name, iterable, statement s)))
  else (
  let init =
    let tok = peek s in
    match tok.Token.token_type with
    | Token.Semicolon ->
      ignore (advance s);
      None
    | Token.Var ->
      ignore (advance s);
      Some (var_decl s (Ast.span_of_token tok))
    | _ -> Some (expr_stmt s (Ast.span_of_token tok))
  in
  let cond = if check s Token.Semicolon then None else Some (expression s) in
  ignore (consume s Token.Semicolon "Expected ';' after loop condition.");
  let step = if check s Token.Right_paren then None else Some (expression s) in
  ignore (consume s Token.Right_paren "Expected ')' after for clauses.");
  Ast.at sp (`For (init, cond, step, statement s)))

(* `as` and `from` are ordinary identifiers, matched by text, so neither
   becomes a word a program cannot use for something else. *)
and import_decl s sp : Ast.stmt =
  let text tok =
    match tok.Token.token_type with
    | Token.String path -> path
    | _ -> raise (error s tok "Expected a quoted path.")
  in
  let decl =
    if check s Token.Left_brace
    then (
      ignore (advance s);
      let rec names acc =
        let name = consume_identifier s "Expected an imported name." in
        match matches s [ Token.Comma ] with
        | Some _ -> names (name :: acc)
        | None -> List.rev (name :: acc)
      in
      let names = names [] in
      ignore (consume s Token.Right_brace "Expected '}' after the imported names.");
      (match (peek s).Token.token_type with
       | Token.Identifier "from" -> ignore (advance s)
       | _ -> raise (error s (peek s) "Expected 'from' after the imported names."));
      Ast.Selective (names, text (advance s)))
    else (
      let path = text (advance s) in
      match (peek s).Token.token_type with
      | Token.Identifier "as" ->
        ignore (advance s);
        Ast.Aliased (path, consume_identifier s "Expected a name after 'as'.")
      | _ ->
        (match String.length path >= 2 && String.sub path (String.length path - 2) 2 = "/*" with
         | true -> Ast.Wildcard (String.sub path 0 (String.length path - 2))
         | false -> Ast.Qualified path))
  in
  ignore (matches s [ Token.Semicolon ]);
  Ast.at sp (`Import decl)

and op_kind s : Ast.op_kind =
  match (peek s).Token.token_type with
  | Token.Fn ->
    ignore (advance s);
    Ast.Op_fn
  | Token.Ctl ->
    ignore (advance s);
    Ast.Op_ctl
  | Token.Final ->
    ignore (advance s);
    ignore (consume s Token.Ctl "Expected 'ctl' after 'final'.");
    Ast.Op_final
  | _ -> raise (error s (peek s) "Expected 'fn', 'ctl' or 'final ctl'.")

and effect_decl s sp : Ast.stmt =
  let name = consume_identifier s "Expected effect name." in
  let params = type_params s in
  ignore (consume s Token.Left_brace "Expected '{' after effect name.");
  let rec loop acc =
    if check s Token.Right_brace || is_at_end s
    then List.rev acc
    else (
      let kind = op_kind s in
      let op_name = consume_identifier s "Expected operation name." in
      ignore (consume s Token.Left_paren "Expected '(' after operation name.");
      let params =
        if check s Token.Right_paren
        then []
        else (
          let rec params acc =
            let param_name = consume_identifier s "Expected parameter name." in
            let p = { Ast.name = param_name; ty = type_annotation s } in
            match matches s [ Token.Comma ] with
            | Some _ -> params (p :: acc)
            | None -> List.rev (p :: acc)
          in
          params [])
      in
      ignore (consume s Token.Right_paren "Expected ')' after parameters.");
      let op_ret =
        match matches s [ Token.Arrow; Token.Colon ] with
        | Some _ -> Some (type_expr s)
        | None -> None
      in
      ignore (consume s Token.Semicolon "Expected ';' after operation.");
      loop ({ Ast.op_name; op_kind = kind; op_params = params; op_ret } :: acc))
  in
  let ops = loop [] in
  ignore (consume s Token.Right_brace "Expected '}' after effect operations.");
  Ast.at sp (`Effect_decl (name, params, ops))

and op_decl s sp : Ast.stmt =
  let tok = advance s in
  let op =
    match tok.Token.token_type with
    | Token.Left_bracket ->
      ignore (consume s Token.Right_bracket "Expected ']' after '['.");
      if matches s [ Token.Equal ] <> None then Ast.Op_index_set else Ast.Op_index
    | other ->
      (match Ast.binop_of_token other with
       | Some op -> Ast.Op_binary op
       | None -> raise (error s tok "Expected an operator after 'op'."))
  in
  let comptime = comptime_params s in
  ignore (consume s Token.Left_paren "Expected '(' after the operator.");
  let params =
    if check s Token.Right_paren
    then []
    else (
      let rec loop acc =
        let name = consume_identifier s "Expected a parameter name." in
        let p = { Ast.name; ty = type_annotation s } in
        match matches s [ Token.Comma ] with
        | Some _ -> loop (p :: acc)
        | None -> List.rev (p :: acc)
      in
      loop [])
  in
  ignore (consume s Token.Right_paren "Expected ')' after operands.");
  let signature = signature ~comptime s in
  ignore (consume s Token.Left_brace "Expected '{' before the operator body.");
  Ast.at sp (`Op_decl (op, params, signature, block s))

and trait_decl s sp : Ast.stmt =
  let name = consume_identifier s "Expected a trait name." in
  ignore (consume s Token.Left_brace "Expected '{' after the trait name.");
  let rec loop acc =
    if check s Token.Right_brace || is_at_end s
    then List.rev acc
    else (
      ignore (consume s Token.Fn "Expected a method signature.");
      let method_name = consume_identifier s "Expected a method name." in
      let comptime = comptime_params s in
      ignore (consume s Token.Left_paren "Expected '(' after the method name.");
      let params = parameters s in
      let signature = signature ~comptime s in
      ignore (consume s Token.Semicolon "Expected ';' after a method signature.");
      loop
        ({ Ast.ms_name = method_name; ms_params = params; ms_signature = signature }
         :: acc))
  in
  let methods = loop [] in
  ignore (consume s Token.Right_brace "Expected '}' after the trait body.");
  Ast.at sp (`Trait_decl (name, methods))

and impl_decl s sp : Ast.stmt =
  let first = consume_identifier s "Expected a type or trait name after 'impl'." in
  let first_params = type_params s in
  let trait, type_name, params =
    match matches s [ Token.For ] with
    | Some _ ->
      let name = consume_identifier s "Expected a type name after 'for'." in
      Some first, name, type_params s
    | None -> None, first, first_params
  in
  ignore (consume s Token.Left_brace "Expected '{' after the impl header.");
  let rec loop acc =
    if check s Token.Right_brace || is_at_end s
    then List.rev acc
    else (
      ignore (consume s Token.Fn "Expected a method.");
      let method_name = consume_identifier s "Expected a method name." in
      let comptime = comptime_params s in
      ignore (consume s Token.Left_paren "Expected '(' after the method name.");
      let params = parameters s in
      let signature = signature ~comptime s in
      ignore (consume s Token.Left_brace "Expected '{' before the method body.");
      loop
        ({ Ast.md_name = method_name
         ; md_params = params
         ; md_signature = signature
         ; md_body = block s
         ; md_ann = ()
         }
         :: acc))
  in
  let methods = loop [] in
  ignore (consume s Token.Right_brace "Expected '}' after the methods.");
  Ast.at sp (`Impl_decl (trait, type_name, params, methods))

and type_decl s sp : Ast.stmt =
  let name = consume_identifier s "Expected a type name." in
  let params = type_params s in
  ignore (consume s Token.Left_brace "Expected '{' after type name.");
  let rec loop fields variants =
    if check s Token.Right_brace || is_at_end s
    then List.rev fields, List.rev variants
    else (
      let label = consume_identifier s "Expected a field or variant name." in
      match (peek s).Token.token_type with
      | Token.Colon ->
        if variants <> []
        then ignore (error s (peek s) "A type declares fields or variants, not both.");
        ignore (advance s);
        let ty = type_expr s in
        ignore (matches s [ Token.Comma ]);
        loop ((label, ty) :: fields) variants
      | _ ->
        if fields <> []
        then ignore (error s (peek s) "A type declares fields or variants, not both.");
        let payload =
          match (peek s).Token.token_type with
          | Token.Left_paren ->
            ignore (advance s);
            let rec args acc =
              let t = type_expr s in
              match matches s [ Token.Comma ] with
              | Some _ -> args (t :: acc)
              | None -> List.rev (t :: acc)
            in
            let items = args [] in
            ignore (consume s Token.Right_paren "Expected ')' after variant payload.");
            Ast.P_tuple items
          | Token.Left_brace ->
            ignore (advance s);
            let rec named acc =
              if check s Token.Right_brace
              then List.rev acc
              else (
                let l = consume_identifier s "Expected a field name." in
                ignore (consume s Token.Colon "Expected ':' after field name.");
                let t = type_expr s in
                ignore (matches s [ Token.Comma ]);
                named ((l, t) :: acc))
            in
            let fields = named [] in
            ignore (consume s Token.Right_brace "Expected '}' after variant fields.");
            Ast.P_fields fields
          | _ -> Ast.P_none
        in
        ignore (matches s [ Token.Comma ]);
        loop fields ({ Ast.v_name = label; v_payload = payload } :: variants))
  in
  let fields, variants = loop [] [] in
  ignore (consume s Token.Right_brace "Expected '}' after type body.");
  let body = if variants <> [] then Ast.T_variants variants else Ast.T_fields fields in
  Ast.at sp (`Type_decl (name, params, body))

and handler_decl s sp : Ast.stmt =
  let name = consume_identifier s "Expected handler name." in
  ignore (consume s Token.Colon "Expected ':' after handler name.");
  Ast.at sp (`Handler_decl (name, handler s))

and handler s : Ast.stmt Ast.handler =
  let handled = consume_identifier s "Expected an effect name." in
  ignore (consume s Token.Left_brace "Expected '{' after effect name.");
  let rec loop acc =
    if check s Token.Right_brace || is_at_end s
    then List.rev acc
    else (
      let kind = op_kind s in
      let arm_name = consume_identifier s "Expected operation name." in
      ignore (consume s Token.Left_paren "Expected '(' after operation name.");
      let params =
        if check s Token.Right_paren
        then []
        else (
          let rec params acc =
            let p = consume_identifier s "Expected parameter name." in
            ignore (type_annotation s);
            match matches s [ Token.Comma ] with
            | Some _ -> params (p :: acc)
            | None -> List.rev (p :: acc)
          in
          params [])
      in
      ignore (consume s Token.Right_paren "Expected ')' after parameters.");
      ignore (consume s Token.Left_brace "Expected '{' before operation body.");
      loop
        ({ Ast.arm_name; arm_kind = kind; arm_params = params; arm_body = block s } :: acc))
  in
  let arms = loop [] in
  ignore (consume s Token.Right_brace "Expected '}' after handler.");
  { Ast.handled; arms }

and run_stmt s sp : Ast.stmt =
  ignore (consume s Token.Left_brace "Expected '{' after 'run'.");
  let body = block s in
  let rec loop acc =
    match (peek s).Token.token_type with
    | Token.Handle ->
      ignore (advance s);
      loop (Ast.Inline (handler s) :: acc)
    | Token.With ->
      ignore (advance s);
      loop (Ast.Named (consume_identifier s "Expected a handler name.") :: acc)
    | _ -> List.rev acc
  in
  let handlers = loop [] in
  if handlers = []
  then ignore (error s (peek s) "Expected 'handle' or 'with' after a run block.");
  (* `with` takes a terminator; `handle` closes with its own brace. *)
  ignore (matches s [ Token.Semicolon ]);
  Ast.at sp (`Run (body, handlers))

and resume_stmt s sp : Ast.stmt =
  let value = if check s Token.Semicolon then None else Some (expression s) in
  ignore (consume s Token.Semicolon "Expected ';' after resume.");
  Ast.at sp (`Resume value)

and match_stmt s sp : Ast.stmt =
  let saved = s.no_brace in
  s.no_brace <- true;
  let scrutinee = expression s in
  s.no_brace <- saved;
  ignore (consume s Token.Left_brace "Expected '{' after the matched value.");
  let rec loop acc =
    if check s Token.Right_brace || is_at_end s
    then List.rev acc
    else (
      let pattern =
        match (peek s).Token.token_type with
        | Token.Identifier "_" ->
          ignore (advance s);
          Ast.Pat_wild
        | _ ->
          let ty = consume_identifier s "Expected a pattern." in
          ignore (consume s Token.Colon_colon "Expected '::' after the type name.");
          let variant = consume_identifier s "Expected a variant name." in
          let payload =
            match (peek s).Token.token_type with
            | Token.Left_paren ->
              ignore (advance s);
              let rec names acc =
                let n = consume_identifier s "Expected a binding name." in
                match matches s [ Token.Comma ] with
                | Some _ -> names (n :: acc)
                | None -> List.rev (n :: acc)
              in
              let items = names [] in
              ignore (consume s Token.Right_paren "Expected ')' after bindings.");
              Ast.P_tuple items
            | Token.Left_brace ->
              ignore (advance s);
              let rec named acc =
                if check s Token.Right_brace
                then List.rev acc
                else (
                  let n = consume_identifier s "Expected a field name." in
                  ignore (matches s [ Token.Comma ]);
                  named ((n, n) :: acc))
              in
              let fields = named [] in
              ignore (consume s Token.Right_brace "Expected '}' after bindings.");
              Ast.P_fields fields
            | _ -> Ast.P_none
          in
          Ast.Pat_variant (ty, variant, payload)
      in
      ignore (consume s Token.Fat_arrow "Expected '=>' after a pattern.");
      ignore (consume s Token.Left_brace "Expected '{' after '=>'.");
      loop ((pattern, block s) :: acc))
  in
  let cases = loop [] in
  ignore (consume s Token.Right_brace "Expected '}' after match cases.");
  Ast.at sp (`Match (scrutinee, cases))

and return_stmt s sp : Ast.stmt =
  let value = if check s Token.Semicolon then None else Some (expression s) in
  ignore (consume s Token.Semicolon "Expected ';' after return value.");
  Ast.at sp (`Return value)

let parse (tokens : Token.token list) : (Ast.program, error list) result =
  let s = { tokens = Array.of_list tokens; current = 0; errors = []; no_brace = false } in
  let rec loop acc =
    if is_at_end s
    then List.rev acc
    else (
      match declaration s with
      | Some st -> loop (st :: acc)
      | None -> loop acc)
  in
  let program = loop [] in
  match s.errors with
  | [] -> Ok program
  | errors -> Error (List.rev errors)
