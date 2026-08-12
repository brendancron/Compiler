type error =
  { line : int
  ; col : int
  ; message : string
  }

type state =
  { source : string
  ; mutable start : int (* offset of the first char of the token being scanned *)
  ; mutable start_line : int (* line/col of that first char, captured before scanning *)
  ; mutable start_col : int
  ; mutable current : int (* offset of the next char to consume *)
  ; mutable line : int
  ; mutable line_start : int (* offset just past the most recent newline *)
  ; mutable tokens : Token.token list (* reversed *)
  ; mutable errors : error list (* reversed *)
  }

let col_of s offset = offset - s.line_start + 1

(* Call immediately after consuming a '\n'. *)
let newline s =
  s.line <- s.line + 1;
  s.line_start <- s.current

let is_at_end s = s.current >= String.length s.source

let advance s =
  let c = s.source.[s.current] in
  s.current <- s.current + 1;
  c

(* '\000' stands in for "past the end"; Lox source has no NUL bytes. *)
let peek s = if is_at_end s then '\000' else s.source.[s.current]

let peek_next s =
  if s.current + 1 >= String.length s.source then '\000' else s.source.[s.current + 1]

let matches s expected =
  if is_at_end s || s.source.[s.current] <> expected
  then false
  else (
    s.current <- s.current + 1;
    true)

let lexeme s = String.sub s.source s.start (s.current - s.start)

(* Tokens and errors are positioned at their first character, not at wherever
   the cursor happens to be — a multi-line string points at its opening quote. *)
let add_token s token_type =
  s.tokens
  <- Token.make token_type ~lexeme:(lexeme s) ~line:s.start_line ~col:s.start_col
     :: s.tokens

let error s message =
  s.errors <- { line = s.start_line; col = s.start_col; message } :: s.errors
let is_digit c = c >= '0' && c <= '9'
let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
let is_alphanumeric c = is_alpha c || is_digit c

let keyword = function
  | "and" -> Some Token.And
  | "ctl" -> Some Token.Ctl
  | "effect" -> Some Token.Effect
  | "else" -> Some Token.Else
  | "false" -> Some Token.False
  | "fn" -> Some Token.Fn
  | "for" -> Some Token.For
  | "if" -> Some Token.If
  | "handle" -> Some Token.Handle
  | "handler" -> Some Token.Handler
  | "match" -> Some Token.Match
  | "new" -> Some Token.New
  | "op" -> Some Token.Op
  | "or" -> Some Token.Or
  | "resume" -> Some Token.Resume
  | "return" -> Some Token.Return
  | "run" -> Some Token.Run
  | "true" -> Some Token.True
  | "type" -> Some Token.Type
  | "typeof" -> Some Token.Typeof
  | "var" -> Some Token.Var
  | "while" -> Some Token.While
  | "with" -> Some Token.With
  | _ -> None

let line_comment s =
  while peek s <> '\n' && not (is_at_end s) do
    ignore (advance s)
  done

let string_literal s =
  while peek s <> '"' && not (is_at_end s) do
    let c = advance s in
    if c = '\n' then newline s
  done;
  if is_at_end s
  then error s "Unterminated string."
  else (
    (* The closing quote. *)
    ignore (advance s);
    (* Trim the surrounding quotes. *)
    add_token s (Token.String (String.sub s.source (s.start + 1) (s.current - s.start - 2))))

let number s =
  while is_digit (peek s) do
    ignore (advance s)
  done;
  (* Only consume the '.' if a digit follows it: `123.` is not a valid float, so
     leave the dot for the caller to reject rather than building a bad lexeme.
     The dot is also what tells int and float literals apart. *)
  let is_float = peek s = '.' && is_digit (peek_next s) in
  if is_float
  then (
    ignore (advance s);
    while is_digit (peek s) do
      ignore (advance s)
    done);
  let text = lexeme s in
  if is_float
  then add_token s (Token.Float (float_of_string text))
  else (
    match int_of_string_opt text with
    | Some n -> add_token s (Token.Int n)
    | None -> error s (Printf.sprintf "Integer literal '%s' is out of range." text))

let identifier s =
  while is_alphanumeric (peek s) do
    ignore (advance s)
  done;
  let text = lexeme s in
  match keyword text with
  | Some token_type -> add_token s token_type
  | None -> add_token s (Token.Identifier text)

let scan_token s =
  match advance s with
  | '(' -> add_token s Token.Left_paren
  | ')' -> add_token s Token.Right_paren
  | '[' -> add_token s Token.Left_bracket
  | ']' -> add_token s Token.Right_bracket
  | '{' -> add_token s Token.Left_brace
  | '}' -> add_token s Token.Right_brace
  | ',' -> add_token s Token.Comma
  | ';' -> add_token s Token.Semicolon
  | ':' -> add_token s (if matches s ':' then Token.Colon_colon else Token.Colon)
  | '.' -> add_token s Token.Dot
  | '*' -> add_token s (if matches s '=' then Token.Star_equal else Token.Star)
  | '+' ->
    add_token
      s
      (if matches s '+'
       then Token.Plus_plus
       else if matches s '='
       then Token.Plus_equal
       else Token.Plus)
  | '-' ->
    add_token
      s
      (if matches s '-'
       then Token.Minus_minus
       else if matches s '='
       then Token.Minus_equal
       else if matches s '>'
       then Token.Arrow
       else Token.Minus)
  | '!' -> add_token s (if matches s '=' then Token.Bang_equal else Token.Bang)
  | '=' -> add_token s (if matches s '=' then Token.Equal_equal else Token.Equal)
  | '<' -> add_token s (if matches s '=' then Token.Less_equal else Token.Less)
  | '>' -> add_token s (if matches s '=' then Token.Greater_equal else Token.Greater)
  | '/' ->
    if matches s '/'
    then line_comment s
    else add_token s (if matches s '=' then Token.Slash_equal else Token.Slash)
  | ' ' | '\r' | '\t' -> ()
  | '\n' -> newline s
  | '"' -> string_literal s
  | c ->
    if is_digit c
    then number s
    else if is_alpha c
    then identifier s
    else error s (Printf.sprintf "Unexpected character '%c'." c)

let scan_tokens source =
  let s =
    { source
    ; start = 0
    ; start_line = 1
    ; start_col = 1
    ; current = 0
    ; line = 1
    ; line_start = 0
    ; tokens = []
    ; errors = []
    }
  in
  while not (is_at_end s) do
    (* We are at the beginning of the next lexeme. *)
    s.start <- s.current;
    s.start_line <- s.line;
    s.start_col <- col_of s s.current;
    scan_token s
  done;
  let eof = Token.make Token.Eof ~lexeme:"" ~line:s.line ~col:(col_of s s.current) in
  match s.errors with
  | [] -> Ok (List.rev (eof :: s.tokens))
  | errors -> Error (List.rev errors)
