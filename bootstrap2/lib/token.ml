type token_type =
  (* Single-character tokens. *)
  | Left_paren
  | Right_paren
  | Left_brace
  | Right_brace
  | Comma
  | Minus
  | Plus
  | Semicolon
  | Slash
  | Star
  (* One or two character tokens. *)
  | Bang
  | Bang_equal
  | Equal
  | Equal_equal
  | Greater
  | Greater_equal
  | Less
  | Less_equal
  | Plus_plus
  | Minus_minus
  | Plus_equal
  | Minus_equal
  | Star_equal
  | Slash_equal
  (* Literals. *)
  | Identifier of string
  | String of string
  | Number of float
  (* Keywords. *)
  | And
  | Else
  | False
  | Fn
  | For
  | If
  | Or
  | Return
  | True
  | Var
  | While
  | Eof

type token =
  { token_type : token_type
  ; lexeme : string
  ; line : int (* 1-based *)
  ; col : int (* 1-based, counted from the start of `line` *)
  }

let make token_type ~lexeme ~line ~col = { token_type; lexeme; line; col }

let token_type_to_string = function
  | Left_paren -> "LEFT_PAREN"
  | Right_paren -> "RIGHT_PAREN"
  | Left_brace -> "LEFT_BRACE"
  | Right_brace -> "RIGHT_BRACE"
  | Comma -> "COMMA"
  | Minus -> "MINUS"
  | Plus -> "PLUS"
  | Semicolon -> "SEMICOLON"
  | Slash -> "SLASH"
  | Star -> "STAR"
  | Bang -> "BANG"
  | Bang_equal -> "BANG_EQUAL"
  | Equal -> "EQUAL"
  | Equal_equal -> "EQUAL_EQUAL"
  | Greater -> "GREATER"
  | Greater_equal -> "GREATER_EQUAL"
  | Less -> "LESS"
  | Less_equal -> "LESS_EQUAL"
  | Plus_plus -> "PLUS_PLUS"
  | Minus_minus -> "MINUS_MINUS"
  | Plus_equal -> "PLUS_EQUAL"
  | Minus_equal -> "MINUS_EQUAL"
  | Star_equal -> "STAR_EQUAL"
  | Slash_equal -> "SLASH_EQUAL"
  | Identifier _ -> "IDENTIFIER"
  | String _ -> "STRING"
  | Number _ -> "NUMBER"
  | And -> "AND"
  | Else -> "ELSE"
  | False -> "FALSE"
  | Fn -> "FN"
  | For -> "FOR"
  | If -> "IF"
  | Or -> "OR"
  | Return -> "RETURN"
  | True -> "TRUE"
  | Var -> "VAR"
  | While -> "WHILE"
  | Eof -> "EOF"

(* Numbers render like Java's Double.toString: integral values keep a ".0". *)
let number_to_string n =
  if Float.is_integer n then Printf.sprintf "%.1f" n else Printf.sprintf "%g" n

let literal_to_string = function
  | Identifier name -> name
  | String s -> s
  | Number n -> number_to_string n
  | _ -> "null"

let to_string { token_type; lexeme; line; col } =
  Printf.sprintf
    "%d:%d %s %s %s"
    line
    col
    (token_type_to_string token_type)
    lexeme
    (literal_to_string token_type)
