type token_type =
  (* Single-character tokens. *)
  | Left_paren
  | Right_paren
  | Left_brace
  | Right_brace
  | Left_bracket
  | Right_bracket
  | Comma
  | Minus
  | Plus
  | Semicolon
  | Slash
  | Star
  | Colon
  | Colon_colon
  | Dot
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
  | Arrow
  (* Literals. *)
  | Identifier of string
  | String of string
  | Int of int
  | Float of float
  (* Keywords. *)
  | And
  | Ctl
  | Effect
  | Else
  | False
  | Fn
  | For
  | If
  | Or
  | Handle
  | Handler
  | Resume
  | Return
  | Run
  | Match
  | New
  | True
  | Type
  | Typeof
  | With
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
  | Left_bracket -> "LEFT_BRACKET"
  | Right_bracket -> "RIGHT_BRACKET"
  | Comma -> "COMMA"
  | Minus -> "MINUS"
  | Plus -> "PLUS"
  | Semicolon -> "SEMICOLON"
  | Slash -> "SLASH"
  | Star -> "STAR"
  | Colon -> "COLON"
  | Colon_colon -> "COLON_COLON"
  | Dot -> "DOT"
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
  | Arrow -> "ARROW"
  | Identifier _ -> "IDENTIFIER"
  | String _ -> "STRING"
  | Int _ -> "INT"
  | Float _ -> "FLOAT"
  | And -> "AND"
  | Ctl -> "CTL"
  | Effect -> "EFFECT"
  | Else -> "ELSE"
  | False -> "FALSE"
  | Fn -> "FN"
  | For -> "FOR"
  | If -> "IF"
  | Or -> "OR"
  | Handle -> "HANDLE"
  | Handler -> "HANDLER"
  | Resume -> "RESUME"
  | Return -> "RETURN"
  | Run -> "RUN"
  | Match -> "MATCH"
  | New -> "NEW"
  | True -> "TRUE"
  | Type -> "TYPE"
  | Typeof -> "TYPEOF"
  | With -> "WITH"
  | Var -> "VAR"
  | While -> "WHILE"
  | Eof -> "EOF"

(* Floats keep a visible fractional part so they never read as ints. *)
let float_to_string n =
  if Float.is_integer n then Printf.sprintf "%.1f" n else Printf.sprintf "%g" n

let literal_to_string = function
  | Identifier name -> name
  | String s -> s
  | Int n -> string_of_int n
  | Float n -> float_to_string n
  | _ -> "null"

let to_string { token_type; lexeme; line; col } =
  Printf.sprintf
    "%d:%d %s %s %s"
    line
    col
    (token_type_to_string token_type)
    lexeme
    (literal_to_string token_type)
