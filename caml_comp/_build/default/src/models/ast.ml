type _ expr =
  | Int: int -> int expr
  | String: string -> string expr
  | Add: int expr * int expr -> int expr
  | Sub : int expr * int expr -> int expr
  | Mult : int expr * int expr -> int expr
  | Div : int expr * int expr -> int expr

let rec string_of_expr = function
  | Int n ->
      "Int(" ^ string_of_int n ^ ")"

  | Add (l, r) ->
      "Add(" ^ string_of_expr l ^ ", " ^ string_of_expr r ^ ")"

  | Sub (l, r) ->
      "Sub(" ^ string_of_expr l ^ ", " ^ string_of_expr r ^ ")"

  | Mult (l, r) ->
      "Mult(" ^ string_of_expr l ^ ", " ^ string_of_expr r ^ ")"

  | Div (l, r) ->
      "Div(" ^ string_of_expr l ^ ", " ^ string_of_expr r ^ ")"

