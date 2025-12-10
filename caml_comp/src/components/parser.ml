open Models.Ast
open Models.Token

exception Parse_error

let parse tokens =
  let pos = ref 0 in
  let peek () = List.nth tokens !pos in
  let advance () = incr pos in
  let consume expected =
    match peek () with
    | t when t = expected -> advance ()
    | _ -> failwith "unexpected token"
  in

  let rec parse_factor () =
    match peek () with
    | NUMBER n ->
        advance ();
        Int n
    | LEFT_PAREN ->
        advance ();
        let e = parse_expr () in
        consume RIGHT_PAREN;
        e
    | _ ->
        failwith "expected number or '('"
  
  and parse_term () =
    let left = ref (parse_factor ()) in
    let rec loop () =
      match peek () with
      | STAR ->
          advance ();
          let right = parse_factor () in
          left := Mult (!left, right);
          loop ()
      | SLASH ->
          advance ();
          let right = parse_factor () in
          left := Div (!left, right);
          loop ()
      | _ ->
          !left
    in
    loop ()
  
  and parse_expr () =
    let left = ref (parse_term ()) in
    let rec loop () =
      match peek () with
      | PLUS ->
          advance ();
          let right = parse_term () in
          left := Add (!left, right);
          loop ()
      | MINUS ->
          advance ();
          let right = parse_term () in
          left := Sub (!left, right);
          loop ()
      | _ ->
          !left
    in
    loop ()
  in

  parse_expr ()
