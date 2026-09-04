(* The subset of TOML a manifest uses. Values carry spans because a manifest
   diagnostic has to point at the key that is wrong, the way every other
   diagnostic in this project points at source. *)

open Bootstrap

type value =
  | String of string
  | Int of int
  | Bool of bool
  | Array of node list
  | Table of entry list

and node =
  { value : value
  ; span : Source_map.Span.t
  }

and entry =
  { key : string
  ; key_span : Source_map.Span.t
  ; node : node
  }

type error =
  { span : Source_map.Span.t
  ; message : string
  }

exception Failed of error

type state =
  { file : Source_map.File.t
  ; text : string
  ; mutable pos : int
  }

let span st ~lo ~hi = Source_map.Span.of_range st.file ~lo ~hi

let fail st ~lo ~hi fmt =
  Printf.ksprintf (fun message -> raise (Failed { span = span st ~lo ~hi; message })) fmt

let peek st = if st.pos < String.length st.text then Some st.text.[st.pos] else None
let advance st = st.pos <- st.pos + 1

let is_bare_key = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '-' -> true
  | _ -> false

let rec skip_trivia st =
  match peek st with
  | Some (' ' | '\t' | '\r' | '\n') ->
    advance st;
    skip_trivia st
  | Some '#' ->
    while
      match peek st with
      | Some '\n' | None -> false
      | Some _ -> true
    do
      advance st
    done;
    skip_trivia st
  | _ -> ()

(* Inside a line, so that a value ends where its line does. *)
let rec skip_inline st =
  match peek st with
  | Some (' ' | '\t' | '\r') ->
    advance st;
    skip_inline st
  | Some '#' ->
    while
      match peek st with
      | Some '\n' | None -> false
      | Some _ -> true
    do
      advance st
    done
  | _ -> ()

let expect st c =
  skip_trivia st;
  match peek st with
  | Some found when Char.equal found c -> advance st
  | Some found -> fail st ~lo:st.pos ~hi:(st.pos + 1) "Expected '%c', found '%c'." c found
  | None -> fail st ~lo:st.pos ~hi:st.pos "Expected '%c', found end of file." c

let quoted_string st =
  let lo = st.pos in
  let quote = match peek st with Some q -> q | None -> assert false in
  advance st;
  let buf = Buffer.create 16 in
  let rec loop () =
    match peek st with
    | None -> fail st ~lo ~hi:st.pos "Unterminated string."
    | Some '\n' -> fail st ~lo ~hi:st.pos "A string may not span a line."
    | Some c when Char.equal c quote ->
      advance st;
      Buffer.contents buf
    | Some '\\' when Char.equal quote '"' ->
      advance st;
      (match peek st with
       | Some 'n' -> Buffer.add_char buf '\n'
       | Some 't' -> Buffer.add_char buf '\t'
       | Some 'r' -> Buffer.add_char buf '\r'
       | Some '"' -> Buffer.add_char buf '"'
       | Some '\\' -> Buffer.add_char buf '\\'
       | Some c -> fail st ~lo:(st.pos - 1) ~hi:(st.pos + 1) "Unknown escape '\\%c'." c
       | None -> fail st ~lo ~hi:st.pos "Unterminated string.");
      advance st;
      loop ()
    | Some c ->
      Buffer.add_char buf c;
      advance st;
      loop ()
  in
  let text = loop () in
  text, lo

let bare_key st =
  let lo = st.pos in
  while
    match peek st with
    | Some c -> is_bare_key c
    | None -> false
  do
    advance st
  done;
  if st.pos = lo
  then
    fail
      st
      ~lo
      ~hi:(lo + 1)
      "Expected a key.%s"
      (match peek st with Some c -> Printf.sprintf " Found '%c'." c | None -> "");
  String.sub st.text lo (st.pos - lo), lo

let key st =
  skip_trivia st;
  match peek st with
  | Some ('"' | '\'') ->
    let text, lo = quoted_string st in
    text, lo, st.pos
  | _ ->
    let text, lo = bare_key st in
    text, lo, st.pos

(* Dotted keys name a path into nested tables: `a.b = 1` and `[a.b]`. *)
let dotted_key st =
  let rec loop acc =
    let text, lo, hi = key st in
    let acc = (text, lo, hi) :: acc in
    skip_inline st;
    match peek st with
    | Some '.' ->
      advance st;
      loop acc
    | _ -> List.rev acc
  in
  loop []

let number st =
  let lo = st.pos in
  (match peek st with
   | Some ('+' | '-') -> advance st
   | _ -> ());
  while
    match peek st with
    | Some ('0' .. '9' | '_') -> true
    | _ -> false
  do
    advance st
  done;
  let text = String.sub st.text lo (st.pos - lo) in
  let digits = String.concat "" (String.split_on_char '_' text) in
  match int_of_string_opt digits with
  | Some n -> Int n, lo
  | None -> fail st ~lo ~hi:st.pos "'%s' is not a number." text

let rec value st =
  skip_trivia st;
  let lo = st.pos in
  match peek st with
  | None -> fail st ~lo ~hi:lo "Expected a value, found end of file."
  | Some ('"' | '\'') ->
    let text, lo = quoted_string st in
    { value = String text; span = span st ~lo ~hi:st.pos }
  | Some '[' -> array st
  | Some '{' -> inline_table st
  | Some ('0' .. '9' | '+' | '-') ->
    let v, lo = number st in
    { value = v; span = span st ~lo ~hi:st.pos }
  | Some _ ->
    let text, lo = bare_key st in
    (match text with
     | "true" -> { value = Bool true; span = span st ~lo ~hi:st.pos }
     | "false" -> { value = Bool false; span = span st ~lo ~hi:st.pos }
     | "" -> fail st ~lo ~hi:(lo + 1) "Expected a value."
     | other -> fail st ~lo ~hi:st.pos "Expected a value, found '%s'." other)

and array st =
  let lo = st.pos in
  expect st '[';
  let rec loop acc =
    skip_trivia st;
    match peek st with
    | Some ']' ->
      advance st;
      List.rev acc
    | None -> fail st ~lo ~hi:st.pos "Unterminated array."
    | Some _ ->
      let item = value st in
      skip_trivia st;
      (match peek st with
       | Some ',' ->
         advance st;
         loop (item :: acc)
       | Some ']' ->
         advance st;
         List.rev (item :: acc)
       | Some c -> fail st ~lo:st.pos ~hi:(st.pos + 1) "Expected ',' or ']', found '%c'." c
       | None -> fail st ~lo ~hi:st.pos "Unterminated array.")
  in
  let items = loop [] in
  { value = Array items; span = span st ~lo ~hi:st.pos }

and inline_table st =
  let lo = st.pos in
  expect st '{';
  let rec loop acc =
    skip_trivia st;
    match peek st with
    | Some '}' ->
      advance st;
      List.rev acc
    | None -> fail st ~lo ~hi:st.pos "Unterminated table."
    | Some _ ->
      let name, klo, khi = key st in
      skip_inline st;
      expect st '=';
      let node = value st in
      let entry = { key = name; key_span = span st ~lo:klo ~hi:khi; node } in
      skip_trivia st;
      (match peek st with
       | Some ',' ->
         advance st;
         loop (entry :: acc)
       | Some '}' ->
         advance st;
         List.rev (entry :: acc)
       | Some c -> fail st ~lo:st.pos ~hi:(st.pos + 1) "Expected ',' or '}', found '%c'." c
       | None -> fail st ~lo ~hi:st.pos "Unterminated table.")
  in
  let entries = loop [] in
  { value = Table entries; span = span st ~lo ~hi:st.pos }

let parse file =
  let text = Source_map.File.text file in
  let st = { file; text; pos = 0 } in
  let root : (string list * entry list ref) list ref = ref [ [], ref [] ] in
  let current = ref [] in
  let declared = ref [] in
  (* Where a table was written, so that a diagnostic about the table points at
     its header rather than at whichever key happens to come first. *)
  let headers : (string list * Source_map.Span.t) list ref = ref [] in
  (* A header names its ancestors into existence too, so `[a.b]` alone still
     produces an `a` for the top level to adopt. *)
  let rec table_for path =
    match List.assoc_opt path !root with
    | Some entries -> entries
    | None ->
      (match List.rev path with
       | _ :: rev_parent -> ignore (table_for (List.rev rev_parent))
       | [] -> ());
      let entries = ref [] in
      root := !root @ [ path, entries ];
      entries
  in
  let rec loop () =
    skip_trivia st;
    match peek st with
    | None -> ()
    | Some '[' ->
      let lo = st.pos in
      advance st;
      let names = dotted_key st in
      skip_inline st;
      expect st ']';
      let path = List.map (fun (n, _, _) -> n) names in
      if List.mem_assoc path !declared
      then fail st ~lo ~hi:st.pos "Table '%s' is defined twice." (String.concat "." path);
      declared := (path, ()) :: !declared;
      headers := (path, span st ~lo ~hi:st.pos) :: !headers;
      ignore (table_for path);
      current := path;
      loop ()
    | Some _ ->
      let names = dotted_key st in
      skip_inline st;
      expect st '=';
      let node = value st in
      let name, klo, khi =
        match List.rev names with
        | last :: _ -> last
        | [] -> assert false
      in
      let prefix = List.rev (List.tl (List.rev (List.map (fun (n, _, _) -> n) names))) in
      let entries = table_for (!current @ prefix) in
      if List.exists (fun e -> String.equal e.key name) !entries
      then fail st ~lo:klo ~hi:khi "'%s' is defined twice." name;
      entries := !entries @ [ { key = name; key_span = span st ~lo:klo ~hi:khi; node } ];
      skip_inline st;
      (match peek st with
       | Some '\n' | None -> loop ()
       | Some c -> fail st ~lo:st.pos ~hi:(st.pos + 1) "Expected a newline, found '%c'." c)
  in
  match loop () with
  | () ->
    (* Deeper paths first, so a table is complete before its parent adopts it. *)
    let paths =
      List.sort
        (fun (a, _) (b, _) -> compare (List.length b) (List.length a))
        (List.filter (fun (path, _) -> not (List.is_empty path)) !root)
    in
    let built = Hashtbl.create 8 in
    List.iter
      (fun (path, entries) ->
        let entries =
          List.map
            (fun e ->
              match Hashtbl.find_opt built (path @ [ e.key ]) with
              | Some node -> { e with node }
              | None -> e)
            !entries
        in
        let child =
          match Hashtbl.find_opt built path with
          | Some { value = Table existing; span } ->
            { value = Table (existing @ entries); span }
          | _ ->
            { value = Table entries
            ; span =
                (match List.assoc_opt path !headers, entries with
                 | Some span, _ -> span
                 | None, e :: _ -> e.key_span
                 | None, [] -> Source_map.Span.nowhere)
            }
        in
        Hashtbl.replace built path child;
        match List.rev path with
        | name :: rev_parent ->
          let parent = List.rev rev_parent in
          let existing =
            match Hashtbl.find_opt built parent with
            | Some { value = Table entries; _ } -> entries
            | _ -> []
          in
          let entry = { key = name; key_span = child.span; node = child } in
          Hashtbl.replace
            built
            parent
            { value =
                Table
                  (List.filter (fun e -> not (String.equal e.key name)) existing @ [ entry ])
            ; span = child.span
            }
        | [] -> ())
      paths;
    let top = match List.assoc_opt [] !root with Some e -> !e | None -> [] in
    let nested =
      match Hashtbl.find_opt built [] with
      | Some { value = Table entries; _ } -> entries
      | _ -> []
    in
    Ok (top @ nested)
  | exception Failed e -> Error e

let find entries key = List.find_opt (fun e -> String.equal e.key key) entries

let kind = function
  | String _ -> "a string"
  | Int _ -> "an integer"
  | Bool _ -> "a boolean"
  | Array _ -> "an array"
  | Table _ -> "a table"
