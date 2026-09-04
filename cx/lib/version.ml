(* Semantic versioning. A build suffix is read and then ignored, which is what
   the specification asks for; a pre-release suffix orders below the release it
   precedes. *)

type t =
  { major : int
  ; minor : int
  ; patch : int
  ; pre : string list
  ; build : string option
  }

let to_string v =
  Printf.sprintf
    "%d.%d.%d%s%s"
    v.major
    v.minor
    v.patch
    (match v.pre with [] -> "" | ids -> "-" ^ String.concat "." ids)
    (match v.build with None -> "" | Some b -> "+" ^ b)

let is_digits s =
  (not (String.equal s "")) && String.for_all (function '0' .. '9' -> true | _ -> false) s

let number_of_string what s =
  if String.equal s ""
  then Error (Printf.sprintf "A version needs a %s." what)
  else if not (is_digits s)
  then Error (Printf.sprintf "'%s' is not a %s." s what)
  else if String.length s > 1 && Char.equal s.[0] '0'
  then Error (Printf.sprintf "'%s' has a leading zero." s)
  else (
    match int_of_string_opt s with
    | Some n -> Ok n
    | None -> Error (Printf.sprintf "'%s' does not fit in a number." s))

let split_once text c =
  match String.index_opt text c with
  | None -> text, None
  | Some i ->
    ( String.sub text 0 i
    , Some (String.sub text (i + 1) (String.length text - i - 1)) )

(* [parts] is how many of major.minor.patch were written, which a requirement
   needs and a version does not: `^1.4` and `^1.4.0` denote different sets. *)
type parsed =
  { version : t
  ; parts : int
  }

let parse_loose text =
  let ( let* ) = Result.bind in
  let core, build = split_once text '+' in
  let core, pre = split_once core '-' in
  let* pre =
    match pre with
    | None -> Ok []
    | Some pre ->
      let ids = String.split_on_char '.' pre in
      if List.exists (String.equal "") ids
      then Error "A pre-release identifier may not be empty."
      else Ok ids
  in
  match String.split_on_char '.' core with
  | [] -> Error "A version needs a major number."
  | fields when List.length fields > 3 ->
    Error (Printf.sprintf "'%s' has more than three numbers." core)
  | fields ->
    let field what i =
      match List.nth_opt fields i with
      | None -> Ok 0
      | Some s -> number_of_string what s
    in
    let* major = field "major number" 0 in
    let* minor = field "minor number" 1 in
    let* patch = field "patch number" 2 in
    Ok { version = { major; minor; patch; pre; build }; parts = List.length fields }

let of_string text =
  let ( let* ) = Result.bind in
  let* parsed = parse_loose text in
  if parsed.parts <> 3
  then Error (Printf.sprintf "'%s' needs all of major.minor.patch." text)
  else Ok parsed.version

let compare_pre_id a b =
  match is_digits a, is_digits b with
  | true, true -> Int.compare (int_of_string a) (int_of_string b)
  | true, false -> -1
  | false, true -> 1
  | false, false -> String.compare a b

let rec compare_pre a b =
  match a, b with
  | [], [] -> 0
  | [], _ :: _ -> -1
  | _ :: _, [] -> 1
  | x :: xs, y :: ys ->
    (match compare_pre_id x y with
     | 0 -> compare_pre xs ys
     | n -> n)

(* A pre-release orders below the release it precedes, and a build suffix does
   not participate at all. *)
let compare a b =
  match Int.compare a.major b.major with
  | 0 ->
    (match Int.compare a.minor b.minor with
     | 0 ->
       (match Int.compare a.patch b.patch with
        | 0 ->
          (match a.pre, b.pre with
           | [], [] -> 0
           | [], _ :: _ -> 1
           | _ :: _, [] -> -1
           | x, y -> compare_pre x y)
        | n -> n)
     | n -> n)
  | n -> n

let equal a b = compare a b = 0
