(* A requirement is a set of acceptable versions, written in Cargo's DSL and
   normalized to a conjunction of comparators. *)

type op =
  | Ge
  | Gt
  | Le
  | Lt
  | Eq

type comparator =
  { op : op
  ; version : Version.t
  }

type t =
  { comparators : comparator list
  ; text : string
  }

let to_string r = r.text

let op_symbol = function
  | Ge -> ">="
  | Gt -> ">"
  | Le -> "<="
  | Lt -> "<"
  | Eq -> "="

let render r =
  String.concat
    ", "
    (List.map
       (fun c -> Printf.sprintf "%s%s" (op_symbol c.op) (Version.to_string c.version))
       r.comparators)

let without_pre (v : Version.t) = { v with Version.pre = []; build = None }
let bump_major (v : Version.t) = { v with Version.major = v.major + 1; minor = 0; patch = 0 }
let bump_minor (v : Version.t) = { v with Version.minor = v.minor + 1; patch = 0 }
let bump_patch (v : Version.t) = { v with Version.patch = v.patch + 1 }

let between lo hi = [ { op = Ge; version = lo }; { op = Lt; version = without_pre hi } ]

(* The upper bound bumps the leftmost field that was written and is not zero,
   because that is the one a break would change: `^1.4` allows `1.9` but not
   `2.0`, while `^0.4` allows neither `0.5` nor `1.0`. With every written field
   zero there is no such field, so the last one written is bumped instead. *)
let caret (v : Version.t) parts =
  let upper =
    if v.major > 0 || parts = 1
    then bump_major v
    else if v.minor > 0 || parts = 2
    then bump_minor v
    else bump_patch v
  in
  between v upper

(* `~1.4` pins the minor when one was written and the major when it was not. *)
let tilde (v : Version.t) parts =
  between v (if parts >= 2 then bump_minor v else bump_major v)

let parse_comparator text =
  let ( let* ) = Result.bind in
  let text = String.trim text in
  let starts prefix =
    String.length text >= String.length prefix
    && String.equal (String.sub text 0 (String.length prefix)) prefix
  in
  let rest n = String.trim (String.sub text n (String.length text - n)) in
  let bare op body =
    let* parsed = Version.parse_loose body in
    Ok [ { op; version = parsed.Version.version } ]
  in
  if String.equal text ""
  then Error "A requirement may not be empty."
  else if starts ">="
  then bare Ge (rest 2)
  else if starts "<="
  then bare Le (rest 2)
  else if starts ">"
  then bare Gt (rest 1)
  else if starts "<"
  then bare Lt (rest 1)
  else if starts "="
  then bare Eq (rest 1)
  else if starts "~"
  then
    let* parsed = Version.parse_loose (rest 1) in
    Ok (tilde parsed.Version.version parsed.Version.parts)
  else if starts "^"
  then
    let* parsed = Version.parse_loose (rest 1) in
    Ok (caret parsed.Version.version parsed.Version.parts)
  else
    let* parsed = Version.parse_loose text in
    Ok (caret parsed.Version.version parsed.Version.parts)

let of_string text =
  let ( let* ) = Result.bind in
  let parts = String.split_on_char ',' text in
  let* comparators =
    List.fold_left
      (fun acc part ->
        let* acc = acc in
        let* cs = parse_comparator part in
        Ok (acc @ cs))
      (Ok [])
      parts
  in
  Ok { comparators; text }

(* A pre-release only satisfies a requirement that names one at the same
   major.minor.patch, which is what keeps `^1.0` from selecting `2.0.0-rc1`. *)
let allows_pre r (v : Version.t) =
  List.is_empty v.pre
  || List.exists
       (fun c ->
         (not (List.is_empty c.version.Version.pre))
         && c.version.Version.major = v.major
         && c.version.Version.minor = v.minor
         && c.version.Version.patch = v.patch)
       r.comparators

let satisfies r v =
  allows_pre r v
  && List.for_all
       (fun c ->
         let n = Version.compare v c.version in
         match c.op with
         | Ge -> n >= 0
         | Gt -> n > 0
         | Le -> n <= 0
         | Lt -> n < 0
         | Eq -> n = 0)
       r.comparators
