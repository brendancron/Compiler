(* The one part of a manifest that a `cx` of any age has to be able to read.

   Dispatch happens before anything knows which compiler should be running, so
   this reader has to work on a manifest written years after it. That is what
   fixes the shape: `cronyx` is a top-level string under `[package]`, never
   nested, never conditional, never computed, and everything else in the file is
   ignored rather than parsed. A `cx` that cannot satisfy what it finds says so
   by naming the version; it never fails with a syntax error about a file it was
   not meant to understand. *)

let trim = String.trim

let unquote text =
  let n = String.length text in
  if n >= 2
     && ((Char.equal text.[0] '"' && Char.equal text.[n - 1] '"')
         || (Char.equal text.[0] '\'' && Char.equal text.[n - 1] '\''))
  then Some (String.sub text 1 (n - 2))
  else None

(* A `#` inside a string is not a comment, and a version is a string. *)
let without_comment line =
  let buffer = Buffer.create (String.length line) in
  let quote = ref None in
  (try
     String.iter
       (fun c ->
         match !quote, c with
         | None, '#' -> raise Exit
         | None, ('"' | '\'') ->
           quote := Some c;
           Buffer.add_char buffer c
         | Some q, c when Char.equal q c ->
           quote := None;
           Buffer.add_char buffer c
         | _ -> Buffer.add_char buffer c)
       line
   with
   | Exit -> ());
  Buffer.contents buffer

let key_value line =
  match String.index_opt line '=' with
  | None -> None
  | Some i ->
    Some
      ( trim (String.sub line 0 i)
      , trim (String.sub line (i + 1) (String.length line - i - 1)) )

let header line =
  let line = trim line in
  let n = String.length line in
  if n >= 2 && Char.equal line.[0] '[' && Char.equal line.[n - 1] ']'
  then Some (trim (String.sub line 1 (n - 2)))
  else None

(* The version a manifest asks for, or nothing. Never an error: a file this
   cannot make sense of is a file for a newer compiler to read. *)
let required text =
  let rec scan table = function
    | [] -> None
    | line :: rest ->
      let line = without_comment line in
      (match header line with
       | Some name -> scan name rest
       | None ->
         (match key_value line with
          | Some (key, value)
            when (String.equal table "package" && String.equal key "cronyx")
                 || String.equal key "package.cronyx" ->
            (match unquote value with
             | Some version -> Some version
             | None -> scan table rest)
          | _ -> scan table rest))
  in
  scan "" (String.split_on_char '\n' text)

let of_file path =
  match In_channel.with_open_bin path In_channel.input_all with
  | text -> required text
  | exception Sys_error _ -> None
