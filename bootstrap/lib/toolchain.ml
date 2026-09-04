(* Where the standard library is. It ships inside the toolchain rather than as
   a package, so `import "std/…"` resolves here and never through the
   filesystem the program happens to sit in. *)

let marker = Filename.concat "stdlib" "lang"

let up_from dir =
  let rec up dir =
    if Sys.file_exists (Filename.concat dir marker)
    then Some (Filename.concat dir "stdlib")
    else (
      let parent = Filename.dirname dir in
      if String.equal parent dir then None else up parent)
  in
  up dir

let stdlib () =
  match Sys.getenv_opt "CRONYX_STDLIB" with
  | Some dir -> Some dir
  | None ->
    (match up_from (Sys.getcwd ()) with
     | Some dir -> Some dir
     | None -> up_from (Filename.dirname Sys.executable_name))
