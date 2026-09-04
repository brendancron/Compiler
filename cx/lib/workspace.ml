(* What the compiler is allowed to reach, worked out from the manifest. `cx`
   resolves; the compiler consumes. *)

open Bootstrap

let dependency_roots (m : Manifest.t) =
  List.map
    (fun (d : Manifest.dependency) ->
      match d.Manifest.source with
      | Manifest.Path path ->
        d.Manifest.name, Loader.from_source (Filename.concat m.Manifest.root path))
    m.Manifest.dependencies

let roots_for entry =
  match Manifest.find_root entry with
  | None -> Ok (Driver.roots_for entry, None)
  | Some root ->
    let path = Filename.concat root Manifest.file_name in
    (match Manifest.load path with
     | Error errors -> Error errors
     | Ok manifest ->
       Ok
         ( { Loader.package = root; std = Toolchain.stdlib (); deps = dependency_roots manifest }
         , Some manifest ))

(* The entry a package runs: `src/main.cx`, and `src/lib.cx` for a library that
   has no other. *)
let entry_of root =
  let candidate name = Filename.concat root (Filename.concat "src" name) in
  if Sys.file_exists (candidate "main.cx")
  then Some (candidate "main.cx")
  else if Sys.file_exists (candidate "lib.cx")
  then Some (candidate "lib.cx")
  else None
