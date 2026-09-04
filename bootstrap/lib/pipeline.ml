(* [Compile] holds everything from `Desugar` on, because metaprocessing needs
   that much and cannot reach this module. What is left here happens on surface
   syntax: reading the files and running the meta blocks in them. *)

let ( let* ) = Result.bind

(* [on_code] sees the program once metaprocessing is done. *)
(* One package: its own units, loaded and metaprocessed. A dependency of it
   arrives as an artifact and is not read here. *)
let package ?(on_code = fun _ -> ()) ?roots ?entry_namespace ?seeds ~out path
  : (Ast.program * Artifact.unit_interface list, Diagnostic.error list) result
  =
  let* loaded, units =
    match Loader.package ?roots ?entry_namespace ?seeds path with
    | linked -> Ok linked
    | exception Loader.Failed e -> Diagnostic.one Diagnostic.Load e.Loader.span e.Loader.message
  in
  let* processed =
    match Metaprocess.program ~out loaded with
    | Ok processed -> Ok processed
    | Error e -> Diagnostic.one Diagnostic.Meta e.Metaprocess.span e.Metaprocess.message
  in
  on_code processed;
  Ok (processed, units)

let front ?on_code ?roots ~out path : (Ast.program, Diagnostic.error list) result =
  let* processed, _ = package ?on_code ?roots ~out path in
  Ok processed

let compile ?on_code ?on_types ?roots ~out path
  : (Ast.cps_stmt list, Diagnostic.error list) result
  =
  let* processed = front ?on_code ?roots ~out path in
  Compile.program ?on_types processed

let run = Compile.run
