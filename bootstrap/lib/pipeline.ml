(* [Compile] holds everything from `Desugar` on, because metaprocessing needs
   that much and cannot reach this module. What is left here happens on surface
   syntax: reading the files and running the meta blocks in them. *)

let ( let* ) = Result.bind

(* [on_code] sees the program once metaprocessing is done. *)
let front ?(on_code = fun _ -> ()) ?roots ~out path
  : (Ast.program, Diagnostic.error list) result
  =
  let* loaded =
    match Loader.program ?roots path with
    | linked -> Ok linked
    | exception Loader.Failed e -> Diagnostic.one Diagnostic.Load e.Loader.span e.Loader.message
  in
  let* processed =
    match Metaprocess.program ~out loaded with
    | Ok processed -> Ok processed
    | Error e -> Diagnostic.one Diagnostic.Meta e.Metaprocess.span e.Metaprocess.message
  in
  on_code processed;
  Ok processed

let compile ?on_code ?on_types ?roots ~out path
  : (Ast.cps_stmt list, Diagnostic.error list) result
  =
  let* processed = front ?on_code ?roots ~out path in
  Compile.program ?on_types processed

let run = Compile.run
