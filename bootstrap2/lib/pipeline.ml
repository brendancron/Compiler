(* The whole compiler, from a path to a program the interpreter can run.

   [Compile] holds everything from `Desugar` on, because metaprocessing needs
   that much and cannot reach this module — it is what runs a meta block. What
   is left here is the part that happens on surface syntax: reading the files
   and running the meta blocks in them. *)

let ( let* ) = Result.bind

(* [on_code] sees the program once metaprocessing is done, which is what a `gen`
   produced turned into and what `--dump-code` prints. *)
let front ?(on_code = fun _ -> ()) ~out path
  : (Ast.program, Diagnostic.error list) result
  =
  let* loaded =
    match Loader.program path with
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

let compile ?on_code ?on_types ~out path
  : (Ast.cps_stmt list, Diagnostic.error list) result
  =
  let* processed = front ?on_code ~out path in
  Compile.program ?on_types processed

let run = Compile.run
