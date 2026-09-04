(* Building a package: its dependencies first, each to an artifact, then the
   package itself against those artifacts rather than against their source. *)

open Bootstrap

let target_dir root = Filename.concat root "target"
let debug_dir root = Filename.concat (target_dir root) "debug"
let artifact_path root name = Filename.concat (debug_dir root) (name ^ Artifact.extension)

let ensure dir = if not (Sys.file_exists dir) then Sys.mkdir dir 0o755

(* Every source file in the package, so the artifact holds all of it. *)
let sources root =
  let rec walk dir =
    Sys.readdir dir
    |> Array.to_list
    |> List.sort String.compare
    |> List.concat_map (fun entry ->
      let path = Filename.concat dir entry in
      if Sys.is_directory path
      then walk path
      else if Filename.check_suffix entry ".cx"
      then [ path ]
      else [])
  in
  let src = Filename.concat root "src" in
  if Sys.file_exists src && Sys.is_directory src then walk src else []

let modified path = try (Unix.stat path).Unix.st_mtime with _ -> 0.0

let manifest_of root =
  Manifest.load (Filename.concat root Manifest.file_name)

let missing root =
  [ Diagnostic.at
      Diagnostic.Manifest
      Source_map.Span.nowhere
      (Printf.sprintf "'%s' has no src/main.cx or src/lib.cx." root)
  ]

(* Depth first, so a dependency is compiled before whoever needs it, and a
   package reached twice through a diamond is compiled once. *)
let rec compile ~out ~built root : (Artifact.t list, Diagnostic.error list) result =
  let ( let* ) = Result.bind in
  let root = if Filename.is_relative root then Filename.concat (Sys.getcwd ()) root else root in
  let* manifest = manifest_of root in
  match Hashtbl.find_opt built manifest.Manifest.name with
  | Some existing -> Ok existing
  | None ->
    let* dependencies =
      List.fold_left
        (fun acc (d : Manifest.dependency) ->
          let* acc = acc in
          match d.Manifest.source with
          | Manifest.Path path ->
            let* transitive = compile ~out ~built (Filename.concat root path) in
            Ok (acc @ transitive))
        (Ok [])
        manifest.Manifest.dependencies
    in
    let compiled name =
      List.find_opt (fun (a : Artifact.t) -> String.equal a.Artifact.package name) dependencies
    in
    let deps =
      List.map
        (fun (d : Manifest.dependency) ->
          match d.Manifest.source with
          | Manifest.Path path ->
            ( d.Manifest.name
            , { Loader.dep_root = Filename.concat root path
              ; compiled = compiled d.Manifest.name
              } ))
        manifest.Manifest.dependencies
    in
    let roots = { Loader.package = root; std = Toolchain.stdlib (); deps } in
    let path = artifact_path root manifest.Manifest.name in
    (* Crude, and deliberately so: whether an artifact is still good is the
       build cache's question, and the cache keys on an input hash rather than
       on a clock. Until it exists, an artifact older than an input it was
       built from is rebuilt. *)
    let fresh =
      match Artifact.load path with
      | Error _ -> None
      | Ok artifact ->
        let built = modified path in
        let inputs =
          (Filename.concat root Manifest.file_name :: sources root)
          @ List.map
              (fun (d : Artifact.t) -> artifact_path root d.Artifact.package)
              dependencies
        in
        if List.for_all (fun input -> modified input <= built) inputs
        then Some artifact
        else None
    in
    (match fresh with
     | Some artifact ->
       let all = dependencies @ [ artifact ] in
       Hashtbl.replace built manifest.Manifest.name all;
       Ok all
     | None ->
       (match Workspace.entry_of root with
        | None -> Error (missing root)
        | Some entry ->
       let* program, units =
         Pipeline.package
           ~roots
           ~entry_namespace:manifest.Manifest.name
           ~seeds:(sources root)
           ~out
           entry
       in
       let artifact =
         { Artifact.compiler = Release.version
         ; package = manifest.Manifest.name
         ; units
         ; program
         }
       in
       ensure (target_dir root);
       ensure (debug_dir root);
       Artifact.save path artifact;
       let all = dependencies @ [ artifact ] in
       Hashtbl.replace built manifest.Manifest.name all;
       Ok all))

let package ~out root = compile ~out ~built:(Hashtbl.create 8) root

(* Each package embeds whatever of the standard library it imported, since the
   library is not itself compiled to an artifact yet. Two of them embedding the
   same module declare it twice, so the link keeps the first of each name. *)
let link (artifacts : Artifact.t list) =
  let seen = Hashtbl.create 256 in
  List.concat_map (fun (a : Artifact.t) -> a.Artifact.program) artifacts
  |> List.filter (fun (s : Ast.stmt) ->
    match Loader.declared_name s with
    | None -> true
    | Some name ->
      if Hashtbl.mem seen name
      then false
      else (
        Hashtbl.replace seen name ();
        true))
