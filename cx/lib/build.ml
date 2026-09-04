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
let profile = "debug"

(* [compiled] names the packages this build actually ran the compiler over, so
   that "nothing to do" is something a caller can see rather than infer from a
   clock. *)
let rec compile ~out ~built ~compiled root : (Artifact.t list, Diagnostic.error list) result =
  let ( let* ) = Result.bind in
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
            let* transitive = compile ~out ~built ~compiled (Filename.concat root path) in
            Ok (acc @ transitive))
        (Ok [])
        manifest.Manifest.dependencies
    in
    let artifact_for name =
      List.find_opt (fun (a : Artifact.t) -> String.equal a.Artifact.package name) dependencies
    in
    let deps =
      List.map
        (fun (d : Manifest.dependency) ->
          match d.Manifest.source with
          | Manifest.Path path ->
            ( d.Manifest.name
            , { Loader.dep_root = Filename.concat root path
              ; compiled = artifact_for d.Manifest.name
              } ))
        manifest.Manifest.dependencies
    in
    let roots = { Loader.package = root; std = Toolchain.stdlib (); deps } in
    let path = artifact_path root manifest.Manifest.name in
    let dependency_prints =
      List.map (fun (d : Artifact.t) -> d.Artifact.fingerprint) dependencies
    in
    (* An artifact is still good when everything it read still hashes to what it
       hashed then, and when the files it would read now are the same ones. The
       second half is what catches a source file added since: its own digest
       would be missing from the list rather than different. *)
    let fresh =
      match Artifact.load path with
      | Error _ -> None
      | Ok artifact ->
        let declared =
          Filename.concat root Manifest.file_name :: sources root |> List.sort String.compare
        in
        let recorded = List.map (fun (i : Artifact.input) -> i.Artifact.path) artifact.Artifact.inputs in
        let unchanged =
          List.for_all
            (fun (i : Artifact.input) ->
              match Artifact.digest_of i.Artifact.path with
              | Some digest -> String.equal digest i.Artifact.digest
              | None -> false)
            artifact.Artifact.inputs
        in
        let same_files =
          List.for_all (fun declared -> List.mem declared recorded) declared
        in
        let same_graph =
          String.equal
            artifact.Artifact.fingerprint
            (Artifact.fingerprint_of
               ~compiler:Release.version
               ~profile
               ~inputs:artifact.Artifact.inputs
               ~dependencies:dependency_prints)
        in
        if unchanged && same_files && same_graph then Some artifact else None
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
       Inputs.reset ();
       let* program, units =
         Pipeline.package
           ~roots
           ~entry_namespace:manifest.Manifest.name
           ~seeds:(sources root)
           ~out
           entry
       in
       let inputs =
         Artifact.inputs_of
           ((Filename.concat root Manifest.file_name :: sources root) @ Inputs.taken ())
       in
       let artifact =
         { Artifact.compiler = Release.version
         ; package = manifest.Manifest.name
         ; units
         ; program
         ; inputs
         ; fingerprint =
             Artifact.fingerprint_of
               ~compiler:Release.version
               ~profile
               ~inputs
               ~dependencies:dependency_prints
         }
       in
       ensure (target_dir root);
       ensure (debug_dir root);
       Artifact.save path artifact;
       compiled := manifest.Manifest.name :: !compiled;
       let all = dependencies @ [ artifact ] in
       Hashtbl.replace built manifest.Manifest.name all;
       Ok all))

(* Built from the package root, so every path an artifact carries is relative to
   it. Two copies of one tree then compile to the same bytes, which is what
   makes an artifact a function of its inputs rather than of its address. *)
let package ~out root =
  let compiled = ref [] in
  let here = Sys.getcwd () in
  Sys.chdir root;
  Fun.protect
    ~finally:(fun () -> Sys.chdir here)
    (fun () ->
      match compile ~out ~built:(Hashtbl.create 8) ~compiled "." with
      | Error errors -> Error errors
      | Ok artifacts -> Ok (artifacts, List.rev !compiled))

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
