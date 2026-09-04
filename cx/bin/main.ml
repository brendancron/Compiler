open Bootstrap

let usage =
  "usage: cx <command> [options]\n\n\
  \  new <name>      create a package skeleton\n\
  \  build           compile the package here, and its dependencies\n\
  \  run [file.cx]   compile and execute a program, or the package here\n\n\
   options for `run`:\n\
  \  --dump-source   echo the source before running\n\
  \  --dump-tokens   print the token stream\n\
  \  --dump-ast      print the parsed AST\n\
  \  --dump-types    print the type-checked AST\n\
  \  --dump-code     print the program as Cronyx, after metaprocessing\n\n\
  \  -h, --help      show this message"

(* Flags may appear in any order, but exactly one positional path is
   required. *)
let parse_run args =
  let path = ref None in
  let dumps = ref Driver.no_dumps in
  let set_path arg =
    match !path with
    | Some _ -> Driver.die ("unexpected extra argument: " ^ arg ^ "\n" ^ usage)
    | None -> path := Some arg
  in
  List.iter
    (fun arg ->
      match arg with
      | "--dump-source" -> dumps := { !dumps with Driver.source = true }
      | "--dump-tokens" -> dumps := { !dumps with Driver.tokens = true }
      | "--dump-ast" -> dumps := { !dumps with Driver.ast = true }
      | "--dump-types" -> dumps := { !dumps with Driver.types = true }
      | "--dump-code" -> dumps := { !dumps with Driver.code = true }
      | "-h" | "--help" ->
        print_endline usage;
        exit 0
      | _ when String.length arg > 1 && arg.[0] = '-' ->
        Driver.die ("unknown option: " ^ arg ^ "\n" ^ usage)
      | _ -> set_path arg)
    args;
  match !path with
  | None -> None, !dumps
  | Some path -> Some path, !dumps

let new_package = function
  | [ name ] ->
    (match Cx.Skeleton.create ~directory:name ~name with
     | Ok () -> Printf.printf "Created package '%s'.\n" name
     | Error message -> Driver.die message)
  | [] -> Driver.die ("new needs a name.\n" ^ usage)
  | _ -> Driver.die ("new takes one name.\n" ^ usage)

let package_root () =
  match Cx.Manifest.find_root (Sys.getcwd ()) with
  | Some root -> root
  | None -> Driver.die "There is no cronyx.toml here or above."

let report entry errors =
  Render.emit ~entry errors;
  exit 65

let build () =
  let root = package_root () in
  match Cx.Build.package ~out:print_string root with
  | Error errors -> report root errors
  | Ok (artifacts, compiled) ->
    List.iter
      (fun (a : Artifact.t) ->
        Printf.printf
          "%s %s\n"
          (if List.mem a.Artifact.package compiled then "checked" else "cached")
          a.Artifact.package)
      artifacts

let run_package dumps =
  let root = package_root () in
  match Cx.Build.package ~out:print_string root with
  | Error errors -> report root errors
  | Ok (artifacts, _) ->
    let entry = Option.value (Cx.Workspace.entry_of root) ~default:root in
    Driver.execute_linked ~dumps ~entry (Cx.Build.link artifacts)

let () =
  match List.tl (Array.to_list Sys.argv) with
  | [] -> Driver.die usage
  | ("-h" | "--help") :: _ ->
    print_endline usage;
    exit 0
  | "new" :: args -> new_package args
  | "build" :: _ -> build ()
  | "run" :: args ->
    (match parse_run args with
     (* No file named: the package here, through its artifacts. *)
     | None, dumps -> run_package dumps
     | Some path, dumps ->
       (match Cx.Workspace.roots_for path with
        | Error errors -> report path errors
        | Ok (roots, _) -> Driver.execute ~dumps ~roots path))
  | command :: _ -> Driver.die ("unknown command: " ^ command ^ "\n" ^ usage)
