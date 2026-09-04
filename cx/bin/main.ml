open Bootstrap

let usage =
  "usage: cx <command> [options]\n\n\
  \  new <name>      create a package skeleton\n\
  \  run <file.cx>   compile and execute a program\n\n\
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
  | None -> Driver.die ("run needs a file to run.\n" ^ usage)
  | Some path -> path, !dumps

let new_package = function
  | [ name ] ->
    (match Cx.Skeleton.create ~directory:name ~name with
     | Ok () -> Printf.printf "Created package '%s'.\n" name
     | Error message -> Driver.die message)
  | [] -> Driver.die ("new needs a name.\n" ^ usage)
  | _ -> Driver.die ("new takes one name.\n" ^ usage)

let () =
  match List.tl (Array.to_list Sys.argv) with
  | [] -> Driver.die usage
  | ("-h" | "--help") :: _ ->
    print_endline usage;
    exit 0
  | "new" :: args -> new_package args
  | "run" :: args ->
    let path, dumps = parse_run args in
    Driver.execute ~dumps path
  | command :: _ -> Driver.die ("unknown command: " ^ command ^ "\n" ^ usage)
