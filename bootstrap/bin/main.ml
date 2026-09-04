open Bootstrap

let usage =
  "usage: bootstrap [options] <file.cx>\n\
  \  --dump-source   echo the source before running\n\
  \  --dump-tokens   print the token stream\n\
  \  --dump-ast      print the parsed AST\n\
  \  --dump-types    print the type-checked AST\n\
  \  --dump-code     print the program as Cronyx, after metaprocessing\n\
  \  -h, --help      show this message"

(* Flags may appear in any order, but exactly one positional path is
   required. *)
let parse_args argv =
  let path = ref None in
  let dumps = ref Driver.no_dumps in
  let set_path arg =
    match !path with
    | Some _ -> Driver.die ("unexpected extra argument: " ^ arg ^ "\n" ^ usage)
    | None -> path := Some arg
  in
  Array.iteri
    (fun i arg ->
      if i > 0
      then (
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
        | _ -> set_path arg))
    argv;
  match !path with
  | None -> Driver.die usage
  | Some path -> path, !dumps

let () =
  let path, dumps = parse_args Sys.argv in
  Driver.execute ~dumps path
