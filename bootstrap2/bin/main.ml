open Bootstrap2

let usage =
  "usage: bootstrap2 [options] <file.cx>\n\
  \  --dump-source   echo the source before running\n\
  \  --dump-tokens   print the token stream\n\
  \  --dump-ast      print the parsed AST\n\
  \  --dump-types    print the type-checked AST\n\
  \  --dump-code     print the program as Cronyx, after metaprocessing\n\
  \  -h, --help      show this message"

type options =
  { path : string
  ; dump_source : bool
  ; dump_tokens : bool
  ; dump_ast : bool
  ; dump_types : bool
  ; dump_code : bool
  }

let die message =
  prerr_endline message;
  exit 64

(* Flags may appear in any order, but exactly one positional path is
   required. *)
let parse_args argv =
  let path = ref None in
  let dump_source = ref false in
  let dump_tokens = ref false in
  let dump_ast = ref false in
  let dump_types = ref false in
  let dump_code = ref false in
  let set_path arg =
    match !path with
    | Some _ -> die ("unexpected extra argument: " ^ arg ^ "\n" ^ usage)
    | None -> path := Some arg
  in
  Array.iteri
    (fun i arg ->
      if i > 0
      then (
        match arg with
        | "--dump-source" -> dump_source := true
        | "--dump-tokens" -> dump_tokens := true
        | "--dump-ast" -> dump_ast := true
        | "--dump-types" -> dump_types := true
        | "--dump-code" -> dump_code := true
        | "-h" | "--help" ->
          print_endline usage;
          exit 0
        | _ when String.length arg > 1 && arg.[0] = '-' ->
          die ("unknown option: " ^ arg ^ "\n" ^ usage)
        | _ -> set_path arg))
    argv;
  match !path with
  | None -> die usage
  | Some path ->
    { path
    ; dump_source = !dump_source
    ; dump_tokens = !dump_tokens
    ; dump_ast = !dump_ast
    ; dump_types = !dump_types
    ; dump_code = !dump_code
    }

let read_file path =
  match open_in_bin path with
  | exception Sys_error message -> die message
  | ic -> Fun.protect ~finally:(fun () -> close_in ic) (fun () -> really_input_string ic (in_channel_length ic))

let report ~entry (e : Diagnostic.error) =
  Printf.eprintf
    "%s %s error: %s\n"
    (Ast.locate ~entry e.Diagnostic.span)
    (Diagnostic.stage_name e.Diagnostic.stage)
    e.Diagnostic.message

let die_at ~entry errors =
  List.iter (report ~entry) errors;
  match errors with
  | [] -> exit 70
  | e :: _ -> exit (Diagnostic.exit_code e.Diagnostic.stage)

(* [Loader] scans and parses the entry file again. It happens here first so that
   a mistake in the file the user named is reported as a scan or a parse error
   rather than as a failure to load a module. *)
let dump_front ~entry opts source =
  if opts.dump_source
  then (
    print_endline "-- source --";
    print_string source;
    if not (String.length source > 0 && source.[String.length source - 1] = '\n')
    then print_newline ());
  let located stage (file, line, col) message =
    Diagnostic.at stage { Ast.file; line; col } message
  in
  match Scanner.scan_tokens source with
  | Error errors ->
    die_at
      ~entry
      (List.map
         (fun (e : Scanner.error) ->
           located Diagnostic.Scan (e.file, e.line, e.col) e.message)
         errors)
  | Ok tokens ->
    if opts.dump_tokens
    then (
      print_endline "-- tokens --";
      List.iter (fun t -> print_endline (Token.to_string t)) tokens);
    (match Parser.parse tokens with
     | Error errors ->
       die_at
         ~entry
         (List.map
            (fun (e : Parser.error) ->
              located Diagnostic.Parse (e.file, e.line, e.col) e.message)
            errors)
     | Ok program ->
       if opts.dump_ast
       then (
         print_endline "-- ast --";
         print_string (Printer.string_of_program program)))

let () =
  let opts = parse_args Sys.argv in
  let entry = opts.path in
  dump_front ~entry opts (read_file entry);
  let on_code processed =
    if opts.dump_code
    then (
      print_endline "-- code --";
      print_string (Source.program processed))
  in
  let on_types typed =
    if opts.dump_types
    then (
      print_endline "-- types --";
      print_string (Printer.string_of_typed_program typed))
  in
  match Pipeline.compile ~on_code ~on_types ~out:print_string entry with
  | Error errors -> die_at ~entry errors
  | Ok converted ->
    (match Pipeline.run (Builtins.env ~out:print_string) converted with
     | Ok () -> ()
     | Error e -> die_at ~entry [ e ])
