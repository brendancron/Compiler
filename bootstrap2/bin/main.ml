open Bootstrap2

let usage =
  "usage: bootstrap2 [options] <file.cx>\n\
  \  --dump-source   echo the source before running\n\
  \  --dump-tokens   print the token stream\n\
  \  --dump-ast      print the parsed AST\n\
  \  -h, --help      show this message"

type options =
  { path : string
  ; dump_source : bool
  ; dump_tokens : bool
  ; dump_ast : bool
  }

let die message =
  prerr_endline message;
  exit 64

(* No external arg parser: flags may appear in any order, but exactly one
   positional path is required. *)
let parse_args argv =
  let path = ref None in
  let dump_source = ref false in
  let dump_tokens = ref false in
  let dump_ast = ref false in
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
    }

let read_file path =
  match open_in_bin path with
  | exception Sys_error message -> die message
  | ic -> Fun.protect ~finally:(fun () -> close_in ic) (fun () -> really_input_string ic (in_channel_length ic))

let report line col kind message =
  Printf.eprintf "[%d:%d] %s error: %s\n" line col kind message

let () =
  let opts = parse_args Sys.argv in
  let source = read_file opts.path in
  if opts.dump_source
  then (
    print_endline "-- source --";
    print_string source;
    if not (String.length source > 0 && source.[String.length source - 1] = '\n')
    then print_newline ());
  match Scanner.scan_tokens source with
  | Error errors ->
    List.iter (fun (e : Scanner.error) -> report e.line e.col "Scan" e.message) errors;
    exit 65
  | Ok tokens ->
    if opts.dump_tokens
    then (
      print_endline "-- tokens --";
      List.iter (fun t -> print_endline (Token.to_string t)) tokens);
    (match Parser.parse tokens with
     | Error errors ->
       List.iter (fun (e : Parser.error) -> report e.line e.col "Parse" e.message) errors;
       exit 65
     | Ok program ->
       if opts.dump_ast
       then (
         print_endline "-- ast --";
         print_string (Printer.string_of_program program));
       (match Interp.run (Desugar.program program) with
        | Ok () -> ()
        | Error e ->
          report e.span.line e.span.col "Runtime" e.message;
          exit 70))
