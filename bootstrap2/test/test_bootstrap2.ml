(* Integration tests against the shared fixtures in <repo>/tests: each case is a
   .cx source paired with a .txt of its expected stdout, compared after
   trimming (same rule as the Rust harness).

   Cases are listed explicitly rather than globbed, so the list doubles as a
   record of what this bootstrap actually supports. Most of <repo>/tests
   exercises language features that do not exist here yet. *)

open Bootstrap2

let cases =
  [ "tests/core/print/hello"
  ; "tests/core/math/math"
  ; "tests/core/variables/variables"
  ; "tests/core/variables/reassign"
  ; "tests/core/control/if"
  ; "tests/core/control/else"
  ; "tests/core/control/if_else_chain"
  ; "tests/core/control/while"
  ; "tests/core/control/for_c"
  ; "tests/core/operators/comparison"
  ; "tests/core/operators/compound_assign"
  ; "tests/core/strings/concat"
  ; "tests/core/functions/fib"
  ; "tests/core/functions/greeting"
  ; "tests/core/functions/return"
  ; "tests/core/functions/closure"
  ]

(* The fixtures live outside the dune project root, so find them at runtime. *)
let repo_root () =
  let marker = Filename.concat "tests" (Filename.concat "core" "print") in
  let rec up dir =
    if Sys.file_exists (Filename.concat dir marker)
    then Some dir
    else (
      let parent = Filename.dirname dir in
      if String.equal parent dir then None else up parent)
  in
  match Sys.getenv_opt "CRONYX_REPO_ROOT" with
  | Some dir -> Some dir
  | None -> up (Sys.getcwd ())

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let normalize s =
  String.trim (String.concat "\n" (String.split_on_char '\r' s |> List.filter (( <> ) "")))

(* Run one program to completion, returning its stdout or the first error. *)
let interpret source =
  let buf = Buffer.create 256 in
  let out = Buffer.add_string buf in
  match Scanner.scan_tokens source with
  | Error (e :: _) -> Error (Printf.sprintf "scan error [%d:%d] %s" e.line e.col e.message)
  | Error [] -> Error "scan failed"
  | Ok tokens ->
    (match Parser.parse tokens with
     | Error (e :: _) ->
       Error (Printf.sprintf "parse error [%d:%d] %s" e.line e.col e.message)
     | Error [] -> Error "parse failed"
     | Ok program ->
       (match Interp.run ~out (Desugar.program program) with
        | Ok () -> Ok (Buffer.contents buf)
        | Error e ->
          Error
            (Printf.sprintf
               "runtime error [%d:%d] %s"
               e.span.Ast.line
               e.span.Ast.col
               e.message)))

let run_case root name =
  let path ext = Filename.concat root (name ^ ext) in
  let expected = read_file (path ".txt") in
  match interpret (read_file (path ".cx")) with
  | Error message ->
    Printf.printf "FAIL %s\n  %s\n" name message;
    false
  | Ok actual ->
    if String.equal (normalize actual) (normalize expected)
    then (
      Printf.printf "ok   %s\n" name;
      true)
    else (
      Printf.printf
        "FAIL %s\n  --- expected ---\n%s\n  --- actual ---\n%s\n"
        name
        (normalize expected)
        (normalize actual);
      false)

let () =
  match repo_root () with
  | None ->
    prerr_endline "could not locate the repo root (set CRONYX_REPO_ROOT)";
    exit 1
  | Some root ->
    let results = List.map (run_case root) cases in
    let failed = List.length (List.filter not results) in
    Printf.printf "\n%d/%d passed\n" (List.length results - failed) (List.length results);
    if failed > 0 then exit 1
