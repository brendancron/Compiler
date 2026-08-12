(* Fixtures live in <repo>/tests. A run case pairs a .cx with a .txt of its
   expected stdout; an error case pairs a .cx with a .err holding one
   `[line:col] message` per diagnostic.

   Cases are listed explicitly rather than globbed, so the lists double as a
   record of what this bootstrap supports — most of <repo>/tests exercises
   features that do not exist here yet. *)

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
  ; "tests/types/inference/annotations"
  ; "tests/types/inference/numeric_defaulting"
  ; "tests/types/inference/polymorphism"
  ; "tests/types/inference/float_math"
  ; "tests/types/inference/higher_order"
  ; "tests/reflection/typeof_exprs"
  ; "tests/reflection/typeof_primitives"
  ; "tests/reflection/typeof_fn"
  ; "tests/effects/rows/inferred_rows"
  ; "tests/effects/log/log"
  ; "tests/effects/ask/ask"
  ; "tests/effects/multi_handle/multi_handle"
  ; "tests/effects/exception/exception"
  ; "tests/effects/delim/delim"
  ; "tests/effects/flip/flip"
  ; "tests/effects/recover/recover"
  ; "tests/effects/handler/handler"
  ; "tests/effects/stream/stream"
  ; "tests/core/arrays/basics"
  ; "tests/core/arrays/identity"
  ; "tests/core/lists/index_access"
  ; "tests/core/lists/index_assign"
  ; "tests/core/tuples/tuple_basic"
  ; "tests/reflection/typeof_tuple"
  ; "tests/core/tuples/typed"
  ; "tests/core/records/structural"
  ; "tests/reflection/typeof_record"
  ; "tests/core/records/nominal"
  ; "tests/core/enums/tuple_variants"
  ; "tests/core/enums/struct_variants"
  ; "tests/operators/vec2_add"
  ; "tests/operators/operator_mul"
  ; "tests/operators/operator_eq"
  ; "tests/operators/operator_chain"
  ; "tests/operators/operator_in_fn"
  ]

(* Programs that must be rejected, and the diagnostics they must produce. *)
let error_cases =
  [ "tests/types/errors/mixed_numeric"
  ; "tests/types/errors/non_bool_condition"
  ; "tests/types/errors/undefined_variable"
  ; "tests/types/errors/arity"
  ; "tests/types/errors/return_mismatch"
  ; "tests/types/errors/assign_mismatch"
  ; "tests/types/errors/unknown_type"
  ; "tests/types/errors/string_comparison"
  ; "tests/effects/errors/unhandled"
  ; "tests/effects/errors/non_exhaustive"
  ; "tests/effects/errors/no_such_operation"
  ; "tests/effects/errors/unknown_effect"
  ; "tests/effects/errors/purity_violated"
  ; "tests/effects/errors/unknown_handler"
  ; "tests/core/arrays/errors/mixed_elements"
  ; "tests/core/arrays/errors/not_indexable"
  ; "tests/core/arrays/errors/element_mismatch"
  ; "tests/core/tuples/errors/no_such_field"
  ; "tests/core/tuples/errors/unknown_arity"
  ; "tests/core/records/errors/no_such_field"
  ; "tests/core/records/errors/field_mismatch"
  ; "tests/core/records/errors/missing_field"
  ; "tests/core/records/errors/nominal_mismatch"
  ; "tests/core/enums/errors/not_exhaustive"
  ; "tests/core/enums/errors/no_such_variant"
  ; "tests/core/enums/errors/payload_mismatch"
  ; "tests/core/enums/errors/not_a_sum"
  ; "tests/operators/errors/duplicate"
  ; "tests/operators/errors/undeclared"
  ; "tests/operators/errors/unannotated"
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
       (match Desugar.program program with
        | Error e ->
          Error
            (Printf.sprintf "desugar error [%d:%d] %s" e.span.Ast.line e.span.Ast.col e.message)
        | Ok desugared ->
        let registry = Registry.builtins () in
        match Typecheck.check ~registry desugared with
        | Error (e :: _) ->
          Error
            (Printf.sprintf "type error [%d:%d] %s" e.span.Ast.line e.span.Ast.col e.message)
        | Error [] -> Error "type check failed"
        | Ok typed ->
          (match Cps.program (Reflect.program (Resolve.program ~registry typed)) with
           | Error e ->
             Error
               (Printf.sprintf "cps error [%d:%d] %s" e.span.Ast.line e.span.Ast.col e.message)
           | Ok converted ->
             (match Verify.program converted with
              | Error e ->
                Error
                  (Printf.sprintf
                     "verify error [%d:%d] %s"
                     e.span.Ast.line
                     e.span.Ast.col
                     e.message)
              | Ok () ->
             match Interp.run ~out converted with
              | Ok () -> Ok (Buffer.contents buf)
              | Error e ->
                Error
                  (Printf.sprintf
                     "runtime error [%d:%d] %s"
                     e.span.Ast.line
                     e.span.Ast.col
                     e.message)))))

(* Formats diagnostics the way a .err file spells them. *)
let rejections source =
  match Scanner.scan_tokens source with
  | Error _ -> Error "scan failed"
  | Ok tokens ->
    (match Parser.parse tokens with
     | Error _ -> Error "parse failed"
     | Ok program ->
       (match Desugar.program program with
        | Error e ->
          Ok (Printf.sprintf "[%d:%d] %s" e.span.Ast.line e.span.Ast.col e.message)
        | Ok desugared ->
        match Typecheck.check ~registry:(Registry.builtins ()) desugared with
        | Ok _ -> Error "expected an error, but the program checked"
        | Error errors ->
          Ok
            (String.concat
               "\n"
               (List.map
                  (fun (e : Typecheck.error) ->
                    Printf.sprintf "[%d:%d] %s" e.span.Ast.line e.span.Ast.col e.message)
                  errors))))

let run_error_case root name =
  let path ext = Filename.concat root (name ^ ext) in
  let expected = read_file (path ".err") in
  match rejections (read_file (path ".cx")) with
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
    let results = List.map (run_case root) cases @ List.map (run_error_case root) error_cases in
    let failed = List.length (List.filter not results) in
    Printf.printf "\n%d/%d passed\n" (List.length results - failed) (List.length results);
    if failed > 0 then exit 1
