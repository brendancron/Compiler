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
  ; "tests/core/operators/unary_minus"
  ; "tests/core/type_annotations/var_annot"
  ; "tests/core/type_annotations/fn_annot"
  ; "tests/core/type_annotations/mixed_annot"
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
  ; "tests/core/lists/list_methods"
  ; "tests/core/strings/string_methods"
  ; "tests/core/operators/not_index"
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
  ; "tests/core/traits/basic_impl/main"
  ; "tests/core/traits/multiple_impls/main"
  ; "tests/core/traits/inherent/main"
  ; "tests/core/traits/builtin_receiver/main"
  ; "tests/effects/methods/methods"
  ; "tests/core/comptime/type_params/main"
  ; "tests/core/comptime/type_reuse/main"
  ; "tests/core/comptime/parameterized_types/main"
  ; "tests/core/comptime/parameterized_sums/main"
  ; "tests/core/comptime/value_params/main"
  ; "tests/core/comptime/mixed_params/main"
  ; "tests/core/traits/trait_bound/main"
  ; "tests/core/comptime/inferred_constraints/main"
  ; "tests/meta/params/func"
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
  ; "tests/core/traits/errors/incomplete"
  ; "tests/core/traits/errors/unknown_trait"
  ; "tests/core/traits/errors/duplicate"
  ; "tests/core/traits/errors/no_self"
  ; "tests/core/traits/errors/no_such_method"
  ; "tests/core/traits/errors/arity"
  ; "tests/core/traits/errors/unknown_method"
  ; "tests/core/comptime/errors/type_arity"
  ; "tests/core/comptime/errors/comptime_arity"
  ; "tests/core/comptime/errors/not_comptime"
  ; "tests/core/comptime/errors/comptime_call"
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
       (match Desugar.program (Prelude.program () @ program) with
        | Error e ->
          Error
            (Printf.sprintf "desugar error [%d:%d] %s" e.span.Ast.line e.span.Ast.col e.message)
        | Ok desugared ->
        match Monomorphize.program desugared with
        | Error e ->
          Error
            (Printf.sprintf
               "comptime error [%d:%d] %s"
               e.span.Ast.line
               e.span.Ast.col
               e.message)
        | Ok desugared ->
        let registry = Registry.builtins () in
        Builtins.register registry;
        match Typecheck.check ~registry desugared with
        | Error (e :: _) ->
          Error
            (Printf.sprintf "type error [%d:%d] %s" e.span.Ast.line e.span.Ast.col e.message)
        | Error [] -> Error "type check failed"
        | Ok typed ->
          match Resolve.program ~registry (Specialize.program typed) with
          | Error e ->
            Error
              (Printf.sprintf
                 "resolve error [%d:%d] %s"
                 e.span.Ast.line
                 e.span.Ast.col
                 e.message)
          | Ok resolved ->
          (match Cps.program (Reflect.program resolved) with
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
             match Interp.run (Builtins.env ~out) converted with
              | Ok () -> Ok (Buffer.contents buf)
              | Error e ->
                Error
                  (Printf.sprintf
                     "runtime error [%d:%d] %s"
                     e.span.Ast.line
                     e.span.Ast.col
                     e.message)))))

(* Why a fixture does not run yet. A step is one from the implementation plan;
   the rest will not be fixed by finishing it. *)
type blocker =
  | Step of string
  | Rewrite (* predates a syntax decision, so the fixture is what is wrong *)
  | Deferred (* a decision taken later on purpose; see internal-docs/TODO.md *)
  | Backend (* needs the LLVM path, which this bootstrap does not have *)
  | Unplanned (* no design for it anywhere yet *)

(* Every fixture under tests/ that neither [cases] nor [error_cases] claims.
   The suite asserts each still fails, so one that starts working is reported
   rather than sitting unnoticed — which is how four of them came to be passing
   with nothing recording it. *)
let expected_failing : (string * blocker) list =
    (* 9 · Iteration *)
  [ "tests/core/lists/list", Step "9 · Iteration"
  ; "tests/effects/logic/multi_guard", Step "9 · Iteration"
  ; "tests/effects/logic/simple_guard", Step "9 · Iteration"
    (* 10 · Modules *)
  ; "tests/core/for_tuple/for_tuple", Step "10 · Modules"
  ; "tests/core/modules/alias/main", Step "10 · Modules"
  ; "tests/core/modules/circular/main", Step "10 · Modules"
  ; "tests/core/modules/main", Step "10 · Modules"
  ; "tests/core/modules/multi_export/main", Step "10 · Modules"
  ; "tests/core/modules/qualified/main", Step "10 · Modules"
  ; "tests/core/modules/same_dir/main", Step "10 · Modules"
  ; "tests/core/modules/selective/main", Step "10 · Modules"
  ; "tests/core/modules/wildcard/main", Step "10 · Modules"
  ; "tests/stdlib/automata/dfa/dfa", Step "10 · Modules"
  ; "tests/stdlib/automata/nfa/nfa", Step "10 · Modules"
  ; "tests/stdlib/error/error", Step "10 · Modules"
  ; "tests/stdlib/fallible/fallible", Step "10 · Modules"
  ; "tests/stdlib/hashmap/hashmap", Step "10 · Modules"
  ; "tests/stdlib/hashset/hashset", Step "10 · Modules"
  ; "tests/stdlib/iterable/iterable", Step "10 · Modules"
  ; "tests/stdlib/list/list", Step "10 · Modules"
  ; "tests/stdlib/math/math", Step "10 · Modules"
  ; "tests/stdlib/regex/regex/regex", Step "10 · Modules"
  ; "tests/stdlib/string/string", Step "10 · Modules"
  ; "tests/stdlib/stringbuilder/stringbuilder", Step "10 · Modules"
  ; "tests/stdlib/toml/toml/toml", Step "10 · Modules"
  ; "tests/stdlib/tostring/tostring", Step "10 · Modules"
    (* 11 · Metaprocessing *)
  ; "tests/meta/codegen/basic", Step "11 · Metaprocessing"
  ; "tests/meta/codegen/env", Step "11 · Metaprocessing"
  ; "tests/meta/codegen/gen_meta", Step "11 · Metaprocessing"
  ; "tests/meta/codegen/gen_symbol", Step "11 · Metaprocessing"
  ; "tests/meta/codegen/greeting", Step "11 · Metaprocessing"
  ; "tests/meta/codegen/nested", Step "11 · Metaprocessing"
  ; "tests/meta/codegen/sub1", Step "11 · Metaprocessing"
  ; "tests/meta/derive/basic/main", Step "11 · Metaprocessing"
  ; "tests/meta/execution/basic", Step "11 · Metaprocessing"
  ; "tests/meta/execution/nested", Step "11 · Metaprocessing"
  ; "tests/meta/functions/fib", Step "11 · Metaprocessing"
  ; "tests/meta/functions/meta_fn", Step "11 · Metaprocessing"
    (* 12 · typeof yields a Type *)
  ; "tests/reflection/typeof_effect_ctl", Step "12 · typeof yields a Type"
  ; "tests/reflection/typeof_effect_fn_vs_ctl", Step "12 · typeof yields a Type"
  ; "tests/reflection/typeof_effect_multi", Step "12 · typeof yields a Type"
  ; "tests/reflection/typeof_effect_transitive", Step "12 · typeof yields a Type"
  ; "tests/reflection/typeof_enum", Step "12 · typeof yields a Type"
  ; "tests/reflection/typeof_slice", Step "12 · typeof yields a Type"
    (* Rewrite *)
  ; "tests/core/builtins/free", Rewrite
  ; "tests/core/enums/unit_variants", Rewrite
  ; "tests/core/enums/wildcard", Rewrite
  ; "tests/core/structs/struct2", Rewrite
  ; "tests/core/structs/struct_dot_assign", Rewrite
  ; "tests/types/gadt/main", Rewrite
    (* Deferred *)
  ; "tests/core/defer/defer_basic", Deferred
  ; "tests/core/defer/defer_lifo", Deferred
  ; "tests/core/defer/defer_return", Deferred
  ; "tests/core/embed/embed", Deferred
  ; "tests/core/resolution/symbol_res", Deferred
  ; "tests/core/slices/negative_index", Deferred
  ; "tests/core/slices/slice_range", Deferred
  ; "tests/core/strings/string_slice", Deferred
    (* Backend *)
  ; "tests/compile/m0/m0", Backend
  ; "tests/compile/m1/fib", Backend
  ; "tests/compile/m2/struct", Backend
  ; "tests/compile/m3/fact", Backend
  ; "tests/compile/m4/countdown", Backend
  ; "tests/compile/m5/sum", Backend
  ; "tests/compile/m6/apply", Backend
  ; "tests/compile/m7/safe_div", Backend
  ; "tests/compile/m8/gadt", Backend
    (* Unplanned *)
  ; "tests/core/builtins/conversions", Unplanned
  ; "tests/core/builtins/ord", Unplanned
  ; "tests/core/builtins/readfile", Unplanned
  ; "tests/core/builtins/writefile", Unplanned
  ; "tests/core/functions/trailing_after_args", Unplanned
  ; "tests/core/functions/trailing_explicit_param", Unplanned
  ; "tests/core/functions/trailing_foreach", Unplanned
  ; "tests/core/functions/trailing_it", Unplanned
  ; "tests/core/functions/trailing_multi_param", Unplanned
  ; "tests/core/math/modulus", Unplanned
  ; "tests/core/operators/logical", Unplanned
  ; "tests/core/operators/logical_symbols", Unplanned
  ; "tests/core/operators/precedence", Unplanned
  ; "tests/core/strings/string_index", Unplanned
  ; "tests/core/strings/string_starts_ends", Unplanned
  ; "tests/core/structs/struct", Unplanned
  ; "tests/effects/async/async", Unplanned
  ; "tests/effects/generic/generic", Unplanned
  ]

(* Formats diagnostics the way a .err file spells them. *)
let rejections source =
  match Scanner.scan_tokens source with
  | Error _ -> Error "scan failed"
  | Ok tokens ->
    (match Parser.parse tokens with
     | Error _ -> Error "parse failed"
     | Ok program ->
       (match Desugar.program (Prelude.program () @ program) with
        | Error e ->
          Ok (Printf.sprintf "[%d:%d] %s" e.span.Ast.line e.span.Ast.col e.message)
        | Ok desugared ->
        match Monomorphize.program desugared with
        | Error e ->
          Ok (Printf.sprintf "[%d:%d] %s" e.span.Ast.line e.span.Ast.col e.message)
        | Ok desugared ->
        let registry = Registry.builtins () in
        Builtins.register registry;
        match Typecheck.check ~registry desugared with
        | Ok typed ->
          (match Resolve.program ~registry (Specialize.program typed) with
           | Ok _ -> Error "expected an error, but the program checked"
           | Error e ->
             Ok (Printf.sprintf "[%d:%d] %s" e.span.Ast.line e.span.Ast.col e.message))
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

(* A fixture is expected to fail, so passing is the failure. *)
let run_expected_failing root (name, blocker) =
  let path ext = Filename.concat root (name ^ ext) in
  let expected = if Sys.file_exists (path ".txt") then Some (read_file (path ".txt")) else None in
  match interpret (read_file (path ".cx")), expected with
  | Ok actual, Some expected when String.equal (normalize actual) (normalize expected) ->
    Printf.printf
      "PASSES %s\n  now works; move it into `cases` (was waiting on %s)\n"
      name
      (match blocker with
       | Step step -> step
       | Rewrite -> "a rewrite"
       | Deferred -> "a deferred decision"
       | Backend -> "the backend"
       | Unplanned -> "a design");
    false
  | _ -> true

let () =
  match repo_root () with
  | None ->
    prerr_endline "could not locate the repo root (set CRONYX_REPO_ROOT)";
    exit 1
  | Some root ->
    let results =
      List.map (run_case root) cases
      @ List.map (run_error_case root) error_cases
      @ List.map (run_expected_failing root) expected_failing
    in
    let failed = List.length (List.filter not results) in
    Printf.printf "\n%d/%d passed\n" (List.length results - failed) (List.length results);
    if failed > 0 then exit 1
