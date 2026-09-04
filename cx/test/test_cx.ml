(* Manifest fixtures live in cx/test/manifests: a .toml with a .ok holding what
   the tool read out of it, or with a .err holding one `[line:col] message` per
   diagnostic. Listed explicitly, so the lists record what is covered. *)

open Bootstrap

let accepted = [ "minimal"; "deps"; "comments"; "dotted" ]

let rejected =
  [ "unknown_top_key"
  ; "unknown_package_key"
  ; "missing_name"
  ; "missing_cronyx"
  ; "partial_version"
  ; "leading_zero"
  ; "version_not_string"
  ; "bad_name"
  ; "registry_dep"
  ; "dep_unknown_key"
  ; "dep_missing_path"
  ; "duplicate_key"
  ; "unterminated_string"
  ; "trailing_junk"
  ]

(* Package fixtures live in cx/test/packages: a directory holding a manifest,
   with an expected.txt of what it prints or an expected.err of the diagnostics
   reading it produced. *)
let packages = [ "two_packages/app"; "uses_std"; "same_unit_name"; "generic_dep" ]
let bad_packages = [ "reaches_out"; "claims_std"; "overlapping_impls" ]

let repo_root () =
  let marker = Filename.concat "cx" (Filename.concat "test" "manifests") in
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

let normalize s = String.trim s

let summary (m : Cx.Manifest.t) =
  String.concat
    "\n"
    ([ Printf.sprintf "name = %s" m.Cx.Manifest.name
     ; Printf.sprintf "version = %s" (Cx.Version.to_string m.Cx.Manifest.version)
     ; Printf.sprintf "cronyx = %s" (Cx.Version.to_string m.Cx.Manifest.cronyx)
     ]
     @ List.map
         (fun (d : Cx.Manifest.dependency) ->
           match d.Cx.Manifest.source with
           | Cx.Manifest.Path path -> Printf.sprintf "dep %s = path %s" d.Cx.Manifest.name path)
         m.Cx.Manifest.dependencies)

(* A diagnostic about a file other than the entry renders that file's path, and
   here that path is absolute. The fixtures are read on more than one machine,
   so the repo root comes back off. *)
let relative root text =
  let prefix = root ^ "/" in
  let width = String.length prefix in
  let buffer = Buffer.create (String.length text) in
  let i = ref 0 in
  while !i < String.length text do
    if !i + width <= String.length text && String.equal (String.sub text !i width) prefix
    then i := !i + width
    else (
      Buffer.add_char buffer text.[!i];
      incr i)
  done;
  Buffer.contents buffer

let diagnostics ?(root = "") path errors =
  let render (e : Diagnostic.error) =
    Printf.sprintf "%s %s" (Ast.locate ~entry:path e.Diagnostic.span) e.Diagnostic.message
  in
  let text = String.concat "\n" (List.map render errors) in
  if String.equal root "" then text else relative root text

let compare_case name ~expected ~actual =
  if String.equal (normalize expected) (normalize actual)
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

let run_accepted dir name =
  let path = Filename.concat dir (name ^ ".toml") in
  match Cx.Manifest.load path with
  | Error errors ->
    Printf.printf "FAIL manifest/%s\n  %s\n" name (diagnostics path errors);
    false
  | Ok manifest ->
    compare_case ("manifest/" ^ name) ~expected:(read_file (Filename.concat dir (name ^ ".ok"))) ~actual:(summary manifest)

let run_rejected dir name =
  let path = Filename.concat dir (name ^ ".toml") in
  match Cx.Manifest.load path with
  | Ok _ ->
    Printf.printf "FAIL manifest/%s\n  expected a diagnostic, but the manifest was read\n" name;
    false
  | Error errors ->
    compare_case
      ("manifest/" ^ name)
      ~expected:(read_file (Filename.concat dir (name ^ ".err")))
      ~actual:(diagnostics path errors)

(* A fixture no list names is a failure of its own, the way it is for the
   compiler's suite. *)
let unclaimed dir =
  let claimed = Hashtbl.create 32 in
  List.iter (fun name -> Hashtbl.replace claimed name ()) (accepted @ rejected);
  Sys.readdir dir
  |> Array.to_list
  |> List.filter_map (fun entry ->
    if Filename.check_suffix entry ".toml"
    then (
      let name = Filename.remove_extension entry in
      if Hashtbl.mem claimed name then None else Some name)
    else None)
  |> List.sort String.compare

let run_partition dir =
  match unclaimed dir with
  | [] ->
    Printf.printf "ok   every manifest fixture is claimed by a list\n";
    true
  | missing ->
    Printf.printf
      "FAIL manifest fixtures no list names, so nothing runs them:\n%s\n"
      (String.concat "\n" (List.map (fun name -> "  " ^ name) missing));
    false

(* The table in the design doc, which is what a reader recognises from Cargo
   and therefore the thing that must not drift. *)
let requirements =
  [ "1.4", ">=1.4.0, <2.0.0"
  ; "^1.4", ">=1.4.0, <2.0.0"
  ; "^1.4.2", ">=1.4.2, <2.0.0"
  ; "^1", ">=1.0.0, <2.0.0"
  ; "~1.4", ">=1.4.0, <1.5.0"
  ; "~1.4.2", ">=1.4.2, <1.5.0"
  ; "~1", ">=1.0.0, <2.0.0"
  ; "=1.4.2", "=1.4.2"
  ; ">=1, <2", ">=1.0.0, <2.0.0"
  ; "0.4", ">=0.4.0, <0.5.0"
  ; "0.4.2", ">=0.4.2, <0.5.0"
  ; "0.0.3", ">=0.0.3, <0.0.4"
  ; "0.0", ">=0.0.0, <0.1.0"
  ; "0", ">=0.0.0, <1.0.0"
  ]

let run_requirement (written, expected) =
  match Cx.Requirement.of_string written with
  | Error message ->
    Printf.printf "FAIL requirement %s\n  %s\n" written message;
    false
  | Ok r ->
    let actual = Cx.Requirement.render r in
    if String.equal actual expected
    then (
      Printf.printf "ok   requirement %s\n" written;
      true)
    else (
      Printf.printf "FAIL requirement %s\n  expected %s\n  actual   %s\n" written expected actual;
      false)

let membership =
  [ "^1.4", "1.4.0", true
  ; "^1.4", "1.9.9", true
  ; "^1.4", "2.0.0", false
  ; "^1.4", "1.3.9", false
  ; "0.4", "0.5.0", false
  ; "0.0.3", "0.0.4", false
    (* A pre-release is only visible to a requirement that names one at the same
       version, or `^1.0` selects `2.0.0-rc1`. *)
  ; "^1.0", "2.0.0-rc1", false
  ; "^1.0", "1.5.0-rc1", false
  ; ">=1.0.0-rc1, <2", "1.0.0-rc1", true
  ]

let run_membership (written, version, expected) =
  match Cx.Requirement.of_string written, Cx.Version.of_string version with
  | Ok r, Ok v ->
    let actual = Cx.Requirement.satisfies r v in
    if Bool.equal actual expected
    then (
      Printf.printf "ok   %s %s %s\n" version (if expected then "satisfies" else "misses") written;
      true)
    else (
      Printf.printf "FAIL %s against %s: expected %b\n" version written expected;
      false)
  | Error message, _ | _, Error message ->
    Printf.printf "FAIL %s against %s\n  %s\n" version written message;
    false

(* The ordering from the SemVer specification, which is the one place a
   hand-rolled comparison usually goes wrong. *)
let ordering =
  [ "1.0.0-alpha"
  ; "1.0.0-alpha.1"
  ; "1.0.0-alpha.beta"
  ; "1.0.0-beta"
  ; "1.0.0-beta.2"
  ; "1.0.0-beta.11"
  ; "1.0.0-rc.1"
  ; "1.0.0"
  ; "1.0.1"
  ; "1.1.0"
  ; "2.0.0"
  ]

let run_ordering () =
  let parsed = List.map (fun text -> text, Cx.Version.of_string text) ordering in
  let ok = ref true in
  List.iteri
    (fun i (text, v) ->
      List.iteri
        (fun j (other, w) ->
          match v, w with
          | Ok v, Ok w ->
            let expected = Int.compare i j in
            let actual = Int.compare (Cx.Version.compare v w) 0 in
            if actual <> expected
            then (
              Printf.printf "FAIL ordering %s vs %s: expected %d, got %d\n" text other expected actual;
              ok := false)
          | Error message, _ | _, Error message ->
            Printf.printf "FAIL ordering %s\n  %s\n" text message;
            ok := false)
        parsed)
    parsed;
  if !ok then Printf.printf "ok   pre-release ordering\n";
  !ok

let interpret roots entry =
  let buf = Buffer.create 256 in
  let out = Buffer.add_string buf in
  match Pipeline.compile ~roots ~out entry with
  | Error errors -> Error errors
  | Ok converted ->
    (match Pipeline.run (Builtins.env ~out) converted with
     | Ok () -> Ok (Buffer.contents buf)
     | Error e -> Error [ e ])

let rec remove path =
  if Sys.file_exists path
  then
    if Sys.is_directory path
    then (
      Array.iter (fun entry -> remove (Filename.concat path entry)) (Sys.readdir path);
      Sys.rmdir path)
    else Sys.remove path

(* Every `target/` under the fixture tree, so a run starts from no artifacts at
   all and the first build is the one that writes them. *)
let clean dir =
  let rec walk path =
    if Sys.is_directory path
    then
      Array.iter
        (fun entry ->
          let child = Filename.concat path entry in
          if String.equal entry "target" then remove child else walk child)
        (Sys.readdir path)
  in
  walk dir

(* Through artifacts: every package compiled to a file, the files concatenated,
   and the result run. *)
let built root =
  let buf = Buffer.create 256 in
  let out = Buffer.add_string buf in
  match Cx.Build.package ~out root with
  | Error errors -> Error errors
  | Ok artifacts ->
    (match Compile.program (Cx.Build.link artifacts) with
     | Error errors -> Error errors
     | Ok converted ->
       (match Pipeline.run (Builtins.env ~out) converted with
        | Ok () -> Ok (Buffer.contents buf)
        | Error e -> Error [ e ]))

(* Built from nothing, then built again over the artifacts the first run left.
   Both have to agree with the expectation, which is what makes `target/` a
   cache rather than part of the program. *)
let package_case dir name =
  let root = Filename.concat dir name in
  let expected = read_file (Filename.concat root "expected.txt") in
  clean dir;
  match built root, built root with
  | Ok cold, Ok warm ->
    compare_case ("package/" ^ name) ~expected ~actual:cold
    && compare_case ("package/" ^ name ^ " (again)") ~expected ~actual:warm
  | Error errors, _ | _, Error errors ->
    Printf.printf "FAIL package/%s\n  %s\n" name (diagnostics ~root:dir root errors);
    false

let bad_package_case dir name =
  let root = Filename.concat dir name in
  let expected = read_file (Filename.concat root "expected.err") in
  let rejected path errors =
    compare_case ("package/" ^ name) ~expected ~actual:(diagnostics ~root:dir path errors)
  in
  clean dir;
  match built root with
  | Error errors -> rejected (Option.value (Cx.Workspace.entry_of root) ~default:root) errors
  | Ok _ ->
    Printf.printf "FAIL package/%s\n  expected a diagnostic, but it ran\n" name;
    false

(* A package fixture no list names is a failure of its own, as with the
   manifests. *)
let unclaimed_packages dir =
  let claimed = Hashtbl.create 8 in
  List.iter (fun name -> Hashtbl.replace claimed name ()) (packages @ bad_packages);
  let rec walk prefix =
    let full = if String.equal prefix "" then dir else Filename.concat dir prefix in
    Sys.readdir full
    |> Array.to_list
    |> List.sort String.compare
    |> List.concat_map (fun entry ->
      let name = if String.equal prefix "" then entry else Filename.concat prefix entry in
      let path = Filename.concat dir name in
      if not (Sys.is_directory path)
      then []
      else if Sys.file_exists (Filename.concat path Cx.Manifest.file_name)
      then
        if Sys.file_exists (Filename.concat path "expected.txt")
           || Sys.file_exists (Filename.concat path "expected.err")
        then if Hashtbl.mem claimed name then [] else [ name ]
        else []
      else walk name)
  in
  walk ""

let run_package_partition dir =
  match unclaimed_packages dir with
  | [] ->
    Printf.printf "ok   every package fixture is claimed by a list\n";
    true
  | missing ->
    Printf.printf
      "FAIL package fixtures no list names, so nothing runs them:\n%s\n"
      (String.concat "\n" (List.map (fun name -> "  " ^ name) missing));
    false

let () =
  match repo_root () with
  | None ->
    prerr_endline "cannot find the repo root; set CRONYX_REPO_ROOT";
    exit 1
  | Some root ->
    let dir = Filename.concat root (Filename.concat "cx" (Filename.concat "test" "manifests")) in
    let packages_dir =
      Filename.concat root (Filename.concat "cx" (Filename.concat "test" "packages"))
    in
    let results =
      (run_partition dir :: List.map (run_accepted dir) accepted)
      @ List.map (run_rejected dir) rejected
      @ (run_package_partition packages_dir :: List.map (package_case packages_dir) packages)
      @ List.map (bad_package_case packages_dir) bad_packages
      @ List.map run_requirement requirements
      @ List.map run_membership membership
      @ [ run_ordering () ]
    in
    let passed = List.length (List.filter Fun.id results) in
    let total = List.length results in
    Printf.printf "\n%d/%d passed\n" passed total;
    if passed <> total then exit 1
