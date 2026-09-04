(* Working out what the build is made of: which packages, at which versions,
   and which compiler.

   A `path` dependency is whatever version its own manifest names, so there is
   nothing to choose and the only failure is a graph that disagrees with
   itself. A registry dependency is a requirement over the versions the index
   offers, so choosing is the whole job: take the newest that satisfies every
   requirement anyone in the graph has made of that name, and when a later
   requirement rules out what was already chosen, choose again. *)

open Bootstrap

type source =
  | Path of string
  | From_registry of string (* checksum *)

type entry =
  { name : string
  ; version : Version.t
  ; source : source
  ; dependencies : string list
  }

type t =
  { packages : entry list (* sorted by name *)
  ; cronyx : Version.t
  }

type chain = (string * Version.t) list

let fail message = Error [ Diagnostic.at Diagnostic.Manifest Source_map.Span.nowhere message ]

let describe (chain : chain) =
  match chain with
  | [] -> "the root package"
  | chain ->
    String.concat
      " → "
      (List.map (fun (name, v) -> Printf.sprintf "%s %s" name (Version.to_string v)) chain)

let manifest_at root = Manifest.load (Filename.concat root Manifest.file_name)

(* Every requirement made of one name, and who made it. *)
type demand =
  { requirement : Requirement.t
  ; by : chain
  }

let resolve ?(pinned = []) root =
  let ( let* ) = Result.bind in
  let registry = Registry_source.root () in
  let paths : (string, entry * chain) Hashtbl.t = Hashtbl.create 8 in
  let demands : (string, demand list) Hashtbl.t = Hashtbl.create 8 in
  let chosen : (string, Registry_source.release) Hashtbl.t = Hashtbl.create 8 in
  let floor = ref None in
  let changed = ref true in
  let raise_floor version =
    match !floor with
    | Some current when Version.compare current version >= 0 -> ()
    | _ -> floor := Some version
  in
  let demanded name = Option.value (Hashtbl.find_opt demands name) ~default:[] in
  (* A yanked version still resolves for a lockfile that already selected it:
     otherwise one disclosure breaks every downstream build at once, which is
     not what a disclosure is for. *)
  let acceptable name (r : Registry_source.release) =
    (not r.Registry_source.yanked)
    || List.exists
         (fun (pinned_name, v) ->
           String.equal pinned_name name && Version.equal v r.Registry_source.version)
         pinned
  in
  let choose name =
    match registry with
    | None ->
      fail
        (Printf.sprintf
           "'%s' is a registry dependency and no registry is configured. Set CRONYX_REGISTRY."
           name)
    | Some registry ->
      let* releases = Registry_source.releases registry name in
      let wanted = demanded name in
      let satisfying =
        List.filter
          (fun (r : Registry_source.release) ->
            acceptable name r
            && List.for_all
                 (fun d -> Requirement.satisfies d.requirement r.Registry_source.version)
                 wanted)
          releases
      in
      (match satisfying with
       | best :: _ ->
         (match Hashtbl.find_opt chosen name with
          | Some current when Version.equal current.Registry_source.version best.Registry_source.version
            -> Ok ()
          | _ ->
            Hashtbl.replace chosen name best;
            changed := true;
            Ok ())
       | [] ->
         fail
           (Printf.sprintf
              "no version of '%s' satisfies every requirement.\n%s\n\n  %s"
              name
              (String.concat
                 "\n"
                 (List.map
                    (fun d ->
                      Printf.sprintf
                        "  ─ %s requires %s %s"
                        (describe d.by)
                        name
                        (Requirement.render d.requirement))
                    wanted))
              (match releases with
               | [] -> "The registry has no versions of it at all."
               | releases ->
                 Printf.sprintf
                   "The registry offers %s."
                   (String.concat
                      ", "
                      (List.map
                         (fun (r : Registry_source.release) ->
                           Version.to_string r.Registry_source.version
                           ^ if r.Registry_source.yanked then " (yanked)" else "")
                         releases)))))
  in
  let rec walk_path ~chain ~source root =
    let* manifest = manifest_at root in
    let name = manifest.Manifest.name in
    let version = manifest.Manifest.version in
    raise_floor manifest.Manifest.cronyx;
    match Hashtbl.find_opt paths name with
    | Some (existing, first) when not (Version.equal existing.version version) ->
      fail
        (Printf.sprintf
           "no version of '%s' satisfies every requirement.\n\
           \  ─ %s requires %s %s\n\
           \  ─ %s requires %s %s\n\n\
           \  A `path` dependency is whatever version its own manifest names, so the two cannot \
            be reconciled by choosing: one of the paths points somewhere it should not."
           name
           (describe first)
           name
           (Version.to_string existing.version)
           (describe chain)
           name
           (Version.to_string version))
    | Some _ -> Ok ()
    | None ->
      Hashtbl.replace
        paths
        name
        ( { name
          ; version
          ; source = Path source
          ; dependencies =
              List.map
                (fun (d : Manifest.dependency) -> d.Manifest.name)
                manifest.Manifest.dependencies
              |> List.sort String.compare
          }
        , chain );
      let chain = chain @ [ name, version ] in
      List.fold_left
        (fun acc (d : Manifest.dependency) ->
          let* () = acc in
          match d.Manifest.source with
          | Manifest.Path path -> walk_path ~chain ~source:path (Filename.concat root path)
          | Manifest.Registry requirement -> want ~chain d.Manifest.name requirement)
        (Ok ())
        manifest.Manifest.dependencies
  and want ~chain name requirement =
    if Hashtbl.mem paths name
    then
      fail
        (Printf.sprintf
           "'%s' is both a `path` dependency and a registry one.\n\
           \  ─ %s requires %s %s from the registry\n\n\
           \  One version per package name, so the graph has to say which it is."
           name
           (describe chain)
           name
           (Requirement.render requirement))
    else (
      let existing = demanded name in
      if not (List.exists (fun d -> String.equal (Requirement.to_string d.requirement) (Requirement.to_string requirement)) existing)
      then (
        Hashtbl.replace demands name ({ requirement; by = chain } :: existing);
        changed := true);
      choose name)
  in
  (* A choice can rule out another package's choice, so the walk repeats until
     nothing moves. It terminates because a name is only ever re-chosen for a
     lower version, and there are finitely many. *)
  let rec settle rounds =
    if rounds > 64
    then fail "Resolution did not settle."
    else if not !changed
    then Ok ()
    else (
      changed := false;
      Hashtbl.reset paths;
      let* () = walk_path ~chain:[] ~source:"." root in
      let* () =
        Hashtbl.fold
          (fun name (release : Registry_source.release) acc ->
            let* () = acc in
            List.fold_left
              (fun acc (dep, requirement) ->
                let* () = acc in
                want
                  ~chain:[ name, release.Registry_source.version ]
                  dep
                  requirement)
              (Ok ())
              release.Registry_source.requirements)
          chosen
          (Ok ())
      in
      settle (rounds + 1))
  in
  let* () = settle 0 in
  let from_registry =
    Hashtbl.fold
      (fun name (r : Registry_source.release) acc ->
        { name
        ; version = r.Registry_source.version
        ; source = From_registry r.Registry_source.checksum
        ; dependencies = List.map fst r.Registry_source.requirements |> List.sort String.compare
        }
        :: acc)
      chosen
      []
  in
  let packages =
    (Hashtbl.fold (fun _ (entry, _) acc -> entry :: acc) paths [] @ from_registry)
    |> List.sort (fun a b -> String.compare a.name b.name)
  in
  match !floor with
  | None -> fail "No package named a compiler version."
  | Some wanted ->
    let available =
      (match Version.of_string Release.version with Ok v -> [ Release.version, v ] | Error _ -> [])
      @ Home.installed ()
    in
    (match List.filter (fun (_, v) -> Version.compare v wanted >= 0) available with
     | [] ->
       fail
         (Printf.sprintf
            "no compiler satisfies every requirement.\n\
            \  ─ the graph requires cronyx >=%s\n\
            \  ─ this machine has %s\n\n\
            \  A compiler requirement is a floor and never a ceiling, so the newest available \
             would do; there is no version this high."
            (Version.to_string wanted)
            (match available with
             | [] -> "none"
             | _ -> String.concat ", " (List.map fst available)))
     | _ -> Ok { packages; cronyx = wanted })
