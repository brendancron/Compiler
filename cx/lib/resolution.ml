(* Working out what the build is made of: which packages, at which versions,
   and which compiler. With `path` dependencies each name has exactly one
   candidate, so what resolution has to catch is a graph that disagrees with
   itself -- two paths reaching one name at two versions -- and a compiler floor
   nothing on this machine reaches. Choosing between candidate versions is what
   arrives with a registry, and PubGrub with it. *)

open Bootstrap

type entry =
  { name : string
  ; version : Version.t
  ; source : string (* the path as the manifest wrote it, relative to the root *)
  ; dependencies : string list
  }

type t =
  { packages : entry list (* sorted by name *)
  ; cronyx : Version.t
  }

(* Who asked for this, in order, so a conflict can say how the graph got here. *)
type chain = (string * Version.t) list

let fail span message = Error [ Diagnostic.at Diagnostic.Manifest span message ]

(* Who wanted it, not what was wanted: the chain stops at the package that
   named this dependency. *)
let describe (chain : chain) =
  match chain with
  | [] -> "the root package"
  | chain ->
    String.concat
      " → "
      (List.map (fun (name, v) -> Printf.sprintf "%s %s" name (Version.to_string v)) chain)

let manifest_at root = Manifest.load (Filename.concat root Manifest.file_name)

let resolve root =
  let ( let* ) = Result.bind in
  let found : (string, entry * chain) Hashtbl.t = Hashtbl.create 8 in
  let floor = ref None in
  let raise_floor version =
    match !floor with
    | Some current when Version.compare current version >= 0 -> ()
    | _ -> floor := Some version
  in
  let rec walk ~chain ~source root =
    let* manifest = manifest_at root in
    let name = manifest.Manifest.name in
    let version = manifest.Manifest.version in
    raise_floor manifest.Manifest.cronyx;
    match Hashtbl.find_opt found name with
    | Some (existing, first) when not (Version.equal existing.version version) ->
      (* The shape the design doc asks for: what each side wants and how the
         graph reached it, rather than the bare fact that they differ. *)
      fail
        Source_map.Span.nowhere
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
      let entry =
        { name
        ; version
        ; source
        ; dependencies =
            List.map (fun (d : Manifest.dependency) -> d.Manifest.name) manifest.Manifest.dependencies
            |> List.sort String.compare
        }
      in
      Hashtbl.replace found name (entry, chain);
      List.fold_left
        (fun acc (d : Manifest.dependency) ->
          let* () = acc in
          match d.Manifest.source with
          | Manifest.Path path ->
            walk ~chain:(chain @ [ name, version ]) ~source:path (Filename.concat root path))
        (Ok ())
        manifest.Manifest.dependencies
  in
  let* () = walk ~chain:[] ~source:"." root in
  let packages =
    Hashtbl.fold (fun _ (entry, _) acc -> entry :: acc) found []
    |> List.sort (fun a b -> String.compare a.name b.name)
  in
  match !floor with
  | None -> fail Source_map.Span.nowhere "No package named a compiler version."
  | Some wanted ->
    (* Lower bound only. A package that could name an upper bound on the
       compiler makes the ecosystem unbuildable the first time one of them is
       abandoned, so the maximum over the graph always satisfies every floor and
       the only failure is a floor nothing reaches. *)
    let available =
      (match Version.of_string Release.version with Ok v -> [ Release.version, v ] | Error _ -> [])
      @ Home.installed ()
    in
    (match List.filter (fun (_, v) -> Version.compare v wanted >= 0) available with
     | [] ->
       fail
         Source_map.Span.nowhere
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
     (* The maximum over the graph, which is the version dispatch hands to and
        therefore the one to pin. *)
     | _ -> Ok { packages; cronyx = wanted })
