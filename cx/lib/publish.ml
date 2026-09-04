(* Putting a package into a registry, and the rules that make what comes back
   out trustworthy: a published version is immutable, and what ships is
   declared rather than whatever happened to be in the directory. *)

open Bootstrap

let fail message = Error [ Diagnostic.at Diagnostic.Manifest Source_map.Span.nowhere message ]

(* A `path` dependency means nothing to whoever downloads this, so it has to
   carry a version as well -- and then the version is what the published
   manifest records. *)
let publishable (m : Manifest.t) =
  match
    List.find_opt
      (fun (d : Manifest.dependency) ->
        match d.Manifest.source with
        | Manifest.Path _ -> true
        | Manifest.Registry _ -> false)
      m.Manifest.dependencies
  with
  | None -> Ok ()
  | Some d ->
    Error
      [ Diagnostic.at
          Diagnostic.Manifest
          d.Manifest.span
          (Printf.sprintf
             "'%s' is a `path` dependency, which means nothing to anyone who downloads this. Give \
              it a version as well, or drop it before publishing."
             d.Manifest.name)
      ]

let release_toml (m : Manifest.t) ~checksum =
  let buffer = Buffer.create 256 in
  Buffer.add_string buffer (Printf.sprintf "checksum = \"%s\"\n" checksum);
  Buffer.add_string buffer "yanked = false\n";
  (match m.Manifest.dependencies with
   | [] -> ()
   | dependencies ->
     Buffer.add_string buffer "\n[dependencies]\n";
     List.iter
       (fun (d : Manifest.dependency) ->
         match d.Manifest.source with
         | Manifest.Registry requirement ->
           Buffer.add_string
             buffer
             (Printf.sprintf "%s = \"%s\"\n" d.Manifest.name (Requirement.to_string requirement))
         | Manifest.Path _ -> ())
       (List.sort
          (fun (a : Manifest.dependency) b -> String.compare a.Manifest.name b.Manifest.name)
          dependencies));
  Buffer.contents buffer

let publish root =
  let ( let* ) = Result.bind in
  match Registry_source.root () with
  | None -> fail "No registry is configured. Set CRONYX_REGISTRY."
  | Some registry ->
    let* manifest = Manifest.load (Filename.concat root Manifest.file_name) in
    let* () = publishable manifest in
    let name = manifest.Manifest.name in
    let version = Version.to_string manifest.Manifest.version in
    let release = Registry_source.release_path registry name version in
    (* Once a version exists, it is what it is: a registry that could serve
       different bytes for one version is a registry nothing can be pinned
       against. *)
    if Sys.file_exists release
    then fail (Printf.sprintf "%s %s is already published." name version)
    else (
      let archive = Archive.pack (Archive.of_directory root) in
      let checksum = Archive.checksum archive in
      Home.ensure (Filename.dirname release);
      Home.ensure (Registry_source.store registry);
      Out_channel.with_open_bin
        (Registry_source.archive_path registry name version)
        (fun out -> Out_channel.output_string out archive);
      Out_channel.with_open_bin release (fun out ->
        Out_channel.output_string out (release_toml manifest ~checksum));
      Ok (name, version, checksum))
