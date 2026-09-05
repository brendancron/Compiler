(* What a package is once it leaves its directory. A real registry serves a
   tarball; this serves the same bytes in a shape both ends here already agree
   on, so the checksum, the cache and the verification are the real ones and
   only the transport is a stand-in.

   Deterministic by construction: entries sorted by path, no timestamps, no
   modes. Two packings of one tree are the same bytes, which is what makes the
   checksum in a lockfile mean anything. *)

let magic = "cxar1\n"

let pack files =
  let buffer = Buffer.create 4096 in
  Buffer.add_string buffer magic;
  List.sort (fun (a, _) (b, _) -> String.compare a b) files
  |> List.iter (fun (path, contents) ->
    Buffer.add_string buffer (Printf.sprintf "%s\n%d\n" path (String.length contents));
    Buffer.add_string buffer contents);
  Buffer.contents buffer

type error = string

let unpack text : ((string * string) list, error) result =
  let n = String.length text in
  if n < String.length magic || not (String.equal (String.sub text 0 (String.length magic)) magic)
  then Error "not a Cronyx archive"
  else (
    let rec entries at acc =
      if at >= n
      then Ok (List.rev acc)
      else (
        match String.index_from_opt text at '\n' with
        | None -> Error "truncated archive"
        | Some path_end ->
          let path = String.sub text at (path_end - at) in
          (match String.index_from_opt text (path_end + 1) '\n' with
           | None -> Error "truncated archive"
           | Some size_end ->
             let size = String.sub text (path_end + 1) (size_end - path_end - 1) in
             (match int_of_string_opt size with
              | None -> Error (Printf.sprintf "'%s' is not a length" size)
              | Some size when size_end + 1 + size > n -> Error "truncated archive"
              | Some size ->
                entries (size_end + 1 + size) ((path, String.sub text (size_end + 1) size) :: acc))))
    in
    entries (String.length magic) [])

let checksum text = "sha256:" ^ Digest.to_hex (Digest.string text)

(* Every file under [root], by its path relative to it. *)
let of_directory root =
  let rec walk prefix dir =
    Sys.readdir dir
    |> Array.to_list
    |> List.sort String.compare
    |> List.concat_map (fun entry ->
      let path = Filename.concat dir entry in
      let relative = if String.equal prefix "" then entry else Filename.concat prefix entry in
      if Sys.is_directory path
      then
        (* `target/` is output. `tests/` at the root is compiled against the
           package rather than into it, and a consumer has neither the
           test-only dependencies to build it nor a reason to. *)
        if String.equal entry "target" || (String.equal prefix "" && String.equal entry "tests")
        then []
        else walk relative path
      else [ relative, In_channel.with_open_bin path In_channel.input_all ])
  in
  walk "" root

let into_directory root files =
  List.iter
    (fun (path, contents) ->
      let full = Filename.concat root path in
      Home.ensure (Filename.dirname full);
      Out_channel.with_open_bin full (fun out -> Out_channel.output_string out contents))
    files
