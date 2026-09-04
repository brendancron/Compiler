(* Installing a toolchain, and the one rule that keeps dispatch working: the
   `cx` on $PATH only ever moves forward. Installing an older toolchain -- to
   reproduce a bug, say -- must not leave a dispatcher too old to read a current
   package. *)

open Bootstrap

let copy from into =
  let contents = In_channel.with_open_bin from In_channel.input_all in
  Out_channel.with_open_bin into (fun out -> Out_channel.output_string out contents);
  Unix.chmod into 0o755

type outcome =
  { version : string
  ; promoted : bool (* whether it became the `cx` on $PATH *)
  }

(* Asked before the new toolchain is on disk, since it answers by looking at
   what is. *)
let promote version =
  match Version.of_string version, Home.highest () with
  | Error _, _ -> false
  | Ok _, None -> true
  | Ok fresh, Some (_, highest) -> Version.compare fresh highest >= 0

(* [binary] is the `cx` to install as this version. There is no download yet;
   what a release would do is put the same file in the same place. *)
let install ~version ~binary =
  match Version.of_string version with
  | Error message -> Error message
  | Ok _ ->
    if not (Sys.file_exists binary)
    then Error (Printf.sprintf "'%s' does not exist." binary)
    else (
      let dir = Filename.concat (Home.toolchain version) "bin" in
      Home.ensure dir;
      let forward = promote version in
      copy binary (Home.toolchain_cx version);
      if forward
      then (
        Home.ensure (Home.bin ());
        copy binary (Home.cx ()));
      Ok { version; promoted = forward })

let list () = List.map fst (Home.installed ())
