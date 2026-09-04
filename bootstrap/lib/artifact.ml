(* A compiled package: its declarations, already mangled and metaprocessed, and
   the names each of its units exports.

   There is no schema. The lockfile pins an exact compiler and every package in
   a build is compiled by that one binary, so an artifact is never read by a
   compiler other than the one that wrote it -- which is what lets this be
   `Marshal` of the compiler's own types, tagged with the version that wrote
   it. A compiler whose types have changed rejects what it finds and the
   package is built again. *)

type unit_interface =
  { namespace : string
  ; exports : string list
  }

type t =
  { compiler : string
  ; package : string
  ; units : unit_interface list
  ; program : Ast.program
  }

let extension = ".cxa"

let save path (artifact : t) =
  let out = Out_channel.open_bin path in
  Fun.protect
    ~finally:(fun () -> Out_channel.close out)
    (fun () -> Marshal.to_channel out artifact [])

type failure =
  | Missing
  | Stale of string (* the compiler that wrote it *)
  | Unreadable

let load path : (t, failure) result =
  if not (Sys.file_exists path)
  then Error Missing
  else (
    match
      let inp = In_channel.open_bin path in
      Fun.protect
        ~finally:(fun () -> In_channel.close inp)
        (fun () -> (Marshal.from_channel inp : t))
    with
    | artifact ->
      if String.equal artifact.compiler Release.version
      then Ok artifact
      else Error (Stale artifact.compiler)
    | exception _ -> Error Unreadable)

let interface (artifact : t) namespace =
  List.find_opt (fun u -> String.equal u.namespace namespace) artifact.units
