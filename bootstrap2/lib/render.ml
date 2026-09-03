(* A diagnostic drawn as a frame. Every part of the frame is unconditional: the
   rows between the rules vary, the rules themselves do not, so no absent field
   can delete a line and leave the frame hanging open. *)

type palette =
  { bold : string
  ; red : string
  ; cyan : string
  ; magenta : string
  ; off : string
  }

let plain = { bold = ""; red = ""; cyan = ""; magenta = ""; off = "" }

let ansi =
  { bold = "\027[1m"
  ; red = "\027[1;31m"
  ; cyan = "\027[36m"
  ; magenta = "\027[35m"
  ; off = "\027[0m"
  }

(* A tab is a stop every eight columns in both the source line and the underline
   beneath it, so the two are measured the same way and line up. *)
let tab_width = 8

let expand text =
  let buf = Buffer.create (String.length text) in
  String.iter
    (fun c ->
      if c = '\t'
      then Buffer.add_string buf (String.make (tab_width - (Buffer.length buf mod tab_width)) ' ')
      else Buffer.add_char buf c)
    text;
  Buffer.contents buf

(* Columns taken by the first [bytes] bytes of [text], counting a UTF-8 sequence
   once rather than once per byte. *)
let width text bytes =
  let expanded = expand (String.sub text 0 (min bytes (String.length text))) in
  let columns = ref 0 in
  String.iter (fun c -> if Char.code c land 0xc0 <> 0x80 then incr columns) expanded;
  !columns

let frame ~palette ~entry (e : Diagnostic.error) =
  let buf = Buffer.create 256 in
  let add = Buffer.add_string buf in
  let located = Source_map.Span.view e.Diagnostic.span in
  (* As wide as the line number that will sit in it, so the rules and the source
     line hang off the same column. *)
  let gutter =
    match located with
    | Source_map.Span.Nowhere_in_source -> " "
    | Source_map.Span.Located l ->
      String.make (String.length (string_of_int l.Source_map.Span.line)) ' '
  in
  let rule text = add (Printf.sprintf "%s %s%s%s\n" gutter palette.cyan text palette.off) in
  add
    (Printf.sprintf
       "%s×%s %s%s error: %s%s\n"
       palette.red
       palette.off
       palette.bold
       (Diagnostic.stage_name e.Diagnostic.stage)
       e.Diagnostic.message
       palette.off);
  (match located with
   | Source_map.Span.Nowhere_in_source -> rule "┌─ (no source location)"
   | Source_map.Span.Located l ->
     let path = Ast.shown_path ~entry (Source_map.File.path l.Source_map.Span.file) in
     rule (Printf.sprintf "┌─ %s:%d:%d" path l.Source_map.Span.line l.Source_map.Span.col);
     let text = l.Source_map.Span.line_text in
     let before = width text l.Source_map.Span.underline_from in
     let marked = max 1 (width text l.Source_map.Span.underline_to - before) in
     rule "│";
     add
       (Printf.sprintf
          "%d %s│%s %s\n"
          l.Source_map.Span.line
          palette.cyan
          palette.off
          (expand text));
     add
       (Printf.sprintf
          "%s %s│%s %s%s%s%s\n"
          gutter
          palette.cyan
          palette.off
          (String.make before ' ')
          palette.magenta
          (String.make marked '~')
          palette.off));
  rule "└─";
  Buffer.contents buf

let colour_wanted channel =
  match Sys.getenv_opt "NO_COLOR" with
  | Some value when not (String.equal value "") -> false
  | _ -> Out_channel.isatty channel

let error ~entry e = frame ~palette:plain ~entry e

let emit ~entry errors =
  let palette = if colour_wanted stderr then ansi else plain in
  List.iter (fun e -> prerr_string (frame ~palette ~entry e)) errors;
  match errors with
  | [] | [ _ ] -> ()
  | many ->
    prerr_string
      (Printf.sprintf
         "%s×%s %s%d errors found.%s\n"
         palette.red
         palette.off
         palette.bold
         (List.length many)
         palette.off)
