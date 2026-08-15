(* What a program can use without declaring it: the types, their signatures, the
   entries that make them containers, and the values that implement them.

   Nothing that consumes a program reads this. The checker registers these the
   same way it registers a declaration, and the interpreter is handed the values
   without being told which are builtin. *)

let types : (string * int) list = []

let methods : (string * string * (unit -> Types.infer_ty list * Types.infer_ty)) list =
  [ "string", "bytes", (fun () -> [ Types.IStr ], Types.iarray Types.IByte)
    (* The one way a string becomes an identifier, and it checks that it can
       be one. Without it a name could be any text a program assembled. *)
  ; "string", "as_name", (fun () -> [ Types.IStr ], Types.iname)
  ]

(* No HM type describes these: any number of arguments, of any type. A call is
   checked structurally against the declared result; a bare reference is a
   function of no arguments, which is what stops one being passed around as a
   value that could be anything. *)
let variadic : (string * (unit -> Types.infer_ty)) list =
  [ "print", (fun () -> Types.IUnit)
    (* What a lowered `gen` calls. The name is unforgeable, and it is bound only
       while a meta block runs. *)
  ; Ast.generated [ "meta"; "emit" ], (fun () -> Types.IUnit)
  ; Ast.generated [ "meta"; "value" ], (fun () -> Types.IUnit)
    (* What a lowered `code` calls, taking the same bindings a `gen` does. *)
  ; Ast.generated [ "meta"; "code" ], (fun () -> Types.icode)
  ]

let functions : (string * (unit -> Types.infer_ty list * Types.infer_ty)) list =
  [ "clock", (fun () -> [], Types.IFloat)
  ; ("str", fun () -> [ Types.fresh () ], Types.IStr)
    (* A scalar value is a code point, so this is the number it already is. *)
  ; ("ord", fun () -> [ Types.IChr ], Types.IInt)
  ; ( "same"
    , fun () ->
        let t = Types.fresh () in
        [ t; t ], Types.IBool )
  ]

(* ---- values ---- *)

let values ~out =
  let native name arity apply = name, Value.Fn { Value.name; arity; apply } in
  let variadic name apply = native name None apply in
  let two name f =
    native name (Some 2) (fun span args ->
      match args with
      | [ a; b ] -> f span a b
      | _ -> Value.fail span "Cannot apply %s to these arguments." name)
  in
  let one name f =
    native name (Some 1) (fun span args ->
      match args with
      | [ a ] -> f span a
      | _ -> Value.fail span "Cannot apply %s to these arguments." name)
  in
  [ variadic "print" (fun _ args ->
      out (String.concat " " (List.map Value.string_of_value args));
      out "\n";
      Value.Unit)
  ; one "str" (fun _ v -> Value.Str (Utf8.decode (Value.string_of_value v)))
  ; one "ord" (fun span v ->
      match v with
      | Value.Chr c -> Value.Int (Uchar.to_int c)
      | _ -> Value.fail span "Cannot apply ord to these arguments.")
  ; two "same" (fun _ a b -> Value.Bool (Value.same a b))
  ; native "clock" (Some 0) (fun _ _ -> Value.Float (Sys.time ()))
  ; one (Ast.method_name "string" "as_name") (fun span v ->
      match v with
      | Value.Str s ->
        let text = Utf8.encode s in
        let usable =
          String.length text > 0
          && (match text.[0] with
              | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
              | _ -> false)
          && String.for_all
               (function
                 | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
                 | _ -> false)
               text
        in
        if usable
        then Value.Name text
        else Value.fail span "'%s' cannot be a name." text
      | _ -> Value.fail span "Cannot apply as_name to these arguments.")
  ; one (Ast.method_name "string" "bytes") (fun span v ->
      match v with
      | Value.Str s ->
        let bytes = Utf8.encode s in
        Value.Array (Array.init (String.length bytes) (fun i -> Value.Byte bytes.[i]))
      | _ -> Value.fail span "Cannot apply bytes to these arguments.")
  ]

(* The scope a program starts in. *)
let env ~out =
  let env = Value.new_env None in
  List.iter (fun (name, v) -> Value.define env name v) (values ~out);
  env
