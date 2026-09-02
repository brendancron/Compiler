
let types : (string * int) list = []

let methods : (string * string * (unit -> Types.infer_ty list * Types.infer_ty)) list =
  [ "string", "bytes", (fun () -> [ Types.IStr ], Types.iarray Types.IByte)
    (* The one way a string becomes an identifier, checked. *)
  ; "string", "as_name", (fun () -> [ Types.IStr ], Types.iname)
  ]

(* No HM type describes these. A call is checked structurally; a bare reference
   is a function of no arguments, which stops one being passed around. *)
let variadic : (string * (unit -> Types.infer_ty)) list =
  [     Ast.generated [ "meta"; "emit" ], (fun () -> Types.IUnit)
  ; Ast.generated [ "meta"; "value" ], (fun () -> Types.IUnit)
      ; Ast.generated [ "meta"; "code" ], (fun () -> Types.icode)
  ]

let functions : (string * (unit -> Types.infer_ty list * Types.infer_ty)) list =
  [ "clock", (fun () -> [], Types.IFloat)
  ; ("print", fun () -> [ Types.fresh () ], Types.IUnit)
  ; ("str", fun () -> [ Types.fresh () ], Types.IStr)
  ; ("ord", fun () -> [ Types.IChr ], Types.IInt)
  ; ( "same"
    , fun () ->
        let t = Types.fresh () in
        [ t; t ], Types.IBool )
  (* What a derived `Eq` reaches, through an impl so a type has to ask. *)
  ; ( "__structural_eq"
    , fun () ->
        let t = Types.fresh () in
        [ t; t ], Types.IBool )
  ]

(* ---- values ---- *)

let values ~out =
  let native name arity apply = name, Value.Fn { Value.name; arity; apply } in
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
  [ one "print" (fun _ v ->
      out (Value.string_of_value v);
      out "\n";
      Value.Unit)
  ; one "str" (fun _ v -> Value.Str (Utf8.decode (Value.string_of_value v)))
  ; one "ord" (fun span v ->
      match v with
      | Value.Chr c -> Value.Int (Uchar.to_int c)
      | _ -> Value.fail span "Cannot apply ord to these arguments.")
  ; two "same" (fun _ a b -> Value.Bool (Value.same a b))
  ; two "__structural_eq" (fun _ a b -> Value.Bool (Value.values_equal a b))
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

let env ~out =
  let env = Value.new_env None in
  List.iter (fun (name, v) -> Value.define env name v) (values ~out);
  env
