(* After checking, because that is when the annotation exists; before
   evaluation, so the interpreter never sees the node.

   A `Type` is not a runtime value: it answers questions about a type while
   compiling, and each answer is folded here into the data it names. What is
   left is ordinary Cronyx — a string, a record, a variant.

   Answers are one level deep. A field reports its name, not its type, so a
   type that mentions itself describes itself in finite space; giving a field
   the type it holds is what will force the answer to be built on demand
   instead of here. *)

type error =
  { span : Ast.span
  ; message : string
  }

exception Failed of error

let fail span fmt =
  Printf.ksprintf (fun message -> raise (Failed { span; message })) fmt

let node span ann it : Ast.reflected_expr = { Ast.it; span; ann }

let string_at span text = node span Types.Str (`Str (Utf8.decode text))

let record_at span name fields values =
  node span (Types.Named (name, [], fields)) (`Record_lit values)

let array_at span elem items =
  node span (Types.array elem) (`Array_lit items)

let name_at span text = node span Types.name (`Name text)
let field_ty = Types.Named (Types.field_name, [], [ "name", Types.name ])

let variant_ty =
  Types.Named (Types.variant_name, [], [ "arity", Types.Int; "name", Types.name ])

let shape_ty = Types.Sum (Types.shape_name, [])

let shape_at span variant payload =
  node span shape_ty (`Variant (variant, payload))

(* A product carries its fields in the type; a sum names its variants nowhere
   but the table the checker built, so reflection reads that. *)
let shape_of span (ty : Types.ty) =
  let one name = shape_at span name [] in
  match ty with
  | Types.Int | Types.Float | Types.Str | Types.Byte | Types.Chr | Types.Bool
  | Types.Unit -> one "Scalar"
  | Types.Named (name, _, fields) when not (String.equal name Types.array_name) ->
    let each (label, _) =
      record_at span Types.field_name [ "name", Types.name ] [ "name", name_at span label ]
    in
    shape_at
      span
      "Product"
      [ "0", name_at span name; "1", array_at span field_ty (List.map each fields) ]
  | Types.Sum (name, _) ->
    let variants =
      match Hashtbl.find_opt Typecheck.ctx_types name with
      | Some (Typecheck.Sum (_, variants)) -> variants
      | _ -> []
    in
    let each (label, (declared : Typecheck.variant_decl)) =
      let arity =
        match declared.Typecheck.vd_payload with
        | Ast.P_none -> 0
        | Ast.P_tuple items -> List.length items
        | Ast.P_fields items -> List.length items
      in
      record_at
        span
        Types.variant_name
        [ "arity", Types.Int; "name", Types.name ]
        [ "arity", node span Types.Int (`Int arity); "name", name_at span label ]
    in
    shape_at
      span
      "Sum"
      [ "0", name_at span name; "1", array_at span variant_ty (List.map each variants) ]
  | _ -> one "Other"

let rec expr (e : Ast.resolved_expr) : Ast.reflected_expr =
  let it : Ast.reflected_expr_kind =
    match e.Ast.it with
    | `Field ({ Ast.it = `Typeof inner; _ }, "name") ->
      (string_at e.Ast.span (Types.string_of_ty inner.Ast.ann)).Ast.it
    | `Field ({ Ast.it = `Typeof inner; _ }, "shape") ->
      (shape_of e.Ast.span inner.Ast.ann).Ast.it
    | `Typeof _ ->
      fail
        e.Ast.span
        "A type is not a value here. Ask for what you need of it, as \
         'typeof(x).name' or 'typeof(x).shape'."
    | #Ast.lit as l -> l
    | #Ast.arrays as a -> (Ast.map_arrays expr a :> Ast.reflected_expr_kind)
    | #Ast.strings as s -> (Ast.map_strings expr s :> Ast.reflected_expr_kind)
    | #Ast.vars as v -> (Ast.map_vars expr v :> Ast.reflected_expr_kind)
    | #Ast.ops as o -> (Ast.map_ops expr o :> Ast.reflected_expr_kind)
    | #Ast.logic as l -> (Ast.map_logic expr l :> Ast.reflected_expr_kind)
    | #Ast.tuple as t -> (Ast.map_tuple expr t :> Ast.reflected_expr_kind)
    | #Ast.record as r -> (Ast.map_record expr r :> Ast.reflected_expr_kind)
    | `Lambda (params, signature, body) ->
      `Lambda (params, signature, List.map stmt body)
    | #Ast.variant_lit as v ->
      (Ast.map_variant_lit expr v :> Ast.reflected_expr_kind)
  in
  { Ast.it; span = e.Ast.span; ann = e.Ast.ann }

and stmt (s : Ast.resolved_stmt) : Ast.reflected_stmt =
  let it : Ast.reflected_stmt_kind =
    match s.Ast.it with
    | #Ast.stmts as st -> (Ast.map_stmts expr stmt st :> Ast.reflected_stmt_kind)
    | #Ast.effects as e -> (Ast.map_effects expr stmt (Ast.map_handler stmt) e :> Ast.reflected_stmt_kind)
    | #Ast.type_defs as t -> t
    | #Ast.matching as m ->
      (Ast.map_matching expr stmt m :> Ast.reflected_stmt_kind)
  in
  { Ast.it; span = s.Ast.span; ann = s.Ast.ann }

let program (p : Ast.resolved_stmt list) : (Ast.reflected_stmt list, error) result =
  try Ok (List.map stmt p) with
  | Failed e -> Error e
