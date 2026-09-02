(* Every construct whose meaning depends on a type becomes a primitive or a
   call here. A statement may expand into several: an `impl` is a group of
   functions written under one header. *)

(* The CPS pass reads a callee's row to decide what evidence to pass, so a
   synthesized call claiming purity would be given none. *)
let declared_rows : (string, Types.row) Hashtbl.t = Hashtbl.create 16

let row_of (t : Types.ty) =
  match t with
  | Types.Fn (_, _, row) -> row
  | _ -> []

let rec record (s : Ast.typed_stmt) =
  match s.Ast.it with
  | `Impl_decl (_, type_name, _, impl) ->
    List.iter
      (fun (m : (Ast.typed_stmt, Types.ty) Ast.method_def) ->
        Hashtbl.replace
          declared_rows
          (Ast.method_name type_name m.Ast.md_name)
          (row_of m.Ast.md_ann);
        List.iter record m.Ast.md_body)
      impl.Ast.ib_methods
  | `Op_decl (op, params, signature, body) ->
    Hashtbl.replace declared_rows (Ast.op_entry_name op params signature) (row_of s.Ast.ann);
    List.iter record body
  | `Block body | `Fn (_, _, _, body) -> List.iter record body
  | `If (_, then_branch, else_branch) ->
    record then_branch;
    Option.iter record else_branch
  | `While (_, body) -> record body
  | `Run (body, handlers) ->
    List.iter record body;
    List.iter
      (fun (h : Ast.typed_stmt Ast.handler) ->
        List.iter
          (fun (a : Ast.typed_stmt Ast.arm) -> List.iter record a.Ast.arm_body)
          h.Ast.arms)
      handlers
  | `Match (_, cases) -> List.iter (fun (_, body) -> List.iter record body) cases
  | _ -> ()

type error =
  { span : Ast.span
  ; message : string
  }

exception Failed of error

let fail span fmt =
  Printf.ksprintf (fun message -> raise (Failed { span; message })) fmt

let accessor registry (target : Ast.resolved_expr) (index : Ast.resolved_expr) pick =
  let by_index owner =
    Registry.indexed registry owner (Option.value (Types.type_name index.Ast.ann) ~default:"")
  in
  match Option.bind (Types.type_name target.Ast.ann) by_index with
  | Some entry ->
    (match pick entry with
     | Some name -> name
     | None ->
       fail target.Ast.span "%s cannot be assigned into." (Types.string_of_ty target.Ast.ann))
  | None ->
    fail target.Ast.span "Cannot index %s." (Types.string_of_ty target.Ast.ann)

let rec expr registry (e : Ast.typed_expr) : Ast.resolved_expr =
  let span = e.Ast.span
  and ann = e.Ast.ann in
  let it : Ast.resolved_expr_kind =
    match e.Ast.it with
    (* No in-place entry exists, so it derives from the plain operator. *)
    | `Compound (op, name, v) ->
      let target : Ast.resolved_expr = { Ast.it = `Var name; span; ann } in
      let combined : Ast.resolved_expr =
        { Ast.it = `Binop (op, target, expr registry v); span; ann }
      in
      `Assign (name, combined)
    | `Binop (op, a, b) ->
      let a = expr registry a
      and b = expr registry b in
      (match Registry.find registry op a.Ast.ann b.Ast.ann with
       | Some { Registry.emit = Registry.Call name; _ } ->
         `Call (fn_ref span name [ a; b ] ann, [ a; b ])
       (* Spelled out rather than caught by a wildcard, so a third emission
          form has to be decided here instead of quietly staying an operator. *)
       | Some { Registry.emit = Registry.Primitive; _ } | None -> `Binop (op, a, b))
    | `Method_call (receiver, name, _, args) ->
      let receiver = expr registry receiver in
      let args = List.map (expr registry) args in
      let owner =
        match Types.type_name receiver.Ast.ann with
        | Some owner -> owner
        | None ->
          fail
            receiver.Ast.span
            "Cannot call '%s': the receiver's type is not known here."
            name
      in
      (* A receiver the checker could not name is only known to be an array
         here, so the length intrinsic is selected in both places. *)
      if String.equal name Types.array_len && args = [] && Types.is_array receiver.Ast.ann
      then `Array_len receiver
      else if String.equal name Types.array_len && args = [] && receiver.Ast.ann = Types.Str
      then `Str_len receiver
      else (
        (* An associated function takes no receiver, so what stood in for it is
           not an argument. *)
        let all =
          if Registry.is_associated registry owner name then args else receiver :: args
        in
        `Call (fn_ref span (Ast.method_name owner name) all ann, all))
    | `Collection_lit items ->
      let items = List.map (expr registry) items in
      if Types.is_array ann
      then `Array_lit items
      else (
        let unbuildable () =
          fail span "%s cannot be built from a literal." (Types.string_of_ty ann)
        in
        match Types.type_name ann with
        | None -> unbuildable ()
        (* Anything but an array is built from one holding its elements, and
           what it holds comes from the entry rather than from its arity. *)
        | Some name ->
          (match Registry.container registry name, Registry.container_element registry name (Types.of_ty ann) with
           | Some make, Some element ->
             let elements : Ast.resolved_expr =
               { Ast.it = `Array_lit items
               ; span
               ; ann = Types.array (Types.resolve element)
               }
             in
             `Call (fn_ref span make.Registry.entry [ elements ] ann, [ elements ])
           | _ -> unbuildable ()))
    | `Index (target, index) ->
      let target = expr registry target
      and index = expr registry index in
      (* The primitive reads take an int. Anything else is an entry the type
         declared, including the range a slice is. *)
      if index.Ast.ann <> Types.Int
      then (
        let name = accessor registry target index (fun entry -> entry.Registry.get) in
        `Call (fn_ref span name [ target; index ] ann, [ target; index ]))
      else if target.Ast.ann = Types.Str
      then `Str_get (target, index)
      else if Types.is_array target.Ast.ann
      then `Array_get (target, index)
      else (
        let name = accessor registry target index (fun entry -> entry.Registry.get) in
        `Call (fn_ref span name [ target; index ] ann, [ target; index ]))
    | `Index_assign (target, index, v) ->
      let target = expr registry target
      and index = expr registry index
      and v = expr registry v in
      if Types.is_array target.Ast.ann
      then `Array_set (target, index, v)
      else (
        let args = [ target; index; v ] in
        let name = accessor registry target index (fun entry -> entry.Registry.set) in
        `Call (fn_ref span name args ann, args))
    | #Ast.lit as l -> l
    | #Ast.arrays as a -> (Ast.map_arrays (expr registry) a :> Ast.resolved_expr_kind)
    | #Ast.strings as s -> (Ast.map_strings (expr registry) s :> Ast.resolved_expr_kind)
    | #Ast.vars as v -> (Ast.map_vars (expr registry) v :> Ast.resolved_expr_kind)
    | #Ast.ops as o -> (Ast.map_ops (expr registry) o :> Ast.resolved_expr_kind)
    | #Ast.logic as l -> (Ast.map_logic (expr registry) l :> Ast.resolved_expr_kind)
    | #Ast.tuple as t -> (Ast.map_tuple (expr registry) t :> Ast.resolved_expr_kind)
    (* The checker turned every constructor call into an ordinary one. *)
    | `New_call _ -> assert false
    | `New (_, fields) ->
      `Record_lit (List.map (fun (l, v) -> l, expr registry v) fields)
    | `New_variant (_, variant, payload) ->
      `Variant
        (variant, List.map (fun (l, v) -> l, expr registry v) (Ast.payload_fields payload))
    | #Ast.record as r ->
      (Ast.map_record (expr registry) r :> Ast.resolved_expr_kind)
    | `Lambda (params, signature, body) ->
      `Lambda (params, signature, List.concat_map (stmt registry) body)
    | #Ast.reflect as r ->
      (Ast.map_reflect (expr registry) r :> Ast.resolved_expr_kind)
  in
  (* A deferred method call carries whatever type its caller pinned; the length
     intrinsics above answer with an int whatever that was. *)
  let ann =
    match it with
    | `Array_len _ | `Str_len _ -> Types.Int
    | _ -> ann
  in
  { Ast.it; span; ann }

and fn_ref span name args result : Ast.resolved_expr =
  { Ast.it = `Var name
  ; span
  ; ann =
      Types.Fn
        ( List.map (fun (a : Ast.resolved_expr) -> a.Ast.ann) args
        , result
        , Option.value ~default:[] (Hashtbl.find_opt declared_rows name) )
  }

and stmt registry (s : Ast.typed_stmt) : Ast.resolved_stmt list =
  let span = s.Ast.span
  and ann = s.Ast.ann in
  let node it : Ast.resolved_stmt = { Ast.it; span; ann } in
  match s.Ast.it with
  (* [`Block] and [`Fn] hold statement lists, which is where an expansion has
     room to land; the rest hold single statements. *)
  | `Block body -> [ node (`Block (block registry body)) ]
  | `Fn (name, params, signature, body) ->
    [ node (`Fn (name, params, signature, block registry body)) ]
  | #Ast.stmts as st ->
    [ node (Ast.map_stmts (expr registry) (one registry) st :> Ast.resolved_stmt_kind) ]
  | #Ast.effects as e ->
    [ node
        (Ast.map_effects (expr registry) (one registry) (Ast.map_handler (one registry)) e
         :> Ast.resolved_stmt_kind)
    ]
  | `Op_decl (op, params, signature, body) ->
    [ node
        (`Fn (Ast.op_entry_name op params signature, params, signature, block registry body))
    ]
  | `Impl_decl (_, type_name, _, impl) ->
    List.map
      (fun (m : (Ast.typed_stmt, Types.ty) Ast.method_def) ->
        { Ast.it =
            (`Fn
            ( Ast.method_name type_name m.Ast.md_name
            , m.Ast.md_params
            , m.Ast.md_signature
              , block registry m.Ast.md_body )
             :> Ast.resolved_stmt_kind)
        ; span
        ; ann = m.Ast.md_ann
        })
      impl.Ast.ib_methods
  | `Trait_decl _ -> []
  | #Ast.type_defs as t -> [ node t ]
  | #Ast.matching as m ->
    [ node (Ast.map_matching (expr registry) (one registry) m :> Ast.resolved_stmt_kind) ]

and block registry body = List.concat_map (stmt registry) body

and one registry (s : Ast.typed_stmt) : Ast.resolved_stmt =
  match stmt registry s with
  | [ single ] -> single
  | many -> { Ast.it = `Block many; span = s.Ast.span; ann = s.Ast.ann }

let program ~registry (p : Ast.typed_stmt list)
  : (Ast.resolved_stmt list, error) result
  =
  Hashtbl.reset declared_rows;
  List.iter record p;
  try Ok (block registry p) with
  | Failed e -> Error e
