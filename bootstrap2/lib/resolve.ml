(* Turns every construct whose meaning depends on a type into a concrete
   primitive or call, using the types the checker inferred.

   Today that is compound assignment. `x += v` becomes `x = x + v` unless the
   type supplies an in-place entry, and no type does yet. Operators, indexing,
   and collection literals join it here as the registry grows. *)

let rec expr registry (e : Ast.typed_expr) : Ast.resolved_expr =
  let span = e.Ast.span
  and ann = e.Ast.ann in
  let it : Ast.resolved_expr_kind =
    match e.Ast.it with
    (* No in-place entry exists, so it derives from the plain operator. The
       target's type is the node's own, which is what the rebuilt read of it
       carries. *)
    | `Compound (op, name, v) ->
      let target : Ast.resolved_expr = { Ast.it = `Var name; span; ann } in
      let combined : Ast.resolved_expr =
        { Ast.it = `Binop (op, target, expr registry v); span; ann }
      in
      `Assign (name, combined)
    (* Selection happens here, once. A builtin stands as written; a declared
       operator becomes a call. *)
    | `Binop (op, a, b) ->
      let a = expr registry a
      and b = expr registry b in
      (match Registry.find registry op a.Ast.ann b.Ast.ann with
       | Some { Registry.emit = Registry.Call name; _ } ->
         let callee : Ast.resolved_expr =
           { Ast.it = `Var name
           ; span
           ; ann = Types.Fn ([ a.Ast.ann; b.Ast.ann ], ann, [])
           }
         in
         `Call (callee, [ a; b ])
       | _ -> `Binop (op, a, b))
    (* Only arrays exist so far, so every literal lowers to the primitive. *)
    | `Collection_lit items -> `Array_lit (List.map (expr registry) items)
    | #Ast.lit as l -> l
    | #Ast.vars as v -> (Ast.map_vars (expr registry) v :> Ast.resolved_expr_kind)
    | #Ast.ops as o -> (Ast.map_ops (expr registry) o :> Ast.resolved_expr_kind)
    | #Ast.logic as l -> (Ast.map_logic (expr registry) l :> Ast.resolved_expr_kind)
    | #Ast.indexing as i ->
      (Ast.map_indexing (expr registry) i :> Ast.resolved_expr_kind)
    | #Ast.tuple as t -> (Ast.map_tuple (expr registry) t :> Ast.resolved_expr_kind)
    (* Nominal identity was the checker's business; the value is a record. *)
    | `New (_, fields) ->
      `Record_lit (List.map (fun (l, v) -> l, expr registry v) fields)
    | `New_variant (_, variant, payload) ->
      `Variant
        (variant, List.map (fun (l, v) -> l, expr registry v) (Ast.payload_fields payload))
    | #Ast.record as r ->
      (Ast.map_record (expr registry) r :> Ast.resolved_expr_kind)
    | #Ast.reflect as r ->
      (Ast.map_reflect (expr registry) r :> Ast.resolved_expr_kind)
  in
  { Ast.it; span; ann }

let rec stmt registry (s : Ast.typed_stmt) : Ast.resolved_stmt =
  let it : Ast.resolved_stmt_kind =
    match s.Ast.it with
    | #Ast.stmts as st ->
      (Ast.map_stmts (expr registry) (stmt registry) st :> Ast.resolved_stmt_kind)
    | #Ast.effects as e ->
      (Ast.map_effects
         (expr registry)
         (stmt registry)
         (Ast.map_handler (stmt registry))
         e
       :> Ast.resolved_stmt_kind)
    (* An operator is an ordinary function under a derived name. *)
    | `Op_decl (op, params, signature, body) ->
      let operand (p : Ast.param) =
        match p.Ast.ty with
        | Some { Ast.it = Ast.Ty_name n; _ } -> n
        | Some { Ast.it = Ast.Ty_app (n, _); _ } -> n
        | _ -> "_"
      in
      (match params with
       | [ lhs; rhs ] ->
         `Fn
           ( Ast.op_name op (operand lhs) (operand rhs)
           , params
           , signature
           , List.map (stmt registry) body )
       | _ -> `Block [])
    | #Ast.type_defs as t -> t
    | #Ast.matching as m ->
      (Ast.map_matching (expr registry) (stmt registry) m :> Ast.resolved_stmt_kind)
  in
  { Ast.it; span = s.Ast.span; ann = s.Ast.ann }

let program ~registry (p : Ast.typed_stmt list) : Ast.resolved_stmt list =
  List.map (stmt registry) p
