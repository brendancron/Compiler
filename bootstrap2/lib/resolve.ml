(* Turns every construct whose meaning depends on a type into a concrete
   primitive or call, using the types the checker inferred.

   Compound assignment, operator selection, and method dispatch all land here.
   A statement may expand into several, because an `impl` is a group of
   functions written under one header. *)

(* The effect row of every function this pass emits, by the name it emits it
   under. A synthesized call has to describe the same function the declaration
   does: the CPS pass reads the row off the callee to decide what evidence to
   pass, and a call that claimed purity would be given none. *)
let declared_rows : (string, Types.row) Hashtbl.t = Hashtbl.create 16

let row_of (t : Types.ty) =
  match t with
  | Types.Fn (_, _, row) -> row
  | _ -> []

let rec record (s : Ast.typed_stmt) =
  match s.Ast.it with
  | `Impl_decl (_, type_name, _, methods) ->
    List.iter
      (fun (m : (Ast.typed_stmt, Types.ty) Ast.method_def) ->
        Hashtbl.replace
          declared_rows
          (Ast.method_name type_name m.Ast.md_name)
          (row_of m.Ast.md_ann);
        List.iter record m.Ast.md_body)
      methods
  | `Op_decl (op, params, _, body) ->
    (match params with
     | [ lhs; rhs ] ->
       Hashtbl.replace
         declared_rows
         (Ast.op_name op (operand lhs) (operand rhs))
         (row_of s.Ast.ann)
     | _ -> ());
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

(* An operator's operands are named as written, which is how [Registry] keyed
   the entry the checker made. *)
and operand (p : Ast.param) =
  match p.Ast.ty with
  | Some { Ast.it = Ast.Ty_name n; _ } -> n
  | Some { Ast.it = Ast.Ty_app (n, _); _ } -> n
  | _ -> "_"

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
         `Call (fn_ref span name [ a; b ] ann, [ a; b ])
       | _ -> `Binop (op, a, b))
    (* The receiver becomes the first argument, which is the shape the method
       was compiled under. *)
    | `Method_call (receiver, name, args) ->
      let receiver = expr registry receiver in
      let args = List.map (expr registry) args in
      let owner =
        match Types.type_name receiver.Ast.ann with
        | Some owner -> owner
        | None -> assert false (* the checker dispatched on this name *)
      in
      let all = receiver :: args in
      `Call (fn_ref span (Ast.method_name owner name) all ann, all)
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

(* A reference to the function a lowered construct calls, typed from the
   arguments it is about to be given. *)
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
  (* An operator is an ordinary function under a derived name. *)
  | `Op_decl (op, params, signature, body) ->
    (match params with
     | [ lhs; rhs ] ->
       [ node
           (`Fn
             ( Ast.op_name op (operand lhs) (operand rhs)
             , params
             , signature
             , block registry body ))
       ]
     | _ -> [])
  (* Each method becomes a function taking the receiver first, which is what
     [`Method_call] was rewritten to call. *)
  | `Impl_decl (_, type_name, _, methods) ->
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
      methods
  | `Trait_decl _ -> []
  | #Ast.type_defs as t -> [ node t ]
  | #Ast.matching as m ->
    [ node (Ast.map_matching (expr registry) (one registry) m :> Ast.resolved_stmt_kind) ]

and block registry body = List.concat_map (stmt registry) body

(* Where only one statement fits. An `impl` in such a position would be scoped
   to it, which is no use to anyone, so nothing is lost by wrapping. *)
and one registry (s : Ast.typed_stmt) : Ast.resolved_stmt =
  match stmt registry s with
  | [ single ] -> single
  | many -> { Ast.it = `Block many; span = s.Ast.span; ann = s.Ast.ann }

let program ~registry (p : Ast.typed_stmt list) : Ast.resolved_stmt list =
  Hashtbl.reset declared_rows;
  List.iter record p;
  block registry p
