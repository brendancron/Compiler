
(* An operator or method inside a generic body cannot be selected while the body
   is generic, so it is copied per concrete type its call sites use. *)
type state =
  { registry : Registry.t
  ; generic : (string, Ast.typed_stmt) Hashtbl.t
  ; copies : (string * string, string) Hashtbl.t
  ; mutable emitted : Ast.typed_stmt list
  ; mutable changed : bool
  }

let rec type_directed_expr (e : Ast.typed_expr) =
  let operand (child : Ast.typed_expr) = Types.has_generic child.Ast.ann in
  match e.Ast.it with
  | `Binop (_, a, b) -> operand a || operand b || type_directed_expr a || type_directed_expr b
  | `Compound (_, _, v) -> Types.has_generic e.Ast.ann || type_directed_expr v
  | `Method_call (receiver, _, _, args) ->
    operand receiver
    || type_directed_expr receiver
    || List.exists type_directed_expr args
  | `Collection_lit items | `Array_lit items ->
    Types.has_generic e.Ast.ann || List.exists type_directed_expr items
  | `Array_new (a, b) | `Array_get (a, b) -> type_directed_expr a || type_directed_expr b
  | `Array_set (a, b, c) ->
    type_directed_expr a || type_directed_expr b || type_directed_expr c
  | `Array_len a | `Str_len a -> type_directed_expr a
  | `Str_get (a, b) -> type_directed_expr a || type_directed_expr b
  | `Unop (_, a) | `Tuple_get (a, _) | `Field (a, _) | `Typeof a | `Assign (_, a) ->
    type_directed_expr a
  | `And (a, b) | `Or (a, b) | `Index (a, b) | `Field_assign (a, _, b) ->
    type_directed_expr a || type_directed_expr b
  | `Index_assign (a, b, c) ->
    type_directed_expr a || type_directed_expr b || type_directed_expr c
  (* A generic argument selects the callee's copy the way a generic receiver
     selects a method's, so the body holding the call has to be copied too. *)
  (* A body is copied for what its own call sites need; a lambda inside it is
     copied along with whatever holds it. *)
  | `Lambda _ -> false
  | `Call (callee, args) ->
    List.exists operand args
    || type_directed_expr callee
    || List.exists type_directed_expr args
  | `Tuple items -> List.exists type_directed_expr items
  | `New_call (_, _, args) -> List.exists type_directed_expr args
  | `Record_lit fields | `New (_, fields) ->
    List.exists (fun (_, v) -> type_directed_expr v) fields
  | `New_variant (_, _, payload) ->
    List.exists (fun (_, v) -> type_directed_expr v) (Ast.payload_fields payload)
  | #Ast.lit | `Var _ -> false

and type_directed (s : Ast.typed_stmt) =
  let expr = type_directed_expr in
  match s.Ast.it with
  | `Expr e | `Return (Some e) | `Var_decl (_, _, Some e) | `Resume (Some e) -> expr e
  | `Block body | `Fn (_, _, _, body) -> List.exists type_directed body
  | `If (cond, then_branch, else_branch) ->
    expr cond
    || type_directed then_branch
    || Option.fold ~none:false ~some:type_directed else_branch
  | `While (cond, body) -> expr cond || type_directed body
  | `Match (scrutinee, cases) ->
    expr scrutinee || List.exists (fun (_, body) -> List.exists type_directed body) cases
  | `Run (body, handlers) ->
    List.exists type_directed body
    || List.exists
         (fun (h : Ast.typed_stmt Ast.handler) ->
           List.exists
             (fun (a : Ast.typed_stmt Ast.arm) -> List.exists type_directed a.Ast.arm_body)
             h.Ast.arms)
         handlers
  | `Impl_decl (_, _, _, methods) ->
    List.exists
      (fun (m : (Ast.typed_stmt, Types.ty) Ast.method_def) ->
        List.exists type_directed m.Ast.md_body)
      methods
  | `Op_decl (_, _, _, body) -> List.exists type_directed body
  | _ -> false

let rec subst_expr mapping (e : Ast.typed_expr) : Ast.typed_expr =
  let it : Ast.typed_expr_kind =
    match e.Ast.it with
    | #Ast.lit as l -> l
    | #Ast.arrays as a -> (Ast.map_arrays (subst_expr mapping) a :> Ast.typed_expr_kind)
    | #Ast.strings as s -> (Ast.map_strings (subst_expr mapping) s :> Ast.typed_expr_kind)
    | #Ast.vars as v -> (Ast.map_vars (subst_expr mapping) v :> Ast.typed_expr_kind)
    | #Ast.ops as o -> (Ast.map_ops (subst_expr mapping) o :> Ast.typed_expr_kind)
    | #Ast.logic as l -> (Ast.map_logic (subst_expr mapping) l :> Ast.typed_expr_kind)
    | #Ast.compound as c -> (Ast.map_compound (subst_expr mapping) c :> Ast.typed_expr_kind)
    | #Ast.indexing as i -> (Ast.map_indexing (subst_expr mapping) i :> Ast.typed_expr_kind)
    | #Ast.tuple as t -> (Ast.map_tuple (subst_expr mapping) t :> Ast.typed_expr_kind)
    | #Ast.record as r -> (Ast.map_record (subst_expr mapping) r :> Ast.typed_expr_kind)
    | #Ast.nominal as n -> (Ast.map_nominal (subst_expr mapping) n :> Ast.typed_expr_kind)
    | #Ast.collection as c ->
      (Ast.map_collection (subst_expr mapping) c :> Ast.typed_expr_kind)
    | #Ast.method_call as m ->
      (Ast.map_method_call (subst_expr mapping) m :> Ast.typed_expr_kind)
    | #Ast.reflect as r -> (Ast.map_reflect (subst_expr mapping) r :> Ast.typed_expr_kind)
    | `Lambda (params, signature, body) ->
      `Lambda (params, signature, List.map (subst_stmt mapping) body)
  in
  { e with Ast.it; ann = Types.subst_generic mapping e.Ast.ann }

and subst_stmt mapping (s : Ast.typed_stmt) : Ast.typed_stmt =
  let it : Ast.typed_stmt_kind =
    match s.Ast.it with
    | #Ast.stmts as st ->
      (Ast.map_stmts (subst_expr mapping) (subst_stmt mapping) st
       :> Ast.typed_stmt_kind)
    | #Ast.effects as e ->
      (Ast.map_effects
         (subst_expr mapping)
         (subst_stmt mapping)
         (Ast.map_handler (subst_stmt mapping))
         e
       :> Ast.typed_stmt_kind)
    | #Ast.type_defs as t -> t
    | #Ast.op_defs as o -> (Ast.map_op_defs (subst_stmt mapping) o :> Ast.typed_stmt_kind)
    | #Ast.method_defs as m ->
      (Ast.map_method_defs (subst_stmt mapping) (Types.subst_generic mapping) m
       :> Ast.typed_stmt_kind)
    | #Ast.matching as m ->
      (Ast.map_matching (subst_expr mapping) (subst_stmt mapping) m
       :> Ast.typed_stmt_kind)
  in
  { s with Ast.it; ann = Types.subst_generic mapping s.Ast.ann }

let copy_for state name (at : Types.ty) =
  let key = Types.string_of_ty at in
  match Hashtbl.find_opt state.copies (name, key) with
  | Some existing -> existing
  | None ->
    let copy = Ast.generated [ name; string_of_int (Hashtbl.length state.copies) ] in
    Hashtbl.replace state.copies (name, key) copy;
    let declaration = Hashtbl.find state.generic name in
    (match declaration.Ast.it with
     | `Fn (_, params, signature, body) ->
       let mapping = Types.match_generic declaration.Ast.ann at [] in
       let specialized =
         subst_stmt mapping { declaration with Ast.it = `Fn (copy, params, signature, body) }
       in
       state.emitted <- specialized :: state.emitted;
       state.changed <- true
     | _ -> ());
    copy

(* The row comes from the template rather than the call site: the CPS pass reads
   it off the callee to decide what evidence to pass. *)
let method_call_type state name (receiver : Ast.typed_expr) args result =
  let row =
    match Hashtbl.find_opt state.generic name with
    | Some { Ast.ann = Types.Fn (_, _, row); _ } -> row
    | _ -> []
  in
  Types.Fn
    ( receiver.Ast.ann :: List.map (fun (a : Ast.typed_expr) -> a.Ast.ann) args
    , result
    , row )

let rec rewrite state (e : Ast.typed_expr) : Ast.typed_expr =
  let it : Ast.typed_expr_kind =
    match e.Ast.it with
    | `Lambda (params, signature, body) ->
      `Lambda (params, signature, List.map (rewrite_stmt state) body)
    (* A method on a generic type is copied like a function, and its call
       becomes an ordinary one so [Resolve] has nothing left to select. *)
    | `Method_call (receiver, name, as_function, args) ->
      let receiver = rewrite state receiver in
      let args = List.map (rewrite state) args in
      let owned =
        match Types.type_name receiver.Ast.ann with
        | Some owner -> Some (Ast.method_name owner name)
        | None -> None
      in
      (match owned with
       | Some mangled when Hashtbl.mem state.generic mangled ->
         let at = method_call_type state mangled receiver args e.Ast.ann in
         if Types.has_generic at
         then `Method_call (receiver, name, as_function, args)
         else (
           let copy = copy_for state mangled at in
           let passed =
             match Types.type_name receiver.Ast.ann with
             | Some owner when Registry.is_associated state.registry owner name -> args
             | _ -> receiver :: args
           in
           `Call ({ receiver with Ast.it = `Var copy; ann = at }, passed))
       | _ -> `Method_call (receiver, name, as_function, args))
    | `Call (callee, args) ->
      let args = List.map (rewrite state) args in
      (match callee.Ast.it with
       | `Var name
         when Hashtbl.mem state.generic name && not (Types.has_generic callee.Ast.ann) ->
         let copy = copy_for state name callee.Ast.ann in
         `Call ({ callee with Ast.it = `Var copy }, args)
       | _ -> `Call (rewrite state callee, args))
    | #Ast.lit as l -> l
    | #Ast.arrays as a -> (Ast.map_arrays (rewrite state) a :> Ast.typed_expr_kind)
    | #Ast.strings as s -> (Ast.map_strings (rewrite state) s :> Ast.typed_expr_kind)
    | #Ast.vars as v -> (Ast.map_vars (rewrite state) v :> Ast.typed_expr_kind)
    | #Ast.ops as o -> (Ast.map_ops (rewrite state) o :> Ast.typed_expr_kind)
    | #Ast.logic as l -> (Ast.map_logic (rewrite state) l :> Ast.typed_expr_kind)
    | #Ast.compound as c -> (Ast.map_compound (rewrite state) c :> Ast.typed_expr_kind)
    | #Ast.indexing as i -> (Ast.map_indexing (rewrite state) i :> Ast.typed_expr_kind)
    | #Ast.tuple as t -> (Ast.map_tuple (rewrite state) t :> Ast.typed_expr_kind)
    | #Ast.record as r -> (Ast.map_record (rewrite state) r :> Ast.typed_expr_kind)
    | #Ast.nominal as n -> (Ast.map_nominal (rewrite state) n :> Ast.typed_expr_kind)
    | #Ast.collection as c ->
      (Ast.map_collection (rewrite state) c :> Ast.typed_expr_kind)
    | #Ast.reflect as r -> (Ast.map_reflect (rewrite state) r :> Ast.typed_expr_kind)
  in
  { e with Ast.it }

and rewrite_stmt state (s : Ast.typed_stmt) : Ast.typed_stmt =
  let it : Ast.typed_stmt_kind =
    match s.Ast.it with
    | #Ast.stmts as st ->
      (Ast.map_stmts (rewrite state) (rewrite_stmt state) st :> Ast.typed_stmt_kind)
    | #Ast.effects as e ->
      (Ast.map_effects
         (rewrite state)
         (rewrite_stmt state)
         (Ast.map_handler (rewrite_stmt state))
         e
       :> Ast.typed_stmt_kind)
    | #Ast.type_defs as t -> t
    | #Ast.op_defs as o -> (Ast.map_op_defs (rewrite_stmt state) o :> Ast.typed_stmt_kind)
    | #Ast.method_defs as m ->
      (Ast.map_method_defs (rewrite_stmt state) Fun.id m :> Ast.typed_stmt_kind)
    | #Ast.matching as m ->
      (Ast.map_matching (rewrite state) (rewrite_stmt state) m :> Ast.typed_stmt_kind)
  in
  { s with Ast.it }

let rec collect state (s : Ast.typed_stmt) =
  match s.Ast.it with
  | `Fn (name, _, _, body) ->
    if Types.has_generic s.Ast.ann && type_directed s
    then Hashtbl.replace state.generic name s;
    List.iter (collect state) body
  (* A method is a function whose first parameter is the receiver, so its
     template is one — the copy is emitted as a plain [`Fn]. *)
  | `Impl_decl (_, type_name, _, methods) ->
    List.iter
      (fun (m : (Ast.typed_stmt, Types.ty) Ast.method_def) ->
        let mangled = Ast.method_name type_name m.Ast.md_name in
        if Types.has_generic m.Ast.md_ann && List.exists type_directed m.Ast.md_body
        then
          Hashtbl.replace
            state.generic
            mangled
            { s with
              Ast.it = `Fn (mangled, m.Ast.md_params, m.Ast.md_signature, m.Ast.md_body)
            ; ann = m.Ast.md_ann
            };
        List.iter (collect state) m.Ast.md_body)
      methods
  | `Block body -> List.iter (collect state) body
  | `If (_, then_branch, else_branch) ->
    collect state then_branch;
    Option.iter (collect state) else_branch
  | `While (_, body) -> collect state body
  | `Run (body, _) -> List.iter (collect state) body
  | `Match (_, cases) ->
    List.iter (fun (_, body) -> List.iter (collect state) body) cases
  | _ -> ()

(* A template is dropped once its copies exist: its own body still names types
   nothing can select on, and every call has been pointed elsewhere. *)
let is_template state (s : Ast.typed_stmt) =
  match s.Ast.it with
  | `Fn (name, _, _, _) -> Hashtbl.mem state.generic name
  | _ -> false

let program ~registry (p : Ast.typed_stmt list) : Ast.typed_stmt list =
  let state =
    { registry
    ; generic = Hashtbl.create 8
    ; copies = Hashtbl.create 8
    ; emitted = []
    ; changed = false
    }
  in
  List.iter (collect state) p;
  if Hashtbl.length state.generic = 0
  then p
  else (
    let rewritten =
      List.filter_map
        (fun s -> if is_template state s then None else Some (rewrite_stmt state s))
        p
    in
    (* A copy may itself call a template at a type nothing has asked for yet. *)
    let rec drain acc =
      let fresh = state.emitted in
      state.emitted <- [];
      state.changed <- false;
      let done_ = List.rev_map (rewrite_stmt state) fresh in
      if state.changed then drain (done_ @ acc) else done_ @ acc
    in
    drain [] @ rewritten)
