(* A node's type is not finished when it is first visited — in
   `fn f(x) { return x + 1; }`, `x` is only pinned at the `+` — so the tree is
   built with mutable types and converted once. *)

type error =
  { span : Ast.span
  ; message : string
  }

exception Located of error

exception Deferred

type checked_expr = (checked_expr_kind, Types.infer_ty) Ast.node

and checked_expr_kind =
  [ Ast.lit
  | checked_expr Ast.vars
  | checked_expr Ast.ops
  | checked_expr Ast.logic
  | checked_expr Ast.compound
  | checked_expr Ast.indexing
  | checked_expr Ast.tuple
  | checked_expr Ast.record
  | checked_expr Ast.nominal
  | checked_expr Ast.collection
  | checked_expr Ast.method_call
  | checked_expr Ast.reflect
  ]

type checked_stmt = (checked_stmt_kind, Types.infer_ty) Ast.node

and checked_stmt_kind =
  [ (checked_expr, checked_stmt) Ast.stmts
  | (checked_expr, checked_stmt, checked_stmt Ast.handler) Ast.effects
  | Ast.type_defs
  | (checked_expr, checked_stmt) Ast.matching
  | checked_stmt Ast.op_defs
  | (checked_stmt, Types.infer_ty) Ast.method_defs
  ]

type env =
  { bindings : (string, Types.scheme) Hashtbl.t
  ; parent : env option
  }

type ctx =
  { registry : Registry.t
  ; mutable return_type : Types.infer_ty option
  ; mutable saw_return : bool
  ; mutable row : Types.infer_row
  ; mutable resume_type : Types.infer_ty option
  }

type effect_info =
  { ops : (string, Ast.op_decl) Hashtbl.t
  ; declared : (string, Ast.op_decl list) Hashtbl.t
  }

let ctx_effects = { ops = Hashtbl.create 16; declared = Hashtbl.create 8 }

type decl =
  | Opaque of Types.infer_ty list
  | Product of Types.infer_ty list * Types.infer_fields
  | Sum of Types.infer_ty list * (string * Types.infer_ty Ast.payload) list

let ctx_types : (string, decl) Hashtbl.t = Hashtbl.create 16

let ctx_type_params : (string, Types.infer_ty) Hashtbl.t = Hashtbl.create 8

let decl_params = function
  | Opaque vars | Product (vars, _) | Sum (vars, _) -> vars

let params_of_decl name =
  match Hashtbl.find_opt ctx_types name with
  | Some decl -> decl_params decl
  | None -> []

let instance vars args = List.map2 (fun v a -> Types.var_id v, a) vars args

let ctx_fn_params : (string, (string * Types.infer_ty) list) Hashtbl.t =
  Hashtbl.create 16

let with_type_params assoc f =
  let saved =
    List.map (fun (name, _) -> name, Hashtbl.find_opt ctx_type_params name) assoc
  in
  List.iter (fun (name, var) -> Hashtbl.replace ctx_type_params name var) assoc;
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun (name, previous) ->
          match previous with
          | Some var -> Hashtbl.replace ctx_type_params name var
          | None -> Hashtbl.remove ctx_type_params name)
        saved)
    f

let ctx_traits : (string, Ast.method_sig list) Hashtbl.t = Hashtbl.create 8
let ctx_methods : (string * string, unit) Hashtbl.t = Hashtbl.create 32

let reset_effects () =
  Hashtbl.reset ctx_effects.ops;
  Hashtbl.reset ctx_effects.declared;
  Hashtbl.reset ctx_types;
  Hashtbl.reset ctx_traits;
  Hashtbl.reset ctx_methods;
  Hashtbl.reset ctx_type_params;
  Hashtbl.reset ctx_fn_params

let new_env parent = { bindings = Hashtbl.create 16; parent }
let bind env name scheme = Hashtbl.replace env.bindings name scheme

let rec lookup env name =
  match Hashtbl.find_opt env.bindings name with
  | Some scheme -> Some scheme
  | None ->
    (match env.parent with
     | Some p -> lookup p name
     | None -> None)

(* Generalization must not quantify a variable the enclosing scope still uses. *)
let env_free_vars env =
  let acc = ref [] in
  let rec walk env =
    Hashtbl.iter
      (fun _ (scheme : Types.scheme) ->
        Types.free_vars scheme.body
        |> List.iter (fun (id, _) ->
          if (not (List.mem id scheme.quantified)) && not (List.mem id !acc)
          then acc := id :: !acc))
      env.bindings;
    match env.parent with
    | Some p -> walk p
    | None -> ()
  in
  walk env;
  !acc

let env_free_row_vars env =
  let acc = ref [] in
  let rec walk env =
    Hashtbl.iter
      (fun _ (scheme : Types.scheme) ->
        Types.free_row_vars scheme.body
        |> List.iter (fun id ->
          if (not (List.mem id scheme.quantified_rows)) && not (List.mem id !acc)
          then acc := id :: !acc))
      env.bindings;
    match env.parent with
    | Some p -> walk p
    | None -> ()
  in
  walk env;
  !acc

let env_free_field_vars env =
  let acc = ref [] in
  let rec walk env =
    Hashtbl.iter
      (fun _ (scheme : Types.scheme) ->
        Types.free_field_vars scheme.body
        |> List.iter (fun id ->
          if (not (List.mem id scheme.quantified_fields)) && not (List.mem id !acc)
          then acc := id :: !acc))
      env.bindings;
    match env.parent with
    | Some p -> walk p
    | None -> ()
  in
  walk env;
  !acc

let fail span fmt =
  Printf.ksprintf (fun message -> raise (Located { span; message })) fmt

let unify_at span expected actual =
  try Types.unify expected actual with
  | Types.Type_error message -> raise (Located { span; message })

let rec infer_ty_of_annotation (t : Ast.type_expr) : Types.infer_ty =
  match t.Ast.it with
  | Ast.Ty_name "int" -> Types.IInt
  | Ast.Ty_name "float" -> Types.IFloat
  | Ast.Ty_name "string" -> Types.IStr
  | Ast.Ty_name "bool" -> Types.IBool
  | Ast.Ty_name "unit" -> Types.IUnit
  | Ast.Ty_tuple items -> Types.ITuple (List.map infer_ty_of_annotation items)
  | Ast.Ty_record fields ->
    Types.IRecord
      (List.fold_right
         (fun (l, t) rest -> Types.FCons (l, infer_ty_of_annotation t, rest))
         fields
         Types.FEmpty)
  | Ast.Ty_app (name, args) ->
    named_type t.Ast.span name (List.map infer_ty_of_annotation args)
  | Ast.Ty_name other ->
    (match Hashtbl.find_opt ctx_type_params other with
     | Some var -> var
     | None -> named_type t.Ast.span other [])
  | Ast.Ty_fn (params, ret, row) ->
    Types.IFn
      ( List.map infer_ty_of_annotation params
      , infer_ty_of_annotation ret
      , row_of_labels row )

and named_type span name args =
  match Hashtbl.find_opt ctx_types name with
  | None -> fail span "Unknown type '%s'." name
  | Some decl ->
    let vars = decl_params decl in
    if List.length vars <> List.length args
    then
      fail
        span
        "Type '%s' takes %d argument(s) but %d were given."
        name
        (List.length vars)
        (List.length args);
    (match decl with
     | Opaque _ -> Types.INamed (name, args, Types.FEmpty)
     | Product (_, fields) ->
       Types.INamed (name, args, Types.substitute_fields (instance vars args) fields)
     | Sum _ -> Types.ISum (name, args))

and row_of_labels labels =
  List.fold_right (fun label rest -> Types.RCons (label, rest)) labels Types.REmpty

let type_params_of span (comptime : Ast.comptime_param list) =
  List.map
    (fun (p : Ast.comptime_param) ->
      match p.Ast.cp_ty with
      | None ->
        let var = Types.fresh () in
        Types.declare_param var;
        p.Ast.cp_name, var
      | Some _ ->
        fail
          span
          "Comptime value parameter '%s' is not supported yet."
          p.Ast.cp_name)
    comptime

let annotated_or_fresh = function
  | Some t -> infer_ty_of_annotation t
  | None -> Types.fresh ()

(* Generalizing a binding that is later assigned is unsound: every use
   instantiates fresh, so `var f = someGenericFn; f = otherFn;` would check. *)
let is_syntactic_value (e : Ast.desugared_expr) =
  match e.Ast.it with
  | #Ast.lit | `Var _ -> true
  | _ -> false

let rec assigned_in_expr (e : Ast.desugared_expr) acc =
  match e.Ast.it with
  | #Ast.lit | `Var _ -> acc
  | `Assign (name, v) | `Compound (_, name, v) -> assigned_in_expr v (name :: acc)
  | `Unop (_, a) -> assigned_in_expr a acc
  | `Binop (_, a, b) | `And (a, b) | `Or (a, b) ->
    assigned_in_expr b (assigned_in_expr a acc)
  | `Call (callee, args) ->
    List.fold_left (fun acc a -> assigned_in_expr a acc) (assigned_in_expr callee acc) args
  | `Typeof e -> assigned_in_expr e acc
  | `Method_call (receiver, _, args) | `Comptime_call (receiver, _, args) ->
    List.fold_left
      (fun acc a -> assigned_in_expr a acc)
      (assigned_in_expr receiver acc)
      args
  | `Index (a, b) -> assigned_in_expr b (assigned_in_expr a acc)
  | `Index_assign (a, b, c) ->
    assigned_in_expr c (assigned_in_expr b (assigned_in_expr a acc))
  | `Collection_lit items | `Tuple items ->
    List.fold_left (fun acc i -> assigned_in_expr i acc) acc items
  | `Tuple_get (t, _) | `Field (t, _) -> assigned_in_expr t acc
  | `Record_lit fields ->
    List.fold_left (fun acc (_, v) -> assigned_in_expr v acc) acc fields
  | `Field_assign (r, _, v) -> assigned_in_expr v (assigned_in_expr r acc)
  | `New (_, fields) ->
    List.fold_left (fun acc (_, v) -> assigned_in_expr v acc) acc fields
  | `New_variant (_, _, payload) ->
    List.fold_left
      (fun acc (_, v) -> assigned_in_expr v acc)
      acc
      (Ast.payload_fields payload)

let rec assigned_in_stmt (s : Ast.desugared_stmt) acc =
  let opt f o acc =
    match o with
    | Some x -> f x acc
    | None -> acc
  in
  match s.Ast.it with
  | `Expr e -> assigned_in_expr e acc
  | `Var_decl (_, _, init) -> opt assigned_in_expr init acc
  | `Block body | `Fn (_, _, _, body) ->
    List.fold_left (fun acc st -> assigned_in_stmt st acc) acc body
  | `If (cond, then_branch, else_branch) ->
    opt assigned_in_stmt else_branch (assigned_in_stmt then_branch (assigned_in_expr cond acc))
  | `While (cond, body) -> assigned_in_stmt body (assigned_in_expr cond acc)
  | `Return e -> opt assigned_in_expr e acc
  | `Effect_decl _ | `Type_decl _ | `Trait_decl _ -> acc
  | `Op_decl (_, _, _, body) ->
    List.fold_left (fun acc st -> assigned_in_stmt st acc) acc body
  | `Impl_decl (_, _, _, methods) ->
    List.fold_left
      (fun acc (m : (Ast.desugared_stmt, unit) Ast.method_def) ->
        List.fold_left (fun acc st -> assigned_in_stmt st acc) acc m.Ast.md_body)
      acc
      methods
  | `Match (scrutinee, cases) ->
    List.fold_left
      (fun acc (_, body) ->
        List.fold_left (fun acc st -> assigned_in_stmt st acc) acc body)
      (assigned_in_expr scrutinee acc)
      cases
  | `Resume e -> opt assigned_in_expr e acc
  | `Run (body, handlers) ->
    let acc = List.fold_left (fun acc st -> assigned_in_stmt st acc) acc body in
    List.fold_left
      (fun acc (h : Ast.desugared_stmt Ast.handler) ->
        List.fold_left
          (fun acc (a : Ast.desugared_stmt Ast.arm) ->
            List.fold_left (fun acc st -> assigned_in_stmt st acc) acc a.Ast.arm_body)
          acc
          h.Ast.arms)
      acc
      handlers

let assigned_names body =
  List.fold_left (fun acc s -> assigned_in_stmt s acc) [] body

let variadic_builtins = [ "print" ]

let field_of (target : checked_expr) label =
  match Types.repr target.Ast.ann with
  | Types.INamed (name, _, fields) ->
    let rec find f =
      match Types.repr_fields f with
      | Types.FCons (l, ty, _) when String.equal l label -> Some ty
      | Types.FCons (_, _, rest) -> find rest
      | _ -> None
    in
    (match find fields with
     | Some ty -> ty
     | None -> fail target.Ast.span "Type '%s' has no field '%s'." name label)
  | _ ->
    let ty = Types.fresh () in
    unify_at
      target.Ast.span
      (Types.IRecord (Types.FCons (label, ty, Types.fresh_fields ())))
      target.Ast.ann;
    ty

let element_of registry (target : checked_expr) =
  match Types.concrete target.Ast.ann with
  | Some other ->
    (match Types.container_element (Types.of_ty other) with
     | Some (name, elem) when Registry.indexed registry name <> None -> elem
     | _ -> fail target.Ast.span "Cannot index %s." (Types.string_of_ty other))
  | None ->
    let elem = Types.fresh () in
    unify_at target.Ast.span (Types.fresh_with (Types.Collection elem)) target.Ast.ann;
    elem

let binop_result registry (op : Ast.binop) a b =
  match Types.concrete a, Types.concrete b with
  | Some lhs, Some rhs ->
    (match Registry.find registry op lhs rhs with
     | Some entry -> Types.of_ty (Registry.result_of entry lhs)
     | None ->
       Types.error
         "No operator %s for %s and %s."
         (Ast.string_of_binop op)
         (Types.string_of_ty lhs)
         (Types.string_of_ty rhs))
  | _ ->
    (* Unifying here would make an asymmetric operator unreachable, so it only
       happens when there is nothing to look up. *)
    Types.unify a b;
    Types.unify a (Types.fresh_with (Registry.constraint_of op));
    Registry.unresolved_result op a

let rec infer_expr env ctx (e : Ast.desugared_expr) : checked_expr =
  try infer_expr_impl env ctx e with
  | Types.Type_error message -> raise (Located { span = e.Ast.span; message })

and infer_expr_impl env ctx (e : Ast.desugared_expr) : checked_expr =
  let span = e.Ast.span in
  let node ty it : checked_expr = Ast.annotated span ty it in
  match e.Ast.it with
  | `Int n -> node Types.IInt (`Int n)
  | `Float n -> node Types.IFloat (`Float n)
  | `Str s -> node Types.IStr (`Str s)
  | `Bool b -> node Types.IBool (`Bool b)
  | `Var name ->
    (match lookup env name with
     | Some scheme -> node (Types.instantiate scheme) (`Var name)
     | None -> fail span "Undefined variable '%s'." name)
  | `Assign (name, v) ->
    (match lookup env name with
     | None -> fail span "Undefined variable '%s'." name
     | Some scheme ->
       let target = Types.instantiate scheme in
       let value = infer_expr env ctx v in
       Types.unify target value.Ast.ann;
       node target (`Assign (name, value)))
  | `Unop (Ast.Neg, a) ->
    let a = infer_expr env ctx a in
    Types.unify a.Ast.ann (Types.fresh_with Types.Numeric);
    node a.Ast.ann (`Unop (Ast.Neg, a))
  | `Unop (Ast.Not, a) ->
    let a = infer_expr env ctx a in
    Types.unify Types.IBool a.Ast.ann;
    node Types.IBool (`Unop (Ast.Not, a))
  | `Binop (op, a, b) ->
    let a = infer_expr env ctx a in
    let b = infer_expr env ctx b in
    node (binop_result ctx.registry op a.Ast.ann b.Ast.ann) (`Binop (op, a, b))
  | `Compound (op, name, v) ->
    (match lookup env name with
     | None -> fail span "Undefined variable '%s'." name
     | Some scheme ->
       let target = Types.instantiate scheme in
       let v = infer_expr env ctx v in
       let result = binop_result ctx.registry op target v.Ast.ann in
       Types.unify target result;
       let it : checked_expr_kind = `Compound (op, name, v) in
       node target it)
  | `And (a, b) | `Or (a, b) ->
    let a = infer_expr env ctx a in
    let b = infer_expr env ctx b in
    Types.unify Types.IBool a.Ast.ann;
    Types.unify Types.IBool b.Ast.ann;
    let it : checked_expr_kind =
      match e.Ast.it with
      | `And _ -> `And (a, b)
      | _ -> `Or (a, b)
    in
    node Types.IBool it
  | `Call (callee, args) ->
    let args = List.map (infer_expr env ctx) args in
    let callee_node = infer_expr env ctx callee in
    let result =
      match callee.Ast.it with
      | `Var name when List.mem name variadic_builtins -> Types.IUnit
      | _ ->
        let ret = Types.fresh () in
        let row = Types.fresh_row () in
        Types.unify
          callee_node.Ast.ann
          (Types.IFn (List.map (fun (a : checked_expr) -> a.Ast.ann) args, ret, row));
        Types.unify_row row ctx.row;
        ret
    in
    node result (`Call (callee_node, args))
  | `Comptime_call (callee, comptime_args, args) ->
    let name =
      match callee.Ast.it with
      | `Var name -> name
      | _ -> fail span "Only a named function takes comptime arguments."
    in
    let scheme =
      match lookup env name with
      | Some scheme -> scheme
      | None -> fail span "Undefined variable '%s'." name
    in
    let declared = Option.value ~default:[] (Hashtbl.find_opt ctx_fn_params name) in
    if List.length declared <> List.length comptime_args
    then
      fail
        span
        "'%s' takes %d comptime argument(s) but %d were given."
        name
        (List.length declared)
        (List.length comptime_args);
    let type_args =
      List.map
        (function
          | Ast.Ct_type t -> t
          | Ast.Ct_value _ ->
            fail span "A comptime argument to '%s' is not known at compile time." name)
        comptime_args
    in
    let bound, pinned =
      List.fold_left2
        (fun (bound, pinned) (_, var) written ->
          let written = infer_ty_of_annotation written in
          match Types.repr var with
          | Types.IVar { contents = Types.Unbound (id, _) } ->
            (id, written) :: bound, pinned
          | other -> bound, (other, written) :: pinned)
        ([], [])
        declared
        type_args
    in
    let fn = Types.instantiate ~bound scheme in
    List.iter (fun (a, b) -> unify_at span a b) pinned;
    let args = List.map (infer_expr env ctx) args in
    let callee_node : checked_expr = Ast.annotated callee.Ast.span fn (`Var name) in
    let ret = Types.fresh () in
    let row = Types.fresh_row () in
    Types.unify
      fn
      (Types.IFn (List.map (fun (a : checked_expr) -> a.Ast.ann) args, ret, row));
    Types.unify_row row ctx.row;
    node ret (`Call (callee_node, args))
  | `Method_call (receiver, name, args) ->
    let receiver = infer_expr env ctx receiver in
    let args = List.map (infer_expr env ctx) args in
    (try
    let owner =
      match Types.infer_type_name receiver.Ast.ann with
      | Some owner -> owner
      | None ->
        (match Types.repr receiver.Ast.ann with
         | Types.IVar _ ->
           let owners =
             Hashtbl.fold
               (fun (owner, declared) _ acc ->
                 if String.equal declared name then owner :: acc else acc)
               ctx_methods
               []
           in
           (match List.sort_uniq compare owners with
            | [] -> fail span "No type has a method '%s'." name
            | [ only ] -> only
            | _ -> raise Deferred)
         | other ->
           fail
             span
             "Cannot call '%s' on %s, which no impl can name."
             name
             (Types.string_of_infer_ty other))
    in
    let missing () = fail span "Type '%s' has no method '%s'." owner name in
    if not (Hashtbl.mem ctx_methods (owner, name)) then missing ();
    let fn =
      match lookup env (Ast.method_name owner name) with
      | Some scheme -> Types.instantiate scheme
      | None -> missing ()
    in
    (match Types.repr fn with
     | Types.IFn (params, _, _) when List.length params <> List.length args + 1 ->
       fail
         span
         "Method '%s' takes %d argument(s) but %d were passed."
         name
         (List.length params - 1)
         (List.length args)
     | _ -> ());
    let ret = Types.fresh () in
    let row = Types.fresh_row () in
    Types.unify
      fn
      (Types.IFn
         ( receiver.Ast.ann :: List.map (fun (a : checked_expr) -> a.Ast.ann) args
         , ret
         , row ));
    Types.unify_row row ctx.row;
    node ret (`Method_call (receiver, name, args))
     with
     | Deferred -> node (Types.fresh ()) (`Method_call (receiver, name, args)))
  | `Typeof e -> node Types.IStr (`Typeof (infer_expr env ctx e))
  | `Collection_lit items ->
    let elem = Types.fresh () in
    let container = Types.fresh_with (Types.Collection elem) in
    let items = List.map (infer_expr env ctx) items in
    List.iter
      (fun (i : checked_expr) -> unify_at i.Ast.span elem i.Ast.ann)
      items;
    node container (`Collection_lit items)
  | `New (name, fields) ->
    (match Hashtbl.find_opt ctx_types name with
     | Some (Opaque _) -> fail span "'%s' cannot be constructed with 'new'." name
     | None | Some (Sum _) -> fail span "Unknown record type '%s'." name
     | Some (Product (vars, declared)) ->
       let args = List.map (fun _ -> Types.fresh ()) vars in
       let declared = Types.substitute_fields (instance vars args) declared in
       let rec labels f =
         match Types.repr_fields f with
         | Types.FEmpty | Types.FVar _ -> []
         | Types.FCons (l, ty, rest) -> (l, ty) :: labels rest
       in
       let expected = labels declared in
       let fields = List.map (fun (l, v) -> l, infer_expr env ctx v) fields in
       List.iter
         (fun (l, _) ->
           if not (List.mem_assoc l expected)
           then fail span "Type '%s' has no field '%s'." name l)
         fields;
       List.iter
         (fun (l, _) ->
           if not (List.mem_assoc l fields)
           then fail span "Field '%s' is missing." l)
         expected;
       List.iter
         (fun (l, (v : checked_expr)) ->
           unify_at v.Ast.span (List.assoc l expected) v.Ast.ann)
         fields;
       node (Types.INamed (name, args, declared)) (`New (name, fields)))
  | `New_variant (ty, variant, payload) ->
    (match Hashtbl.find_opt ctx_types ty with
     | None | Some (Product _) | Some (Opaque _) ->
       fail span "Unknown sum type '%s'." ty
     | Some (Sum (vars, variants)) ->
       (match List.assoc_opt variant variants with
        | None -> fail span "Type '%s' has no variant '%s'." ty variant
        | Some declared ->
          let args = List.map (fun _ -> Types.fresh ()) vars in
          let mapping = instance vars args in
          let expected =
            List.map (fun (l, t) -> l, Types.substitute mapping t) (Ast.payload_fields declared)
          in
          let given =
            List.map (fun (l, v) -> l, infer_expr env ctx v) (Ast.payload_fields payload)
          in
          if List.length expected <> List.length given
          then
            fail
              span
              "Variant '%s' carries %d value(s) but %d were given."
              variant
              (List.length expected)
              (List.length given);
          List.iter
            (fun (l, (v : checked_expr)) ->
              match List.assoc_opt l expected with
              | Some ty -> unify_at v.Ast.span ty v.Ast.ann
              | None -> fail span "Variant '%s' has no field '%s'." variant l)
            given;
          let payload =
            match payload with
            | Ast.P_none -> Ast.P_none
            | Ast.P_tuple _ -> Ast.P_tuple (List.map snd given)
            | Ast.P_fields _ -> Ast.P_fields given
          in
          node (Types.ISum (ty, args)) (`New_variant (ty, variant, payload))))
  | `Record_lit fields ->
    let fields = List.map (fun (l, v) -> l, infer_expr env ctx v) fields in
    node
      (Types.IRecord
         (List.fold_right
            (fun (l, (v : checked_expr)) rest -> Types.FCons (l, v.Ast.ann, rest))
            fields
            Types.FEmpty))
      (`Record_lit fields)
  | `Field (target, label) ->
    let target = infer_expr env ctx target in
    node (field_of target label) (`Field (target, label))
  | `Field_assign (target, label, v) ->
    let target = infer_expr env ctx target in
    let v = infer_expr env ctx v in
    unify_at v.Ast.span (field_of target label) v.Ast.ann;
    node v.Ast.ann (`Field_assign (target, label, v))
  | `Tuple items ->
    let items = List.map (infer_expr env ctx) items in
    node
      (Types.ITuple (List.map (fun (i : checked_expr) -> i.Ast.ann) items))
      (`Tuple items)
  | `Tuple_get (target, index) ->
    let target = infer_expr env ctx target in
    (match Types.repr target.Ast.ann with
     | Types.ITuple items ->
       (match List.nth_opt items index with
        | Some ty -> node ty (`Tuple_get (target, index))
        | None ->
          fail
            span
            "A tuple of %d element(s) has no field %d."
            (List.length items)
            index)
     | Types.IVar _ ->
       fail
         target.Ast.span
         "The type of this tuple is not known here, so field %d cannot be resolved."
         index
     | other ->
       fail
         target.Ast.span
         "Cannot take a field of %s."
         (Types.string_of_infer_ty other))
  | `Index (target, index) ->
    let target = infer_expr env ctx target in
    let index = infer_expr env ctx index in
    unify_at index.Ast.span Types.IInt index.Ast.ann;
    node (element_of ctx.registry target) (`Index (target, index))
  | `Index_assign (target, index, v) ->
    let target = infer_expr env ctx target in
    let index = infer_expr env ctx index in
    let v = infer_expr env ctx v in
    unify_at index.Ast.span Types.IInt index.Ast.ann;
    unify_at v.Ast.span (element_of ctx.registry target) v.Ast.ann;
    node v.Ast.ann (`Index_assign (target, index, v))

and declare_ops registry (body : Ast.desugared_stmt list) =
  List.iter
    (fun (s : Ast.desugared_stmt) ->
      match s.Ast.it with
      | `Op_decl (op, params, signature, _) ->
        (match params with
         | [ lhs; rhs ] ->
           let operand (p : Ast.param) =
             match p.Ast.ty with
             | Some t ->
               (match Types.concrete (infer_ty_of_annotation t) with
                | Some ty -> ty
                | None -> fail s.Ast.span "An operator's operands must be concrete types.")
             | None -> fail s.Ast.span "An operator's operands must be annotated."
           in
           let lhs_ty = operand lhs
           and rhs_ty = operand rhs in
           let result =
             match signature.Ast.ret with
             | Some t ->
               (match Types.concrete (infer_ty_of_annotation t) with
                | Some ty -> ty
                | None -> fail s.Ast.span "An operator's result must be a concrete type.")
             | None -> fail s.Ast.span "An operator must declare its result type."
           in
           if Registry.find_exact registry op lhs_ty rhs_ty <> None
           then
             fail
               s.Ast.span
               "Operator %s is already defined for %s and %s."
               (Ast.string_of_binop op)
               (Types.string_of_ty lhs_ty)
               (Types.string_of_ty rhs_ty);
           let name =
             Ast.op_name
               op
               (Types.string_of_ty lhs_ty)
               (Types.string_of_ty rhs_ty)
           in
           Registry.register
             registry
             op
             lhs_ty
             rhs_ty
             { Registry.result = Some result; emit = Registry.Call name }
         | _ -> fail s.Ast.span "An operator takes exactly two operands.")
      | _ -> ())
    body

and declare_traits (body : Ast.desugared_stmt list) =
  List.iter
    (fun (s : Ast.desugared_stmt) ->
      match s.Ast.it with
      | `Trait_decl (name, methods) ->
        if Hashtbl.mem ctx_traits name
        then fail s.Ast.span "Trait '%s' is already declared." name;
        Hashtbl.replace ctx_traits name methods
      | _ -> ())
    body

and declare_impls (body : Ast.desugared_stmt list) =
  List.iter
    (fun (s : Ast.desugared_stmt) ->
      match s.Ast.it with
      | `Impl_decl (trait, type_name, params, methods) ->
        let span = s.Ast.span in
        let type_params = List.map (fun name -> name, Types.fresh ()) params in
        let supplies name =
          List.exists
            (fun (m : (Ast.desugared_stmt, unit) Ast.method_def) ->
              String.equal m.Ast.md_name name)
            methods
        in
        with_type_params type_params (fun () -> ignore (self_ty span type_name params));
        (match trait with
         | None -> ()
         | Some trait ->
           (match Hashtbl.find_opt ctx_traits trait with
            | None -> fail span "Unknown trait '%s'." trait
            | Some required ->
              List.iter
                (fun (r : Ast.method_sig) ->
                  if not (supplies r.Ast.ms_name)
                  then
                    fail
                      span
                      "'%s' for '%s' is missing method '%s'."
                      trait
                      type_name
                      r.Ast.ms_name)
                required));
        List.iter
          (fun (m : (Ast.desugared_stmt, unit) Ast.method_def) ->
            (match m.Ast.md_params with
             | { Ast.name = "self"; _ } :: _ -> ()
             | _ ->
               fail
                 span
                 "Method '%s' must take 'self' as its first parameter."
                 m.Ast.md_name);
            if Hashtbl.mem ctx_methods (type_name, m.Ast.md_name)
            then
              fail
                span
                "Type '%s' already has a method '%s'."
                type_name
                m.Ast.md_name;
            Hashtbl.replace ctx_methods (type_name, m.Ast.md_name) ())
          methods
      | _ -> ())
    body

and self_ty span type_name params =
  if String.equal type_name "Array"
  then Types.iopaque "Array" [ Types.fresh () ]
  else if params = []
  then infer_ty_of_annotation (Ast.at span (Ast.Ty_name type_name))
  else
    named_type
      span
      type_name
      (List.map
         (fun name ->
           match Hashtbl.find_opt ctx_type_params name with
           | Some var -> var
           | None -> Types.fresh ())
         params)

and declare_types (body : Ast.desugared_stmt list) =
  List.iter
    (fun (s : Ast.desugared_stmt) ->
      match s.Ast.it with
      | `Type_decl (name, params, body) ->
        if Hashtbl.mem ctx_types name
        then fail s.Ast.span "Type '%s' is already declared." name;
        let type_params = List.map (fun name -> name, Types.fresh ()) params in
        let vars = List.map snd type_params in
        Hashtbl.replace ctx_types name (Product (vars, Types.FEmpty));
        let declared =
          with_type_params type_params (fun () ->
            match body with
            | Ast.T_fields fields ->
              Product
                ( vars
                , List.fold_right
                    (fun (l, t) rest -> Types.FCons (l, infer_ty_of_annotation t, rest))
                    fields
                    Types.FEmpty )
            | Ast.T_variants variants ->
              Sum
                ( vars
                , List.map
                    (fun (v : Ast.variant) ->
                      v.Ast.v_name, Ast.map_payload infer_ty_of_annotation v.Ast.v_payload)
                    variants ))
        in
        Hashtbl.replace ctx_types name declared
      | _ -> ())
    body

and hoist env (body : Ast.desugared_stmt list) =
  List.iter
    (fun (s : Ast.desugared_stmt) ->
      match s.Ast.it with
      | `Op_decl (op, params, signature, _) ->
        ignore (type_params_of s.Ast.span signature.Ast.comptime);
        let operand (p : Ast.param) =
          match p.Ast.ty with
          | Some t -> Types.string_of_ty (Option.get (Types.concrete (infer_ty_of_annotation t)))
          | None -> "_"
        in
        (match params with
         | [ lhs; rhs ] ->
           let param_types =
             List.map (fun (p : Ast.param) -> annotated_or_fresh p.Ast.ty) params
           in
           bind
             env
             (Ast.op_name op (operand lhs) (operand rhs))
             (Types.mono
                (Types.IFn
                   ( param_types
                   , annotated_or_fresh signature.Ast.ret
                   , Types.fresh_row () )))
         | _ -> ())
      | `Impl_decl (_, type_name, params, methods) ->
        let impl_params = List.map (fun name -> name, Types.fresh ()) params in
        List.iter
          (fun (m : (Ast.desugared_stmt, unit) Ast.method_def) ->
            match m.Ast.md_params with
            | [] -> ()
            | _ :: rest ->
              let mangled = Ast.method_name type_name m.Ast.md_name in
              let type_params =
                impl_params
                @ type_params_of s.Ast.span m.Ast.md_signature.Ast.comptime
              in
              Hashtbl.replace ctx_fn_params mangled type_params;
              with_type_params type_params (fun () ->
                let param_types =
                  self_ty s.Ast.span type_name params
                  :: List.map (fun (p : Ast.param) -> annotated_or_fresh p.Ast.ty) rest
                in
                let row =
                  match m.Ast.md_signature.Ast.row with
                  | Some labels -> row_of_labels labels
                  | None -> Types.fresh_row ()
                in
                bind
                  env
                  mangled
                  (Types.mono
                     (Types.IFn
                        ( param_types
                        , annotated_or_fresh m.Ast.md_signature.Ast.ret
                        , row )))))
          methods
      | `Fn (name, params, signature, _) ->
        let type_params = type_params_of s.Ast.span signature.Ast.comptime in
        Hashtbl.replace ctx_fn_params name type_params;
        with_type_params type_params (fun () ->
          let param_types =
            List.map (fun (p : Ast.param) -> annotated_or_fresh p.Ast.ty) params
          in
          let row =
            match signature.Ast.row with
            | Some labels -> row_of_labels labels
            | None -> Types.fresh_row ()
          in
          bind
            env
            name
            (Types.mono
               (Types.IFn (param_types, annotated_or_fresh signature.Ast.ret, row))))
      | _ -> ())
    body

(* This order is load-bearing: hoisting reads signatures, which may name a
   declared type or a trait, and a use may precede its declaration. *)
and infer_block env ctx (body : Ast.desugared_stmt list) : checked_stmt list =
  declare_types body;
  declare_traits body;
  declare_impls body;
  hoist env body;
  let assigned = assigned_names body in
  List.map (fun s -> infer_stmt env ctx assigned s) body

and infer_stmt env ctx assigned (s : Ast.desugared_stmt) : checked_stmt =
  try infer_stmt_impl env ctx assigned s with
  | Types.Type_error message -> raise (Located { span = s.Ast.span; message })

and infer_stmt_impl env ctx assigned (s : Ast.desugared_stmt) : checked_stmt =
  let span = s.Ast.span in
  let node it : checked_stmt = Ast.annotated span Types.IUnit it in
  match s.Ast.it with
  | `Expr e -> node (`Expr (infer_expr env ctx e))
  | `Var_decl (name, annotation, init) ->
    let declared = annotated_or_fresh annotation in
    let init =
      match init with
      | None ->
        Types.unify declared Types.IUnit;
        None
      | Some e ->
        let e' = infer_expr env ctx e in
        unify_at e'.Ast.span declared e'.Ast.ann;
        Some e'
    in
    let generalizable =
      (not (List.mem name assigned))
      && (match s.Ast.it with
          | `Var_decl (_, _, Some e) -> is_syntactic_value e
          | _ -> false)
    in
    bind
      env
      name
      (if generalizable
       then
         Types.generalize
           ~env_vars:(env_free_vars env)
           ~env_rows:(env_free_row_vars env)
           ~env_fields:(env_free_field_vars env)
           declared
       else Types.mono declared);
    node (`Var_decl (name, annotation, init))
  | `Block body ->
    let scope = new_env (Some env) in
    node (`Block (infer_block scope ctx body))
  | `If (cond, then_branch, else_branch) ->
    let cond = infer_expr env ctx cond in
    unify_at cond.Ast.span Types.IBool cond.Ast.ann;
    node
      (`If
        ( cond
        , infer_stmt env ctx assigned then_branch
        , Option.map (infer_stmt env ctx assigned) else_branch ))
  | `While (cond, body) ->
    let cond = infer_expr env ctx cond in
    unify_at cond.Ast.span Types.IBool cond.Ast.ann;
    node (`While (cond, infer_stmt env ctx assigned body))
  | `Fn (name, params, signature, body) ->
    with_type_params
      (Option.value ~default:[] (Hashtbl.find_opt ctx_fn_params name))
      (fun () ->
    let param_types, declared_ret, declared_row =
      match lookup env name with
      | Some { Types.body = Types.IFn (ps, r, row); _ } -> ps, r, row
      | _ ->
        ( List.map (fun (p : Ast.param) -> annotated_or_fresh p.Ast.ty) params
        , annotated_or_fresh signature.Ast.ret
        , Types.fresh_row () )
    in
    let scope = new_env (Some env) in
    List.iter2
      (fun (p : Ast.param) ty -> bind scope p.Ast.name (Types.mono ty))
      params
      param_types;
    let saved_return = ctx.return_type
    and saved_saw = ctx.saw_return
    and saved_row = ctx.row in
    ctx.return_type <- Some declared_ret;
    ctx.saw_return <- false;
    ctx.row <- declared_row;
    let body = infer_block scope ctx body in
    if not ctx.saw_return then Types.unify declared_ret Types.IUnit;
    ctx.return_type <- saved_return;
    ctx.saw_return <- saved_saw;
    ctx.row <- saved_row;
    let fn_type = Types.IFn (param_types, declared_ret, declared_row) in
    (* Leaving [hoist]'s binding in place would make the function's own variables
       count as free in the enclosing scope, so nothing would ever be
       quantified. *)
    Hashtbl.remove env.bindings name;
    bind
      env
      name
      (Types.generalize
         ~env_vars:(env_free_vars env)
         ~env_rows:(env_free_row_vars env)
         ~env_fields:(env_free_field_vars env)
         fn_type);
    Ast.annotated span fn_type (`Fn (name, params, signature, body)))
  | `Type_decl (name, params, body) -> node (`Type_decl (name, params, body))
  | `Op_decl (op, params, signature, body) ->
    let scope = new_env (Some env) in
    let param_types =
      List.map (fun (p : Ast.param) -> annotated_or_fresh p.Ast.ty) params
    in
    List.iter2
      (fun (p : Ast.param) ty -> bind scope p.Ast.name (Types.mono ty))
      params
      param_types;
    let declared_ret = annotated_or_fresh signature.Ast.ret in
    let declared_row = Types.fresh_row () in
    let saved_return = ctx.return_type
    and saved_saw = ctx.saw_return
    and saved_row = ctx.row in
    ctx.return_type <- Some declared_ret;
    ctx.saw_return <- false;
    ctx.row <- declared_row;
    let body = infer_block scope ctx body in
    if not ctx.saw_return then Types.unify declared_ret Types.IUnit;
    ctx.return_type <- saved_return;
    ctx.saw_return <- saved_saw;
    ctx.row <- saved_row;
    Ast.annotated
      span
      (Types.IFn (param_types, declared_ret, declared_row))
      (`Op_decl (op, params, signature, body))
  | `Trait_decl (name, methods) -> node (`Trait_decl (name, methods))
  | `Impl_decl (trait, type_name, params, methods) ->
    let methods =
      List.map
        (fun (m : (Ast.desugared_stmt, unit) Ast.method_def) ->
          let mangled = Ast.method_name type_name m.Ast.md_name in
          with_type_params
            (Option.value ~default:[] (Hashtbl.find_opt ctx_fn_params mangled))
            (fun () ->
          let hoisted =
            match lookup env mangled with
            | Some { Types.body = Types.IFn (params, ret, row); _ }
              when List.length params = List.length m.Ast.md_params ->
              Some (params, ret, row)
            | _ -> None
          in
          let param_types, declared_ret, declared_row =
            match hoisted with
            | Some triple -> triple
            | None ->
              ( List.map
                  (fun (p : Ast.param) -> annotated_or_fresh p.Ast.ty)
                  m.Ast.md_params
              , annotated_or_fresh m.Ast.md_signature.Ast.ret
              , Types.fresh_row () )
          in
          let scope = new_env (Some env) in
          List.iter2
            (fun (p : Ast.param) ty -> bind scope p.Ast.name (Types.mono ty))
            m.Ast.md_params
            param_types;
          let saved_return = ctx.return_type
          and saved_saw = ctx.saw_return
          and saved_row = ctx.row in
          ctx.return_type <- Some declared_ret;
          ctx.saw_return <- false;
          ctx.row <- declared_row;
          let body = infer_block scope ctx m.Ast.md_body in
          if not ctx.saw_return then Types.unify declared_ret Types.IUnit;
          ctx.return_type <- saved_return;
          ctx.saw_return <- saved_saw;
          ctx.row <- saved_row;
          let fn_type = Types.IFn (param_types, declared_ret, declared_row) in
          Hashtbl.remove env.bindings mangled;
          bind
            env
            mangled
            (Types.generalize
               ~env_vars:(env_free_vars env)
               ~env_rows:(env_free_row_vars env)
               ~env_fields:(env_free_field_vars env)
               fn_type);
          { m with Ast.md_body = body; md_ann = fn_type }))
        methods
    in
    node (`Impl_decl (trait, type_name, params, methods))
  | `Match (scrutinee, cases) ->
    let scrutinee = infer_expr env ctx scrutinee in
    let sum, sum_args =
      match Types.repr scrutinee.Ast.ann with
      | Types.ISum (name, args) -> name, args
      | other ->
        fail
          scrutinee.Ast.span
          "Only a sum type can be matched, and this is %s."
          (Types.string_of_infer_ty other)
    in
    let variants =
      match Hashtbl.find_opt ctx_types sum with
      | Some (Sum (vars, variants)) ->
        let mapping = instance vars sum_args in
        List.map
          (fun (name, payload) ->
            name, Ast.map_payload (Types.substitute mapping) payload)
          variants
      | _ -> fail span "Unknown sum type '%s'." sum
    in
    let covered = ref [] in
    let cases =
      List.map
        (fun ((pattern : Ast.pattern), body) ->
          let scope = new_env (Some env) in
          (match pattern with
           | Ast.Pat_wild -> covered := List.map fst variants
           | Ast.Pat_variant (ty, variant, payload) ->
             if not (String.equal ty sum)
             then fail span "This matches a %s, not a %s." ty sum;
             (match List.assoc_opt variant variants with
              | None -> fail span "Type '%s' has no variant '%s'." sum variant
              | Some declared ->
                covered := variant :: !covered;
                let expected = Ast.payload_fields declared in
                let bindings = Ast.payload_fields payload in
                if List.length expected <> List.length bindings
                then
                  fail
                    span
                    "Variant '%s' carries %d value(s) but %d were bound."
                    variant
                    (List.length expected)
                    (List.length bindings);
                List.iter
                  (fun (l, name) ->
                    match List.assoc_opt l expected with
                    | Some ty -> bind scope name (Types.mono ty)
                    | None -> fail span "Variant '%s' has no field '%s'." variant l)
                  bindings));
          pattern, infer_block scope ctx body)
        cases
    in
    List.iter
      (fun (name, _) ->
        if not (List.mem name !covered)
        then fail span "This match does not cover '%s'." name)
      variants;
    node (`Match (scrutinee, cases))
  | `Effect_decl (name, ops) ->
    Hashtbl.replace ctx_effects.declared name ops;
    List.iter
      (fun (o : Ast.op_decl) ->
        Hashtbl.replace ctx_effects.ops o.Ast.op_name o;
        let params =
          List.map (fun (p : Ast.param) -> annotated_or_fresh p.Ast.ty) o.Ast.op_params
        in
        let ret = annotated_or_fresh o.Ast.op_ret in
        let op_type = Types.IFn (params, ret, Types.RCons (name, Types.fresh_row ())) in
        bind
          env
          o.Ast.op_name
          { Types.quantified = []
          ; quantified_rows = Types.free_row_vars op_type
          ; quantified_fields = []
          ; body = op_type
          })
      ops;
    node (`Effect_decl (name, ops))
  | `Run (body, handlers) ->
    List.iter
      (fun (h : Ast.desugared_stmt Ast.handler) ->
        match Hashtbl.find_opt ctx_effects.declared h.Ast.handled with
        | None -> fail span "Unknown effect '%s'." h.Ast.handled
        | Some ops ->
          (* Label-level discharge is only sound if the handler covers the whole
             effect. *)
          List.iter
            (fun (o : Ast.op_decl) ->
              if not (List.exists (fun (a : Ast.desugared_stmt Ast.arm) ->
                        String.equal a.Ast.arm_name o.Ast.op_name)
                        h.Ast.arms)
              then
                fail
                  span
                  "Handler for '%s' is missing operation '%s'."
                  h.Ast.handled
                  o.Ast.op_name)
            ops;
          List.iter
            (fun (a : Ast.desugared_stmt Ast.arm) ->
              if not (List.exists (fun (o : Ast.op_decl) ->
                        String.equal o.Ast.op_name a.Ast.arm_name)
                        ops)
              then
                fail
                  span
                  "Effect '%s' has no operation '%s'."
                  h.Ast.handled
                  a.Ast.arm_name)
            h.Ast.arms)
      handlers;
    let body_row = Types.fresh_row () in
    let saved_row = ctx.row in
    ctx.row <- body_row;
    let scope = new_env (Some env) in
    let body = infer_block scope ctx body in
    ctx.row <- saved_row;
    let remaining =
      List.fold_left
        (fun row (h : Ast.desugared_stmt Ast.handler) ->
          try Types.rewrite_row h.Ast.handled row with
          | Types.Type_error _ -> row)
        body_row
        handlers
    in
    Types.unify_row remaining ctx.row;
    node (`Run (body, List.map (infer_handler env ctx assigned) handlers))
  | `Resume value ->
    let expected =
      match ctx.resume_type with
      | Some t -> t
      | None -> fail span "'resume' outside of a 'ctl' handler."
    in
    let value =
      match value with
      | None ->
        unify_at span expected Types.IUnit;
        None
      | Some e ->
        let e' = infer_expr env ctx e in
        unify_at e'.Ast.span expected e'.Ast.ann;
        Some e'
    in
    node (`Resume value)
  | `Return e ->
    let expected =
      match ctx.return_type with
      | Some t -> t
      | None -> fail span "'return' outside of a function."
    in
    ctx.saw_return <- true;
    let e =
      match e with
      | None ->
        Types.unify expected Types.IUnit;
        None
      | Some e ->
        let e' = infer_expr env ctx e in
        unify_at e'.Ast.span expected e'.Ast.ann;
        Some e'
    in
    node (`Return e)

(* Arms run outside the handler they belong to, so an arm that performs its own
   operation propagates outward instead of catching itself. *)
and infer_handler env ctx assigned (h : Ast.desugared_stmt Ast.handler)
  : checked_stmt Ast.handler
  =
  let arm (a : Ast.desugared_stmt Ast.arm) : checked_stmt Ast.arm =
    let op = Hashtbl.find ctx_effects.ops a.Ast.arm_name in
    let param_types =
      List.map (fun (p : Ast.param) -> annotated_or_fresh p.Ast.ty) op.Ast.op_params
    in
    if List.length param_types <> List.length a.Ast.arm_params
    then
      Types.error
        "Operation '%s' takes %d argument(s) but the handler binds %d."
        a.Ast.arm_name
        (List.length param_types)
        (List.length a.Ast.arm_params);
    let scope = new_env (Some env) in
    List.iter2 (fun name ty -> bind scope name (Types.mono ty)) a.Ast.arm_params param_types;
    let saved_resume = ctx.resume_type
    and saved_return = ctx.return_type
    and saved_saw = ctx.saw_return in
    ctx.resume_type <- (match a.Ast.arm_kind with
                        | Ast.Op_ctl -> Some (annotated_or_fresh op.Ast.op_ret)
                        | Ast.Op_fn -> None);
    ctx.return_type <- Some (annotated_or_fresh op.Ast.op_ret);
    ctx.saw_return <- false;
    let body = List.map (infer_stmt scope ctx assigned) a.Ast.arm_body in
    ctx.resume_type <- saved_resume;
    ctx.return_type <- saved_return;
    ctx.saw_return <- saved_saw;
    { Ast.arm_name = a.Ast.arm_name
    ; arm_kind = a.Ast.arm_kind
    ; arm_params = a.Ast.arm_params
    ; arm_body = body
    }
  in
  { Ast.handled = h.Ast.handled; arms = List.map arm h.Ast.arms }

let rec resolve_expr (e : checked_expr) : Ast.typed_expr =
  let it : Ast.typed_expr_kind =
    match e.Ast.it with
    | #Ast.lit as l -> l
    | #Ast.vars as v -> (Ast.map_vars resolve_expr v :> Ast.typed_expr_kind)
    | #Ast.ops as o -> (Ast.map_ops resolve_expr o :> Ast.typed_expr_kind)
    | #Ast.logic as l -> (Ast.map_logic resolve_expr l :> Ast.typed_expr_kind)
    | #Ast.compound as c -> (Ast.map_compound resolve_expr c :> Ast.typed_expr_kind)
    | #Ast.indexing as i -> (Ast.map_indexing resolve_expr i :> Ast.typed_expr_kind)
    | #Ast.tuple as t -> (Ast.map_tuple resolve_expr t :> Ast.typed_expr_kind)
    | #Ast.record as r -> (Ast.map_record resolve_expr r :> Ast.typed_expr_kind)
    | #Ast.nominal as n -> (Ast.map_nominal resolve_expr n :> Ast.typed_expr_kind)
    | #Ast.collection as c ->
      (Ast.map_collection resolve_expr c :> Ast.typed_expr_kind)
    | #Ast.method_call as m ->
      (Ast.map_method_call resolve_expr m :> Ast.typed_expr_kind)
    | #Ast.reflect as r -> (Ast.map_reflect resolve_expr r :> Ast.typed_expr_kind)
  in
  { Ast.it; span = e.Ast.span; ann = Types.resolve e.Ast.ann }

let rec resolve_stmt (s : checked_stmt) : Ast.typed_stmt =
  let it : Ast.typed_stmt_kind =
    match s.Ast.it with
    | #Ast.stmts as st -> (Ast.map_stmts resolve_expr resolve_stmt st :> Ast.typed_stmt_kind)
    | #Ast.effects as e ->
      (Ast.map_effects resolve_expr resolve_stmt (Ast.map_handler resolve_stmt) e
       :> Ast.typed_stmt_kind)
    | #Ast.type_defs as t -> t
    | #Ast.matching as m ->
      (Ast.map_matching resolve_expr resolve_stmt m :> Ast.typed_stmt_kind)
    | #Ast.op_defs as o -> (Ast.map_op_defs resolve_stmt o :> Ast.typed_stmt_kind)
    | #Ast.method_defs as m ->
      (Ast.map_method_defs resolve_stmt Types.resolve m :> Ast.typed_stmt_kind)
  in
  { Ast.it; span = s.Ast.span; ann = Types.resolve s.Ast.ann }

(* A closed row here would close the caller's, since a call unifies the two.
   Quantifying gives every call site its own, as generalization does for a pure
   function written in Cronyx. *)
let pure params ret =
  let row = Types.fresh_row () in
  let scheme_of body =
    { Types.quantified = List.map fst (Types.free_vars body)
    ; quantified_rows = Types.free_row_vars body
    ; quantified_fields = []
    ; body
    }
  in
  scheme_of (Types.IFn (params, ret, row))

let globals () =
  let env = new_env None in
  bind env "str" (pure [ Types.fresh () ] Types.IStr);
  bind env "clock" (pure [] Types.IFloat);
  let beta = Types.fresh () in
  bind
    env
    "print"
    { Types.quantified = List.map fst (Types.free_vars beta)
    ; quantified_rows = []
    ; quantified_fields = []
    ; body = beta
    };
  env

let admits registry kind (t : Types.infer_ty) =
  match kind with
  | Types.Collection elem ->
    (match Types.container_element t with
     | Some (name, held) when Registry.container registry name <> None ->
       Types.unify elem held;
       true
     | _ -> false)
  | Types.Any -> true
  | Types.Addable | Types.Numeric ->
    (match Types.concrete t with
     | None -> false
     | Some ty ->
       let has op = Registry.find registry op ty ty <> None in
       (match kind with
        | Types.Addable -> has Ast.Add
        | _ -> List.exists has [ Ast.Sub; Ast.Mul; Ast.Div; Ast.Less; Ast.Greater ]))

let list_of = "List__of"

let declare_builtin_types () =
  List.iter
    (fun name -> Hashtbl.replace ctx_types name (Opaque [ Types.fresh () ]))
    [ "Array"; "List" ]

let declare_builtin_methods env =
  let method_ owner name params ret =
    Hashtbl.replace ctx_methods (owner, name) ();
    bind env (Ast.method_name owner name) (pure params ret)
  in
  let elem = Types.fresh () in
  method_ "Array" "len" [ Types.iopaque "Array" [ elem ] ] Types.IInt;
  method_ "string" "len" [ Types.IStr ] Types.IInt;
  method_ "string" "trim" [ Types.IStr ] Types.IStr;
  method_ "string" "contains" [ Types.IStr; Types.IStr ] Types.IBool;
  method_ "string" "split" [ Types.IStr; Types.IStr ] (Types.iopaque "Array" [ Types.IStr ]);
  method_ "string" "chars" [ Types.IStr ] (Types.iopaque "Array" [ Types.IStr ]);
  let item = Types.fresh () in
  method_ "List" "len" [ Types.iopaque "List" [ item ] ] Types.IInt;
  method_ "List" "push" [ Types.iopaque "List" [ item ]; item ] Types.IUnit;
  method_ "List" "pop" [ Types.iopaque "List" [ item ] ] item;
  method_ "List" "contains" [ Types.iopaque "List" [ item ]; item ] Types.IBool;
  method_ "Array" "contains" [ Types.iopaque "Array" [ item ]; item ] Types.IBool;
  bind env list_of (pure [ Types.iopaque "Array" [ item ] ] (Types.iopaque "List" [ item ]))

let check ~registry (program : Ast.desugared_stmt list)
  : (Ast.typed_stmt list, error list) result
  =
  Types.reset ();
  Types.extra_admits := admits registry;
  reset_effects ();
  let env = globals () in
  declare_builtin_types ();
  declare_builtin_methods env;
  let ctx =
    { registry
    ; return_type = None
    ; saw_return = false
    ; row = Types.fresh_row ()
    ; resume_type = None
    }
  in
  let errors = ref [] in
  (try
     declare_types program;
     declare_traits program;
     declare_impls program;
     declare_ops registry program;
     hoist env program
   with
   | Located e -> errors := [ e ]);
  let assigned = assigned_names program in
  let checked =
    List.filter_map
      (fun s ->
        try Some (infer_stmt env ctx assigned s) with
        | Located e ->
          errors := e :: !errors;
          None)
      program
  in
  let errors =
    match Types.resolve_row ctx.row with
    | [] -> !errors
    | labels ->
      { span = { Ast.line = 1; col = 1 }
      ; message =
          Printf.sprintf
            "Unhandled effect(s): %s."
            (String.concat ", " labels)
      }
      :: !errors
  in
  match List.rev errors with
  | [] -> Ok (List.map resolve_stmt checked)
  | errors -> Error errors
