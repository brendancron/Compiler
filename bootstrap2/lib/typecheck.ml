(* A node's type is not finished when it is first visited — in
   `fn f(x) { return x + 1; }`, `x` is only pinned at the `+` — so the tree is
   built with mutable types and resolved once at the end. *)

type error =
  { span : Ast.span
  ; message : string
  }

exception Located of error

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
  | checked_expr Ast.arrays
  | checked_expr Ast.strings
  | checked_expr Ast.method_call
  | checked_expr Ast.reflect
  | (checked_expr, checked_stmt) Ast.lambdas
  ]

and checked_stmt = (checked_stmt_kind, Types.infer_ty) Ast.node

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
  ; mutable in_final_arm : bool
  }

type effect_info =
  { ops : (string, Ast.op_decl) Hashtbl.t
  ; declared : (string, Ast.op_decl list) Hashtbl.t
  }

let ctx_effects = { ops = Hashtbl.create 16; declared = Hashtbl.create 8 }

(* Operations share one namespace, so which effect claimed a name has to be
   remembered: without this a second declaration silently overwrites the first
   and every call site is checked against the wrong signature. *)
let ctx_op_owner : (string, string) Hashtbl.t = Hashtbl.create 16

(* An effect's type parameters, so a handler's arms resolve the same names its
   operations were declared with. *)
let ctx_effect_params : (string, (string * Types.infer_ty) list) Hashtbl.t =
  Hashtbl.create 8

type decl =
  | Opaque of Types.infer_ty list
  | Product of Types.infer_ty list * Types.infer_fields
  | Sum of Types.infer_ty list * (string * variant_decl) list

(* A constructor's signature: what it takes, what it builds, and the variables
   it binds itself. An ordinary variant builds the type applied to the type's
   own parameters and binds nothing; a GADT variant is what makes both of those
   worth carrying. *)
and variant_decl =
  { vd_params : Types.infer_ty list
  ; vd_payload : Types.infer_ty Ast.payload
  ; vd_result : Types.infer_ty list
  (* Written with parameters or a head of its own, so matching it says
     something about the scrutinee that has to be scoped to the arm. A sum
     whose variants all answer [false] is checked exactly as before. *)
  ; vd_refines : bool
  }

let ctx_types : (string, decl) Hashtbl.t = Hashtbl.create 16

let ctx_type_params : (string, Types.infer_ty) Hashtbl.t = Hashtbl.create 8

let decl_params = function
  | Opaque vars | Product (vars, _) | Sum (vars, _) -> vars

let params_of_decl name =
  match Hashtbl.find_opt ctx_types name with
  | Some decl -> decl_params decl
  | None -> []

let instance vars args = List.map2 (fun v a -> Types.var_id v, a) vars args

(* One fresh copy of everything a constructor binds — the type's parameters and
   its own together, since a variant's payload and head may mention both. *)
let instantiation vars declared =
  let bound = vars @ declared.vd_params in
  instance bound (List.map (fun _ -> Types.fresh ()) bound)

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

(* Methods reached through the type rather than through a value of it. *)
let ctx_associated : (string * string, unit) Hashtbl.t = Hashtbl.create 8

(* A trait's parameters, its associated type names, and its signatures. *)
let ctx_traits : (string, string list * Ast.trait_body) Hashtbl.t = Hashtbl.create 8

(* Where each trait was declared, so a program's own declaration can be told
   from the prelude's and allowed to replace it. *)
let ctx_trait_spans : (string, Ast.span) Hashtbl.t = Hashtbl.create 8

(* What an impl bound each associated name to, keyed by the implementing type.
   The trait is not part of the key: two traits naming the same associated type
   for one type would collide, which no program yet does. *)
let ctx_assoc : (string * string, Types.infer_ty) Hashtbl.t = Hashtbl.create 8

(* What a type implemented a trait at. Added rather than replaced, so one type
   may implement a trait at more than one argument. *)
let ctx_impls : (string * string, Types.infer_ty list) Hashtbl.t = Hashtbl.create 8

(* The scheme each variadic is bound to, so a call site can ask whether the name
   still refers to it rather than testing the spelling — a program that declares
   its own `print` gets its own. *)
let ctx_variadic : (string, Types.scheme * Types.infer_ty) Hashtbl.t = Hashtbl.create 4
let ctx_methods : (string * string, unit) Hashtbl.t = Hashtbl.create 32

(* A declaration written inside a block belongs to that block. The tables are
   program-global, so what a block adds is taken back out on the way past it —
   otherwise a type written in a function is visible after it, and two
   functions cannot each declare one of the same name.

   [add] rather than [replace] on the way back: [ctx_impls] keeps every entry
   for a key, and the snapshot is oldest-first so the order survives. *)
let scoped_declarations f =
  let snapshot table = Hashtbl.fold (fun key v acc -> (key, v) :: acc) table [] in
  let restore table saved =
    Hashtbl.reset table;
    List.iter (fun (key, v) -> Hashtbl.add table key v) saved
  in
  let types = snapshot ctx_types
  and traits = snapshot ctx_traits
  and trait_spans = snapshot ctx_trait_spans
  and methods = snapshot ctx_methods
  and impls = snapshot ctx_impls
  and associated = snapshot ctx_associated
  and assoc = snapshot ctx_assoc
  and ops = snapshot ctx_effects.ops
  and declared = snapshot ctx_effects.declared
  and owners = snapshot ctx_op_owner
  and effect_params = snapshot ctx_effect_params in
  Fun.protect
    ~finally:(fun () ->
      restore ctx_types types;
      restore ctx_traits traits;
      restore ctx_trait_spans trait_spans;
      restore ctx_methods methods;
      restore ctx_impls impls;
      restore ctx_associated associated;
      restore ctx_assoc assoc;
      restore ctx_effects.ops ops;
      restore ctx_effects.declared declared;
      restore ctx_op_owner owners;
      restore ctx_effect_params effect_params)
    f

let reset_effects () =
  Hashtbl.reset ctx_effects.ops;
  Hashtbl.reset ctx_effects.declared;
  Hashtbl.reset ctx_op_owner;
  Hashtbl.reset ctx_types;
  Hashtbl.reset ctx_effect_params;
  Hashtbl.reset ctx_traits;
  Hashtbl.reset ctx_trait_spans;
  Hashtbl.reset ctx_associated;
  Hashtbl.reset ctx_assoc;
  Hashtbl.reset ctx_impls;
  Hashtbl.reset ctx_variadic;
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
let env_free ~free ~quantified env =
  let acc = ref [] in
  let rec walk env =
    Hashtbl.iter
      (fun _ (scheme : Types.scheme) ->
        free scheme.Types.body
        |> List.iter (fun id ->
          if (not (List.mem id (quantified scheme))) && not (List.mem id !acc)
          then acc := id :: !acc))
      env.bindings;
    Option.iter walk env.parent
  in
  walk env;
  !acc

let env_free_vars env =
  env_free
    ~free:(fun body -> List.map fst (Types.free_vars body))
    ~quantified:(fun (s : Types.scheme) -> s.Types.quantified)
    env

let env_free_row_vars env =
  env_free
    ~free:Types.free_row_vars
    ~quantified:(fun (s : Types.scheme) -> s.Types.quantified_rows)
    env

let env_free_field_vars env =
  env_free
    ~free:Types.free_field_vars
    ~quantified:(fun (s : Types.scheme) -> s.Types.quantified_fields)
    env

let fail span fmt =
  Printf.ksprintf (fun message -> raise (Located { span; message })) fmt

type receiver =
  | Owner of string
  | Via_trait of string

(* Which operator a trait gives meaning to, and the method that carries it.
   An impl of one registers the entry an `op` declaration would have. *)
let operator_traits =
  [ "Add", (Ast.Add, "add")
  ; "Sub", (Ast.Sub, "sub")
  ; "Mul", (Ast.Mul, "mul")
  ; "Div", (Ast.Div, "div")
  ; "Rem", (Ast.Mod, "rem")
  ]

(* A trait and everything it requires, so a bound reaches an inherited method.
   [seen] guards a cycle, which nothing rejects yet. *)
let rec trait_closure ?(seen = []) (trait : string) : string list =
  if List.mem trait seen
  then seen
  else (
    let seen = trait :: seen in
    match Hashtbl.find_opt ctx_traits trait with
    | None -> seen
    | Some (_, body) ->
      List.fold_left
        (fun seen (super, _) -> trait_closure ~seen super)
        seen
        body.Ast.tb_super)

let declares trait name =
  match Hashtbl.find_opt ctx_traits trait with
  | None -> false
  | Some (_, body) ->
    List.exists (fun (m : Ast.method_sig) -> String.equal m.Ast.ms_name name) body.Ast.tb_methods

let listed names =
  match List.rev names with
  | [] -> ""
  | [ only ] -> only
  | last :: earlier -> String.concat ", " (List.rev earlier) ^ " and " ^ last

(* Where a method call's meaning comes from when the receiver's type is not
   written down: a bound supplies the signature, a literal narrows to the
   container that has the method, and anything else is undecidable. Guessing
   the single owner of a name is unsound — three types declare `len`, and
   picking one by accident annotates the call with the wrong type. *)
let receiver_of span registry (receiver : (_, Types.infer_ty) Ast.node) name elem_owners =
  match Types.infer_type_name receiver.Ast.ann with
  | Some owner -> Owner owner
  | None ->
    (match Types.repr receiver.Ast.ann with
     | Types.IVar { contents = Types.Unbound (_, Types.Bound traits) } ->
       (* The bound may name several traits and each may require others, so the
          one that answers is the one declaring the method rather than the one
          written first. *)
       let reachable = List.concat_map (fun (trait, _) -> trait_closure trait) traits in
       (match List.find_opt (fun trait -> declares trait name) reachable, traits with
        | Some trait, _ -> Via_trait trait
        | None, (trait, _) :: _ -> Via_trait trait
        | None, [] ->
          fail span "Cannot call '%s': the receiver's type is not known here." name)
     | Types.IVar { contents = Types.Unbound (_, Types.Collection elem) } ->
       let holds owner =
         String.equal owner Types.array_name || Registry.container registry owner <> None
       in
       let candidates = List.filter holds elem_owners in
       let narrow_to owner =
         (match
            (if String.equal owner Types.array_name
             then Some (Types.iarray elem)
             else (
               match Registry.container registry owner with
               | Some c ->
                 (match Types.repr (Types.instantiate c.Registry.scheme) with
                  | Types.IFn ([ element ], result, _) ->
                    Types.unify element elem;
                    Some result
                  | _ -> None)
               | None -> None))
          with
          | Some ty ->
            (try Types.unify receiver.Ast.ann ty with
             | Types.Type_error message -> raise (Located { span; message }))
          | None -> fail span "'%s' cannot be built from a literal." owner);
         Owner owner
       in
       (match candidates with
        | [] -> fail span "No container has a method '%s'." name
        (* The array is what a literal is until something asks for more. *)
        | _ when List.mem Types.array_name candidates -> narrow_to Types.array_name
        | [ only ] -> narrow_to only
        | several ->
          fail span
            "'%s' does not say which container this is: %s all declare it."
            name
            (listed several))
     | Types.IVar _ ->
       (match elem_owners with
        | [] -> fail span "No type has a method '%s'." name
        | owners ->
          fail span
            "Cannot call '%s': the receiver's type is not known here. %s declare one, so this needs a bound or an annotation."
            name
            (listed owners))
     | other ->
       fail span
         "Cannot call '%s' on %s, which no impl can name."
         name
         (Types.string_of_infer_ty other))

(* A callee whose scheme quantifies no rows is either concrete — in which case
   containment is what is wanted — or still being inferred, a forward or
   recursive reference sharing a row variable with its own definition. Only the
   second needs the rows tied together, and only while its row is unknown. *)
let admits_row (callee : Types.scheme option) row (caller : Types.infer_row) =
  let pending =
    match Types.repr_row row, callee with
    | Types.RVar _, Some { Types.quantified_rows = []; _ } -> true
    | Types.RVar _, None -> true
    | _ -> false
  in
  if pending then Types.unify_row row caller else Types.row_within row caller

let unify_at span expected actual =
  try Types.unify expected actual with
  | Types.Type_error message -> raise (Located { span; message })

(* The whole context is put back, not only what [set] touched, and it is put
   back however the body leaves: [check] catches an error per top-level
   statement and carries on, so a field still pointing at the failed function
   would follow it into everything after. *)
let in_ctx ctx ~set body =
  let saved =
    ctx.return_type, ctx.saw_return, ctx.row, ctx.resume_type, ctx.in_final_arm
  in
  let restore () =
    let return_type, saw_return, row, resume_type, in_final_arm = saved in
    ctx.return_type <- return_type;
    ctx.saw_return <- saw_return;
    ctx.row <- row;
    ctx.resume_type <- resume_type;
    ctx.in_final_arm <- in_final_arm
  in
  Fun.protect ~finally:restore (fun () ->
    set ();
    body ())

(* A body with no `return` returns unit, which is only knowable once it has been
   walked — so the check belongs here rather than at each caller. *)
let in_function_body ctx ~ret ~row body =
  in_ctx
    ctx
    ~set:(fun () ->
      ctx.return_type <- Some ret;
      ctx.saw_return <- false;
      ctx.row <- row)
    (fun () ->
      let checked = body () in
      if not ctx.saw_return then Types.unify ret Types.IUnit;
      checked)

(* The types the language supplies rather than a declaration. They are not in
   [ctx_types], so anything that resolves a written name has to ask here. *)
let primitive = function
  | "int" -> Some Types.IInt
  | "float" -> Some Types.IFloat
  | "string" -> Some Types.IStr
  | "byte" -> Some Types.IByte
  | "char" -> Some Types.IChr
  | "bool" -> Some Types.IBool
  | "unit" -> Some Types.IUnit
  | _ -> None

let rec infer_ty_of_annotation (t : Ast.type_expr) : Types.infer_ty =
  match t.Ast.it with
  | Ast.Ty_variadic element -> Types.iarray (infer_ty_of_annotation element)
  | Ast.Ty_name name when primitive name <> None -> Option.get (primitive name)
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
  (* A projection off a type that is still a variable cannot be looked up, so it
     becomes one too. The copy [Type_mono] makes has a concrete owner, and the
     lookup succeeds there. *)
  | Ast.Ty_assoc (owner, member) ->
    Types.project (infer_ty_of_annotation owner) member
  | Ast.Ty_fn (params, ret, row) ->
    Types.IFn
      ( List.map infer_ty_of_annotation params
      , infer_ty_of_annotation ret
      , row_of_labels row )

and named_type span name args =
  if String.equal name Types.reflection_name && args = []
  then Types.ireflected
  else if String.equal name Types.code_name && args = []
  then Types.icode
  else if String.equal name Types.name_name && args = []
  then Types.iname
  else if String.equal name Types.array_name
  then (
    match args with
    | [ elem ] -> Types.iarray elem
    | _ ->
      fail
        span
        "Type '%s' takes 1 argument(s) but %d were given."
        name
        (List.length args))
  else
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

(* A written row names effects without instantiating them, so each entry takes
   fresh arguments and a use is what settles them. *)
and row_of_labels labels =
  List.fold_right
    (fun label rest ->
      let arity =
        List.length (Option.value ~default:[] (Hashtbl.find_opt ctx_effect_params label))
      in
      Types.RCons (label, List.init arity (fun _ -> Types.fresh ()), rest))
    labels
    Types.REmpty

(* A bound may name a parameter declared before it — `<S, T: TryFrom<S>>` — so
   each is in scope while the next one's annotation is read. What is registered
   here is undone before returning; the caller installs the whole list. *)
let type_params_of span (comptime : Ast.comptime_param list) =
  let touched = List.map (fun (p : Ast.comptime_param) ->
    p.Ast.cp_name, Hashtbl.find_opt ctx_type_params p.Ast.cp_name) comptime
  in
  let restore () =
    List.iter
      (fun (name, previous) ->
        match previous with
        | Some var -> Hashtbl.replace ctx_type_params name var
        | None -> Hashtbl.remove ctx_type_params name)
      touched
  in
  Fun.protect ~finally:restore (fun () ->
  List.map
    (fun (p : Ast.comptime_param) ->
      let remember var =
        Hashtbl.replace ctx_type_params p.Ast.cp_name var;
        p.Ast.cp_name, var
      in
      match p.Ast.cp_ty with
      | None ->
        let var = Types.fresh () in
        Types.declare_param var;
        remember var
      (* An annotation naming a declared trait is a bound on a type parameter;
         anything else would be a value parameter. *)
      | Some { Ast.it = Ast.Ty_name trait; _ } when Hashtbl.mem ctx_traits trait ->
        let var = Types.fresh_with (Types.Bound [ trait, [] ]) in
        Types.declare_param var;
        remember var
      (* `T: TryFrom<S>` — the arguments are part of which impl the bound
         reaches, so they are read here rather than dropped. *)
      | Some { Ast.it = Ast.Ty_app (trait, args); _ } when Hashtbl.mem ctx_traits trait ->
        let var =
          Types.fresh_with (Types.Bound [ trait, List.map infer_ty_of_annotation args ])
        in
        Types.declare_param var;
        remember var
      | Some _ ->
        fail
          span
          "Comptime value parameter '%s' is not supported yet."
          p.Ast.cp_name)
    comptime)

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
  (* What a lambda assigns it assigns when called, not where it stands. *)
  | `Lambda _ -> acc
  | #Ast.lit | `Var _ -> acc
  | `Assign (name, v) | `Compound (_, name, v) -> assigned_in_expr v (name :: acc)
  | `Unop (_, a) -> assigned_in_expr a acc
  | `Binop (_, a, b) | `And (a, b) | `Or (a, b) ->
    assigned_in_expr b (assigned_in_expr a acc)
  | `Call (callee, args) ->
    List.fold_left (fun acc a -> assigned_in_expr a acc) (assigned_in_expr callee acc) args
  | `Typeof e -> assigned_in_expr e acc
  | `Method_call (receiver, _, _, args) | `Comptime_call (receiver, _, args) ->
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
  | `New_call (_, _, args) ->
    List.fold_left (fun acc a -> assigned_in_expr a acc) acc args
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
  | `Defer inner -> assigned_in_stmt inner acc
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
  | `Impl_decl (_, _, _, impl) ->
    List.fold_left
      (fun acc (m : (Ast.desugared_stmt, unit) Ast.method_def) ->
        List.fold_left (fun acc st -> assigned_in_stmt st acc) acc m.Ast.md_body)
      acc
      impl.Ast.ib_methods
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
  | Some Types.Str -> Types.IChr
  | Some other ->
    (match Types.container_element (Types.of_ty other) with
     | Some (name, elem)
       when String.equal name Types.array_name || Registry.is_indexed registry name ->
       elem
     | _ ->
       (* A type that is not a container holds no element to read off its own
          arguments, so what indexing answers with is what its impl bound. *)
       (match Types.type_name other with
        | Some name when Registry.is_indexed registry name ->
          (match Hashtbl.find_opt ctx_assoc (name, "Output") with
           | Some elem -> elem
           | None -> fail target.Ast.span "Cannot index %s." (Types.string_of_ty other))
        | _ -> fail target.Ast.span "Cannot index %s." (Types.string_of_ty other)))
  | None ->
    let elem = Types.fresh () in
    unify_at target.Ast.span (Types.fresh_with (Types.Collection elem)) target.Ast.ann;
    elem

(* The entry for reading this target at this index, when the index is a type
   the target declared one for. Its declared signature is what says how the
   operands and the result relate. *)
let declared_index env registry (target : checked_expr) (index : checked_expr) =
  let concrete t = Option.map Types.string_of_ty (Types.concrete t) in
  match Types.concrete index.Ast.ann with
  | Some Types.Int | None -> None
  | Some _ ->
    (* A literal whose container nothing has chosen is an array, which is what
       it would have defaulted to anyway — decided here because the entry to
       reach cannot be found without it. *)
    (match Types.repr target.Ast.ann with
     | Types.IVar { contents = Types.Unbound (_, Types.Collection elem) } ->
       (try Types.unify target.Ast.ann (Types.iarray elem) with
        | Types.Type_error _ -> ())
     | _ -> ());
    (match Types.type_name (Option.value (Types.concrete target.Ast.ann) ~default:Types.Unit) with
     | None -> None
     | Some owner ->
       (match Registry.indexed registry owner (Option.value (concrete index.Ast.ann) ~default:"") with
        | Some { Registry.get = Some fn; _ } ->
          (match lookup env fn with
           | None -> None
           | Some scheme ->
             let result = Types.fresh () in
             (try
                Types.unify
                  (Types.instantiate scheme)
                  (Types.IFn ([ target.Ast.ann; index.Ast.ann ], result, Types.REmpty));
                Some result
              with
              | Types.Type_error _ -> None))
        | _ -> None))

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
  | `Name n -> node Types.iname (`Name n)
  | `Bytes b -> node (Types.iarray Types.IByte) (`Bytes b)
  | `Char c -> node Types.IChr (`Char c)
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
    (* The name has to still mean the entry, not merely be spelled like it. *)
    let variadic =
      match callee.Ast.it with
      | `Var name ->
        (match Hashtbl.find_opt ctx_variadic name, lookup env name with
         | Some (declared, result), Some found when declared == found -> Some result
         | _ -> None)
      | _ -> None
    in
    (match variadic with
     | Some result ->
       (* No HM type describes the arguments, so the call carries a signature
          built from the ones it was given; without it every such call would
          reach [Verify] as a generic callee and be waved through. *)
       let callee_node =
         { callee_node with
           Ast.ann =
             Types.IFn
               ( List.map (fun (a : checked_expr) -> a.Ast.ann) args
               , result
               , Types.REmpty )
         }
       in
       node result (`Call (callee_node, args))
     | None ->
       let ret = Types.fresh () in
       let row = Types.fresh_row () in
       Types.unify
         callee_node.Ast.ann
         (Types.IFn (List.map (fun (a : checked_expr) -> a.Ast.ann) args, ret, row));
       let scheme =
         match callee.Ast.it with
         | `Var name -> lookup env name
         | _ -> None
       in
       admits_row scheme row ctx.row;
       node ret (`Call (callee_node, args)))
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
    admits_row (lookup env name) row ctx.row;
    node ret (`Call (callee_node, args))
  | `Method_call (receiver, name, as_function, args) ->
    (* `T.from(x)` — the receiver names a type rather than a value, which is how
       an associated function is reached. *)
    let named_receiver =
      match receiver.Ast.it with
      | `Var owner when lookup env owner = None ->
        (match Hashtbl.find_opt ctx_type_params owner with
         | Some var -> Some var
         (* A primitive is not in [ctx_types] — it is recognized by name in
            [infer_ty_of_annotation] — so it needs asking for separately, or
            `int.from(…)` reaches nothing while `21.double()` works. *)
         | None when primitive owner <> None -> primitive owner
         | None ->
           if Hashtbl.mem ctx_types owner
           then Some (named_type receiver.Ast.span owner [])
           else None)
      | _ -> None
    in
    let receiver =
      match named_receiver with
      | Some ann -> { Ast.it = `Int 0; span = receiver.Ast.span; ann }
      | None -> infer_expr env ctx receiver
    in
    let args = List.map (infer_expr env ctx) args in
    let owners =
      List.sort_uniq
        compare
        (Hashtbl.fold
           (fun (owner, declared) _ acc ->
             if String.equal declared name then owner :: acc else acc)
           ctx_methods
           [])
    in
    (* A dot is a call with the receiver passed first, so a free function
       answers where the type declares no method of that name. An `impl` wins,
       or a method could be shadowed by whatever function is in scope. *)
    let via_function () =
      match lookup env as_function with
      | None -> None
      | Some scheme ->
        let fn = Types.instantiate scheme in
        let passed =
          receiver.Ast.ann :: List.map (fun (a : checked_expr) -> a.Ast.ann) args
        in
        (match Types.repr fn with
         | Types.IFn (params, _, _) when List.length params <> List.length passed ->
           fail
             span
             "'%s' takes %d argument(s) but %d were passed, counting the receiver."
             name
             (List.length params)
             (List.length passed)
         | Types.IFn _ -> ()
         | _ -> fail span "'%s' is not a function." name);
        let ret = Types.fresh () in
        let row = Types.fresh_row () in
        Types.unify fn (Types.IFn (passed, ret, row));
        admits_row (Some scheme) row ctx.row;
        Some
          (node ret (`Call ({ Ast.it = `Var as_function; span; ann = fn }, receiver :: args)))
    in
    (* A receiver whose type is not pinned has no owner to look in, and a free
       function is what pins it. *)
    let found =
      try Ok (receiver_of span ctx.registry receiver name owners) with
      | Located _ as e -> Error e
    in
    (match found with
     | Error e ->
       (match via_function () with
        | Some call -> call
        | None -> raise e)
     | Ok found ->
       (match found with
     (* A bound answers without an owner: the trait declares the signature, and
        which type supplies the body is settled later. *)
     | Via_trait trait ->
       let trait_params, trait_body =
         Option.value
           (Hashtbl.find_opt ctx_traits trait)
           ~default:([], { Ast.tb_super = []; tb_assoc = []; tb_methods = [] })
       in
       let trait_methods = trait_body.Ast.tb_methods in
       (* `Self` is the type the bound stands for, and the trait's own
          parameters are what the bound named it at. *)
       let bound_args =
         match Types.repr receiver.Ast.ann with
         | Types.IVar { contents = Types.Unbound (_, Types.Bound traits) } ->
           Option.value (List.assoc_opt trait traits) ~default:[]
         | _ -> []
       in
       (* An associated name stands for whatever the impl bound it to, which is
          not known from a bound alone, so it reads as a variable here and is
          pinned when the receiver's own impl is reached. *)
       let projected name = Types.project receiver.Ast.ann name in
       let in_scope =
         ("Self", receiver.Ast.ann)
         :: List.map (fun name -> name, projected name) trait_body.Ast.tb_assoc
         @ (if List.length trait_params = List.length bound_args
            then List.combine trait_params bound_args
            else [])
       in
       with_type_params in_scope (fun () ->
       let declared =
         List.find_opt (fun (m : Ast.method_sig) -> String.equal m.Ast.ms_name name) trait_methods
       in
       (match declared with
        | None -> fail span "Trait '%s' has no method '%s'." trait name
        | Some m ->
          (* Without `self` every parameter is its own; with it, the first is
             the receiver and is already accounted for. *)
          let rest =
            match m.Ast.ms_params with
            | { Ast.name = "self"; _ } :: rest -> rest
            | all -> all
          in
          if List.length rest <> List.length args
          then
            fail
              span
              "Method '%s' takes %d argument(s) but %d were passed."
              name
              (List.length rest)
              (List.length args);
          let fn =
            Types.IFn
              ( receiver.Ast.ann
                :: List.map (fun (p : Ast.param) -> annotated_or_fresh p.Ast.ty) rest
              , annotated_or_fresh m.Ast.ms_signature.Ast.ret
              , Types.fresh_row () )
          in
          let ret = Types.fresh () in
          let row = Types.fresh_row () in
          Types.unify
            fn
            (Types.IFn
               ( receiver.Ast.ann :: List.map (fun (a : checked_expr) -> a.Ast.ann) args
               , ret
               , row ));
          Types.unify_row row ctx.row;
          node ret (`Method_call (receiver, name, as_function, args))))
     | Owner owner ->
       if Hashtbl.mem ctx_associated (owner, name) && named_receiver = None
       then
         fail
           span
           "'%s' is an associated function of '%s', so it is reached as '%s.%s'."
           name
           owner
           owner
           name;
       let missing () = fail span "Type '%s' has no method '%s'." owner name in
       if (String.equal owner Types.array_name || String.equal owner Types.string_name)
          && String.equal name Types.array_len
       then (
         if args <> []
         then
           fail
             span
             "Method '%s' takes 0 argument(s) but %d were passed."
             Types.array_len
             (List.length args);
         node
           Types.IInt
           (if String.equal owner Types.string_name
            then `Str_len receiver
            else `Array_len receiver))
       else if not (Hashtbl.mem ctx_methods (owner, name))
       then (
         match via_function () with
         | Some call -> call
         | None -> missing ())
       else (
         let fn =
           match lookup env (Ast.method_name owner name) with
           | Some scheme -> Types.instantiate scheme
           | None -> missing ()
         in
         (* An associated function takes no receiver, so what it was passed is
            the arguments alone. *)
         let associated = Hashtbl.mem ctx_associated (owner, name) in
         let passed =
           let given = List.map (fun (a : checked_expr) -> a.Ast.ann) args in
           if associated then given else receiver.Ast.ann :: given
         in
         (match Types.repr fn with
          | Types.IFn (params, _, _) when List.length params <> List.length passed ->
            fail
              span
              "%s '%s' takes %d argument(s) but %d were passed."
              (if associated then "Associated function" else "Method")
              name
              (if associated then List.length params else List.length params - 1)
              (List.length args)
          | _ -> ());
         let ret = Types.fresh () in
         let row = Types.fresh_row () in
         Types.unify fn (Types.IFn (passed, ret, row));
         admits_row (lookup env (Ast.method_name owner name)) row ctx.row;
         node ret (`Method_call (receiver, name, as_function, args)))))
  (* Checked the way a named function is, minus the binding and the
     generalization: it is a value here, not a declaration. *)
  | `Lambda (params, signature, body) ->
    let param_types = List.map (fun (p : Ast.param) -> annotated_or_fresh p.Ast.ty) params in
    let declared_ret = annotated_or_fresh signature.Ast.ret in
    let row = Types.fresh_row () in
    let scope = new_env (Some env) in
    List.iter2
      (fun (p : Ast.param) ty -> bind scope p.Ast.name (Types.mono ty))
      params
      param_types;
    let body =
      in_function_body ctx ~ret:declared_ret ~row (fun () -> infer_block scope ctx body)
    in
    node (Types.IFn (param_types, declared_ret, row)) (`Lambda (params, signature, body))
  (* A bare name is a value's if one answers to it, and the declared type
     otherwise, so `typeof(Dog)` reaches a type nothing holds. The operand is
     only ever read for its annotation, so what stands under it does not matter
     beyond carrying one. *)
  | `Typeof { Ast.it = `Var name; span = inner; _ } when lookup env name = None ->
    let ty =
      try infer_ty_of_annotation { Ast.it = Ast.Ty_name name; span = inner; ann = () } with
      | Located _ -> fail inner "Nothing named '%s' is a value or a type." name
    in
    node Types.ireflected (`Typeof { Ast.it = `Int 0; span = inner; ann = ty })
  | `Typeof e -> node Types.ireflected (`Typeof (infer_expr env ctx e))
  | `Collection_lit items ->
    let elem = Types.fresh () in
    let container = Types.fresh_with (Types.Collection elem) in
    let items = List.map (infer_expr env ctx) items in
    List.iter
      (fun (i : checked_expr) -> unify_at i.Ast.span elem i.Ast.ann)
      items;
    node container (`Collection_lit items)
  | `New_call (name, type_args, args) when String.equal name Types.array_name ->
    let elem =
      match List.map infer_ty_of_annotation type_args with
      | [ elem ] -> elem
      | [] -> Types.fresh ()
      | given ->
        fail span "Type 'Array' takes 1 argument(s) but %d were given." (List.length given)
    in
    (match List.map (infer_expr env ctx) args with
     | [ length; fill ] ->
       unify_at length.Ast.span Types.IInt length.Ast.ann;
       unify_at fill.Ast.span elem fill.Ast.ann;
       node (Types.iarray elem) (`Array_new (length, fill))
     | given ->
       fail
         span
         "An array takes a length and a fill value, but %d argument(s) were given."
         (List.length given))
  (* `new T<A>(x)` is a call of the function the registry names for T. *)
  | `New_call (name, type_args, args) ->
    (match Registry.constructor ctx.registry name with
     | None -> fail span "'%s' cannot be constructed with arguments." name
     | Some fn ->
       let scheme =
         match lookup env fn with
         | Some scheme -> scheme
         | None -> fail span "Undefined variable '%s'." fn
       in
       let fn_ty = Types.instantiate scheme in
       let args = List.map (infer_expr env ctx) args in
       let ret = Types.fresh () in
       let row = Types.fresh_row () in
       Types.unify
         fn_ty
         (Types.IFn (List.map (fun (a : checked_expr) -> a.Ast.ann) args, ret, row));
       admits_row (lookup env fn) row ctx.row;
       (* Written arguments pin the result; the constructor's own parameters are
          its business. *)
       if type_args <> []
       then
         unify_at
           span
           (named_type span name (List.map infer_ty_of_annotation type_args))
           ret;
       node ret (`Call (Ast.annotated span fn_ty (`Var fn), args)))
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
          (* The variant's own parameters are freshened with the type's: a
             constructor is a signature, and every use gets its own copy. *)
          let mapping = instantiation vars declared in
          let args = List.map (Types.substitute mapping) declared.vd_result in
          let expected =
            List.map
              (fun (l, t) -> l, Types.substitute mapping t)
              (Ast.payload_fields declared.vd_payload)
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
    (* An index that is not an int is one the type declared an entry for, and
       what that entry returns is its own business — a range yields a slice,
       not an element. *)
    (match declared_index env ctx.registry target index with
     | Some result -> node result (`Index (target, index))
     | None ->
       unify_at index.Ast.span Types.IInt index.Ast.ann;
       node (element_of ctx.registry target) (`Index (target, index)))
  | `Index_assign (target, index, v) ->
    let target = infer_expr env ctx target in
    if Types.concrete target.Ast.ann = Some Types.Str
    then fail target.Ast.span "A string cannot be assigned into.";
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
        let named (p : Ast.param) =
          match p.Ast.ty with
          | Some { Ast.it = Ast.Ty_name n; _ } | Some { Ast.it = Ast.Ty_app (n, _); _ } -> n
          | _ -> fail s.Ast.span "An operator's operands must be annotated."
        in
        let emitted = Ast.op_entry_name op params signature in
        let result_name () =
          match signature.Ast.ret with
          | Some { Ast.it = Ast.Ty_name n; _ } | Some { Ast.it = Ast.Ty_app (n, _); _ } -> n
          | _ -> fail s.Ast.span "An operator must declare its result type."
        in
        let taken what name = fail s.Ast.span "%s is already declared for %s." what name in
        (match op, params with
         (* One operand builds the type from a literal, two read an element. *)
         | Ast.Op_index, [ items ] ->
           let owner = result_name () in
           if Registry.container registry owner <> None then taken "A literal" owner;
           with_type_params (type_params_of s.Ast.span signature.Ast.comptime) (fun () ->
             let element =
               match Types.repr (annotated_or_fresh items.Ast.ty) with
               | Types.INamed (name, [ element ], _) when String.equal name Types.array_name ->
                 element
               | _ ->
                 fail
                   s.Ast.span
                   "A literal entry takes one array of the elements it builds from."
             in
             let body =
               Types.IFn ([ element ], annotated_or_fresh signature.Ast.ret, Types.REmpty)
             in
             Registry.register_container
               registry
               owner
               { Registry.entry = emitted
               ; scheme =
                   { Types.quantified = List.map fst (Types.free_vars body)
                   ; quantified_rows = []
                   ; quantified_fields = []
                   ; body
                   }
               })
         (* Two entries differ by what indexes them, so `taken` is about one
            index type rather than about the whole type. *)
         | Ast.Op_index, [ target; index ] ->
           let owner = named target
           and by = named index in
           (match Registry.exact_index registry owner by with
            | Some { Registry.get = Some _; _ } -> taken ("Indexing by " ^ by) owner
            | _ -> ());
           Registry.register_index_get registry owner by emitted
         | Ast.Op_index_set, [ target; index; _ ] ->
           let owner = named target
           and by = named index in
           (match Registry.exact_index registry owner by with
            | Some { Registry.set = Some _; _ } -> taken ("Index assignment by " ^ by) owner
            | _ -> ());
           Registry.register_index_set registry owner by emitted
         | Ast.Op_index, _ ->
           fail
             s.Ast.span
             "'[]' takes one operand to build from a literal, or two to read an element."
         | Ast.Op_index_set, _ -> fail s.Ast.span "'[]=' takes three operands."
         | Ast.Op_binary binary, [ lhs; rhs ] ->
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
           if Registry.find_exact registry binary lhs_ty rhs_ty <> None
           then
             fail
               s.Ast.span
               "Operator %s is already defined for %s and %s."
               (Ast.string_of_binop binary)
               (Types.string_of_ty lhs_ty)
               (Types.string_of_ty rhs_ty);
           Registry.register
             registry
             binary
             lhs_ty
             rhs_ty
             { Registry.result = Some result; emit = Registry.Call emitted }
         | Ast.Op_binary _, _ -> fail s.Ast.span "An operator takes exactly two operands.")
      | _ -> ())
    body

and declare_traits (body : Ast.desugared_stmt list) =
  List.iter
    (fun (s : Ast.desugared_stmt) ->
      match s.Ast.it with
      | `Trait_decl (name, params, trait_body) ->
        (* A program declaring a trait the prelude also declares gets its own,
           the way one declaring its own `print` does. Two declarations in the
           program itself are still a mistake. *)
        let from_prelude (span : Ast.span) = String.equal span.Ast.file Prelude.file in
        (match Hashtbl.find_opt ctx_trait_spans name with
         | Some declared when from_prelude declared && not (from_prelude s.Ast.span) -> ()
         | Some _ -> fail s.Ast.span "Trait '%s' is already declared." name
         | None -> ());
        Hashtbl.replace ctx_trait_spans name s.Ast.span;
        Hashtbl.replace ctx_traits name (params, trait_body)
      | _ -> ())
    body

and declare_impls registry (body : Ast.desugared_stmt list) =
  List.iter
    (fun (s : Ast.desugared_stmt) ->
      match s.Ast.it with
      | `Impl_decl (trait, type_name, params, impl) ->
        let span = s.Ast.span in
        let methods = impl.Ast.ib_methods in
        let type_params = List.map (fun name -> name, Types.fresh ()) params in
        let supplies name =
          List.exists
            (fun (m : (Ast.desugared_stmt, unit) Ast.method_def) ->
              String.equal m.Ast.md_name name)
            methods
        in
        with_type_params type_params (fun () -> ignore (self_ty span type_name params));
        with_type_params type_params (fun () ->
          List.iter
            (fun (name, bound) ->
              Hashtbl.replace ctx_assoc (type_name, name) (infer_ty_of_annotation bound))
            impl.Ast.ib_assoc);
        (match trait with
         | None -> ()
         | Some trait ->
           (match Hashtbl.find_opt ctx_traits (fst trait) with
            | None -> fail span "Unknown trait '%s'." (fst trait)
            | Some (_, required) ->
              List.iter
                (fun name ->
                  if not (List.mem_assoc name impl.Ast.ib_assoc)
                  then
                    fail
                      span
                      "'%s' for '%s' is missing associated type '%s'."
                      (fst trait)
                      type_name
                      name)
                required.Ast.tb_assoc;
              List.iter
                (fun (r : Ast.method_sig) ->
                  if not (supplies r.Ast.ms_name)
                  then
                    fail
                      span
                      "'%s' for '%s' is missing method '%s'."
                      (fst trait)
                      type_name
                      r.Ast.ms_name)
                required.Ast.tb_methods));
        List.iter
          (fun (m : (Ast.desugared_stmt, unit) Ast.method_def) ->
            (* Without `self` it is an associated function, which is what lets a
               conversion name the type it produces. *)
            (match m.Ast.md_params with
             | { Ast.name = "self"; _ } :: _ -> ()
             | _ ->
               Hashtbl.replace ctx_associated (type_name, m.Ast.md_name) ();
               Registry.mark_associated registry type_name m.Ast.md_name);
            if Hashtbl.mem ctx_methods (type_name, m.Ast.md_name)
            then
              fail
                span
                "Type '%s' already has a method '%s'."
                type_name
                m.Ast.md_name;
            Hashtbl.replace ctx_methods (type_name, m.Ast.md_name) ())
          methods;
        Option.iter
          (fun (t, args) ->
            Hashtbl.add ctx_impls (type_name, t) (List.map infer_ty_of_annotation args))
          trait;
        (* An operator trait's impl is an entry in the same table an `op`
           declaration writes to, so the lowering in [Resolve] is unchanged. *)
        Option.iter
          (fun (t, args) ->
            let entry_name method_ = Ast.method_name type_name method_ in
            let named what ty =
              match Types.type_name ty with
              | Some name -> name
              | None -> fail span "An operator impl's %s must be a named type." what
            in
            let self_concrete () =
              match Types.concrete (self_ty span type_name params) with
              | Some ty -> ty
              | None -> fail span "An operator impl's type must be concrete."
            in
            let one_argument () =
              match args with
              | [ only ] -> infer_ty_of_annotation only
              | _ -> fail span "'%s' takes one type argument." t
            in
            (match t with
             | "Eq" ->
               with_type_params type_params (fun () ->
                 let self = self_concrete () in
                 List.iter
                   (fun op ->
                     Registry.register
                       registry
                       op
                       self
                       self
                       { Registry.result = Some Types.Bool
                       ; emit = Registry.Call (entry_name "eq")
                       })
                   [ Ast.Equal; Ast.Not_equal ])
             | "Index" ->
               with_type_params type_params (fun () ->
                 Registry.register_index_get
                   registry
                   (named "type" (self_concrete ()))
                   (named "index" (Types.resolve (one_argument ())))
                   (entry_name "get"))
             | "IndexSet" ->
               with_type_params type_params (fun () ->
                 Registry.register_index_set
                   registry
                   (named "type" (self_concrete ()))
                   (named "index" (Types.resolve (one_argument ())))
                   (entry_name "set"))
             | _ -> ());
            match List.assoc_opt t operator_traits with
            | None -> ()
            | Some (binary, method_) ->
              let concrete what ty =
                match Types.concrete ty with
                | Some ty -> ty
                | None -> fail span "An operator impl's %s must be a concrete type." what
              in
              with_type_params type_params (fun () ->
                let lhs = concrete "type" (self_ty span type_name params) in
                let rhs =
                  match args with
                  | [ rhs ] -> concrete "right operand" (infer_ty_of_annotation rhs)
                  | _ -> fail span "'%s' takes one type argument." t
                in
                let result =
                  match List.assoc_opt "Output" impl.Ast.ib_assoc with
                  | Some bound -> concrete "Output" (infer_ty_of_annotation bound)
                  | None -> fail span "'%s' for '%s' is missing associated type 'Output'." t type_name
                in
                if Registry.find_exact registry binary lhs rhs <> None
                then
                  fail
                    span
                    "Operator %s is already defined for %s and %s."
                    (Ast.string_of_binop binary)
                    (Types.string_of_ty lhs)
                    (Types.string_of_ty rhs);
                Registry.register
                  registry
                  binary
                  lhs
                  rhs
                  { Registry.result = Some result
                  ; emit = Registry.Call (Ast.method_name type_name method_)
                  }))
          trait
      | _ -> ())
    body;
  (* A second pass: an impl may satisfy a supertrait further down the file, so
     nothing can be concluded until every one has been registered. *)
  List.iter
    (fun (s : Ast.desugared_stmt) ->
      match s.Ast.it with
      | `Impl_decl (Some (trait, _), type_name, _, _) ->
        (match Hashtbl.find_opt ctx_traits trait with
         | None -> ()
         | Some (_, body) ->
           List.iter
             (fun (super, _) ->
               if not (Hashtbl.mem ctx_impls (type_name, super))
               then
                 fail
                   s.Ast.span
                   "'%s' for '%s' is missing the supertrait '%s'."
                   trait
                   type_name
                   super)
             body.Ast.tb_super)
      | _ -> ())
    body

and self_ty span type_name params =
  if params = []
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
        (* The placeholder a recursive mention resolves against while the rest
           of the declaration is read. It has to be the right shape already, or
           `Add(Expr<int>, …)` would read `Expr` as a product. *)
        Hashtbl.replace
          ctx_types
          name
          (match body with
           | Ast.T_variants _ -> Sum (vars, [])
           | Ast.T_fields _ -> Product (vars, Types.FEmpty));
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
              Sum (vars, List.map (variant_of s.Ast.span name vars) variants))
        in
        Hashtbl.replace ctx_types name declared
      | _ -> ())
    body

(* A variant's own parameters are in scope for its payload and its head, on top
   of the type's. A head that is written is the type at whatever arguments the
   constructor pins; one that is not is the type at its own parameters, which is
   every variant that is not a GADT's. *)
and variant_of span owner vars (v : Ast.variant) =
  let own = List.map (fun name -> name, Types.fresh ()) v.Ast.v_params in
  with_type_params own (fun () ->
    let payload = Ast.map_payload infer_ty_of_annotation v.Ast.v_payload in
    let result =
      match v.Ast.v_result with
      | None -> vars
      | Some head ->
        (match Types.repr (infer_ty_of_annotation head) with
         | Types.ISum (built, args) when String.equal built owner -> args
         | other ->
           fail
             span
             "Variant '%s' builds %s, but it is declared in '%s'."
             v.Ast.v_name
             (Types.string_of_infer_ty other)
             owner)
    in
    ( v.Ast.v_name
    , { vd_params = List.map snd own
      ; vd_payload = payload
      ; vd_result = result
      ; vd_refines = v.Ast.v_params <> [] || v.Ast.v_result <> None
      } ))

(* What a recursive occurrence sees. Quantifying the parameters the author wrote
   is what lets a function call itself at a different instantiation than the one
   being checked — `depth<T>` reaching `depth` at `Nested<(T, T)>`. Inference
   cannot find that on its own, which is why it takes a written `<T>` and why
   only those variables are quantified: everything else about the signature is
   still being inferred and has to stay shared with the body.

   The row is left monomorphic on purpose, so recursion still contributes to the
   effects the function is inferred to perform. *)
and declared_scheme type_params body =
  match
    List.filter_map
      (fun (_, var) ->
        match Types.repr var with
        | Types.IVar { contents = Types.Unbound (id, _) } -> Some id
        | _ -> None)
      type_params
  with
  | [] -> Types.mono body
  | quantified ->
    { Types.quantified; quantified_rows = []; quantified_fields = []; body }

and hoist env (body : Ast.desugared_stmt list) =
  List.iter
    (fun (s : Ast.desugared_stmt) ->
      match s.Ast.it with
      | `Op_decl (op, params, signature, _) ->
        let name = Ast.op_entry_name op params signature in
        let type_params = type_params_of s.Ast.span signature.Ast.comptime in
        Hashtbl.replace ctx_fn_params name type_params;
        with_type_params type_params (fun () ->
          let param_types =
            List.map (fun (p : Ast.param) -> annotated_or_fresh p.Ast.ty) params
          in
          bind
            env
            name
            (Types.mono
               (Types.IFn
                  ( param_types
                  , annotated_or_fresh signature.Ast.ret
                  , Types.fresh_row () ))))
      | `Impl_decl (_, type_name, params, impl) ->
        (* Declared, like a written `<T>`, so a kind constraint from the body
           must not default it. *)
        let impl_params =
          List.map
            (fun name ->
              let var = Types.fresh () in
              Types.declare_param var;
              name, var)
            params
        in
        List.iter
          (fun (m : (Ast.desugared_stmt, unit) Ast.method_def) ->
            match m.Ast.md_params with
            (* [declare_impls] reported it; binding it anyway would report the
               receiver as a mismatch on a parameter the author did write. *)
            | [] -> ()
            | { Ast.name = "self"; _ } :: rest ->
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
                        , row ))))
            | _ -> ())
          impl.Ast.ib_methods
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
            (declared_scheme
               type_params
               (Types.IFn (param_types, annotated_or_fresh signature.Ast.ret, row))))
      | _ -> ())
    body

(* This order is load-bearing: hoisting reads signatures, which may name a
   declared type or a trait, and a use may precede its declaration. *)
and infer_block env ctx (body : Ast.desugared_stmt list) : checked_stmt list =
  scoped_declarations (fun () ->
    declare_types body;
    declare_traits body;
    declare_impls ctx.registry body;
    hoist env body;
    let assigned = assigned_names body in
    List.map (fun s -> infer_stmt env ctx assigned s) body)

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
    let body =
      in_function_body ctx ~ret:declared_ret ~row:declared_row (fun () ->
        infer_block scope ctx body)
    in
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
  | `Defer inner -> node (`Defer (infer_stmt env ctx assigned inner))
  | `Type_decl (name, params, body) -> node (`Type_decl (name, params, body))
  | `Op_decl (op, params, signature, body) ->
    let mangled = Ast.op_entry_name op params signature in
    with_type_params
      (Option.value ~default:[] (Hashtbl.find_opt ctx_fn_params mangled))
      (fun () ->
    let scope = new_env (Some env) in
    let param_types =
      match lookup env mangled with
      | Some { Types.body = Types.IFn (ps, _, _); _ }
        when List.length ps = List.length params -> ps
      | _ -> List.map (fun (p : Ast.param) -> annotated_or_fresh p.Ast.ty) params
    in
    List.iter2
      (fun (p : Ast.param) ty -> bind scope p.Ast.name (Types.mono ty))
      params
      param_types;
    let declared_ret = annotated_or_fresh signature.Ast.ret in
    let declared_row = Types.fresh_row () in
    let body =
      in_function_body ctx ~ret:declared_ret ~row:declared_row (fun () ->
        infer_block scope ctx body)
    in
    Ast.annotated
      span
      (Types.IFn (param_types, declared_ret, declared_row))
      (`Op_decl (op, params, signature, body)))
  | `Trait_decl (name, params, methods) -> node (`Trait_decl (name, params, methods))
  | `Impl_decl (trait, type_name, params, impl) ->
    let inferred =
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
          let body =
            in_function_body ctx ~ret:declared_ret ~row:declared_row (fun () ->
              infer_block scope ctx m.Ast.md_body)
          in
          let fn_type = Types.IFn (param_types, declared_ret, declared_row) in
          mangled, fn_type, { m with Ast.md_body = body; md_ann = fn_type }))
        impl.Ast.ib_methods
    in
    (* One variable per impl type parameter is shared by every method, so a
       sibling still bound to it monomorphically makes it look used by the
       enclosing scope and nothing is quantified. The impl generalizes as a
       group or not at all. *)
    List.iter (fun (mangled, _, _) -> Hashtbl.remove env.bindings mangled) inferred;
    let env_vars = env_free_vars env
    and env_rows = env_free_row_vars env
    and env_fields = env_free_field_vars env in
    List.iter
      (fun (mangled, fn_type, _) ->
        bind env mangled (Types.generalize ~env_vars ~env_rows ~env_fields fn_type))
      inferred;
    node
      (`Impl_decl (trait, type_name, params, { impl with Ast.ib_methods = List.map (fun (_, _, m) -> m) inferred }))
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
    let vars, variants =
      match Hashtbl.find_opt ctx_types sum with
      | Some (Sum (vars, variants)) -> vars, variants
      | _ -> fail span "Unknown sum type '%s'." sum
    in
    (* Matching a constructor is learning that the scrutinee's arguments are the
       ones that constructor builds. For an ordinary variant those are the
       type's own parameters and this says nothing; for a GADT it is where
       `Expr<T>` becomes `Expr<int>` — and only for this arm, which is why it is
       solved into a substitution rather than unified into the store. *)
    let refine declared =
      let freshened = instantiation vars declared in
      let head = List.map (Types.substitute freshened) declared.vd_result in
      match Types.solve (List.combine sum_args head) with
      | Some refinement -> freshened, refinement
      | None -> raise Not_found
    in
    (* A variant whose head cannot be the scrutinee's type is not reachable
       here, so a match that leaves it out is still exhaustive — which is what
       lets `head` take a vector that cannot be `Nil`. *)
    let reachable declared =
      match refine declared with
      | _ -> true
      | exception Not_found -> false
    in
    (* Everything the arm sees is rewritten by the refinement: the payload it
       binds, the return type it must produce, the names already in scope whose
       type mentions a refined variable, and what a written `T` means inside it.
       The store is left alone, so a constraint the arm places on anything else
       is ordinary and permanent. *)
    let refined_scope refinement =
      let scope = new_env (Some env) in
      (* A variable the scheme quantifies is bound by it, not by the match — the
         recursive occurrence of the function being checked quantifies the very
         parameter being refined, and substituting into it would make the
         function monomorphic at this arm's type. *)
      let applicable (scheme : Types.scheme) =
        List.filter (fun (id, _) -> not (List.mem id scheme.Types.quantified)) refinement
      in
      let rec walk visible =
        Option.iter walk visible.parent;
        Hashtbl.iter
          (fun name (scheme : Types.scheme) ->
            match applicable scheme with
            | [] -> ()
            | refinement ->
              if
                List.exists
                  (fun (id, _) -> List.mem_assoc id refinement)
                  (Types.free_vars scheme.Types.body)
              then
                bind
                  scope
                  name
                  { scheme with Types.body = Types.substitute refinement scheme.Types.body })
          visible.bindings
      in
      walk env;
      scope
    in
    let refined_params refinement =
      Hashtbl.fold
        (fun name var found ->
          match Types.repr var with
          | Types.IVar { contents = Types.Unbound (id, _) } when List.mem_assoc id refinement ->
            (name, Types.substitute refinement var) :: found
          | _ -> found)
        ctx_type_params
        []
    in
    let covered = ref [] in
    let arm variant declared payload body =
      let freshened, refinement =
        if not declared.vd_refines
        then instance vars sum_args, []
        else (
          match refine declared with
          | solved -> solved
          (* The head it builds cannot be the scrutinee's type, so no value
             reaching here was ever built by it. *)
          | exception Not_found ->
            fail
              span
              "'%s' cannot be a %s, so this arm can never match."
              variant
              (Types.string_of_infer_ty (Types.ISum (sum, sum_args))))
      in
      let scope = if refinement = [] then new_env (Some env) else refined_scope refinement in
      let expected =
        List.map
          (fun (l, t) -> l, Types.substitute refinement (Types.substitute freshened t))
          (Ast.payload_fields declared.vd_payload)
      in
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
        bindings;
      if refinement = []
      then infer_block scope ctx body
      else
        with_type_params (refined_params refinement) (fun () ->
          (* [in_ctx] puts the whole context back, and a `return` in the arm is
             a fact about the function rather than about the arm — so that one
             field is carried out by hand. *)
          let returned = ref false in
          let checked =
            in_ctx
              ctx
              ~set:(fun () ->
                ctx.return_type <- Option.map (Types.substitute refinement) ctx.return_type)
              (fun () ->
                let checked = infer_block scope ctx body in
                returned := ctx.saw_return;
                checked)
          in
          if !returned then ctx.saw_return <- true;
          checked)
    in
    let cases =
      List.map
        (fun ((pattern : Ast.pattern), body) ->
          match pattern with
          | Ast.Pat_wild ->
            covered := List.map fst variants;
            pattern, infer_block (new_env (Some env)) ctx body
          | Ast.Pat_variant (ty, variant, payload) ->
            if not (String.equal ty sum)
            then fail span "This matches a %s, not a %s." ty sum;
            (match List.assoc_opt variant variants with
             | None -> fail span "Type '%s' has no variant '%s'." sum variant
             | Some declared ->
               covered := variant :: !covered;
               pattern, arm variant declared payload body))
        cases
    in
    List.iter
      (fun (name, declared) ->
        if (not (List.mem name !covered)) && reachable declared
        then fail span "This match does not cover '%s'." name)
      variants;
    node (`Match (scrutinee, cases))
  | `Effect_decl (name, params, ops) ->
    Hashtbl.replace ctx_effects.declared name ops;
    (* One variable per parameter for the whole declaration, so operations
       naming it agree, and a call site is what settles what it is. *)
    let bound = List.map (fun p -> p, Types.fresh ()) params in
    Hashtbl.replace ctx_effect_params name bound;
    with_type_params bound (fun () ->
    List.iter
      (fun (o : Ast.op_decl) ->
        (match Hashtbl.find_opt ctx_op_owner o.Ast.op_name with
         | Some owner when not (String.equal owner name) ->
           fail span "Effect '%s' already declares an operation '%s'." owner o.Ast.op_name
         | _ -> ());
        Hashtbl.replace ctx_op_owner o.Ast.op_name name;
        Hashtbl.replace ctx_effects.ops o.Ast.op_name o;
        let params =
          List.map (fun (p : Ast.param) -> annotated_or_fresh p.Ast.ty) o.Ast.op_params
        in
        let ret = annotated_or_fresh o.Ast.op_ret in
        (* The entry carries the declaration's parameters, so a use of the
           operation instantiates the effect along with the operation. *)
        let op_type =
          Types.IFn (params, ret, Types.RCons (name, List.map snd bound, Types.fresh_row ()))
        in
        (* A `final ctl` never hands anything back, so every call site may read
           its result as whatever it needs. Quantifying is sound for exactly
           that reason, and only over what the arguments do not pin down. *)
        let effect_vars =
          List.filter_map
            (fun (_, v) ->
              match Types.repr v with
              | Types.IVar { contents = Types.Unbound (id, _) } -> Some id
              | _ -> None)
            bound
        in
        let quantified =
          effect_vars
          @
          match o.Ast.op_kind with
          | Ast.Op_fn | Ast.Op_ctl -> []
          | Ast.Op_final ->
            let pinned = List.concat_map Types.free_vars params |> List.map fst in
            Types.free_vars ret
            |> List.map fst
            |> List.filter (fun id -> not (List.mem id pinned))
        in
        bind
          env
          o.Ast.op_name
          { Types.quantified
          ; quantified_rows = Types.free_row_vars op_type
          ; quantified_fields = []
          ; body = op_type
          })
      ops);
    node (`Effect_decl (name, params, ops))
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
    let body =
      in_ctx ctx ~set:(fun () -> ctx.row <- body_row) (fun () ->
        infer_block (new_env (Some env)) ctx body)
    in
    (* Each handler discharges one instantiation, and which one is settled by
       unifying against what its arms turn out to take. *)
    let instantiated =
      List.map
        (fun (h : Ast.desugared_stmt Ast.handler) ->
          let arity =
            List.length
              (Option.value ~default:[] (Hashtbl.find_opt ctx_effect_params h.Ast.handled))
          in
          h, List.init arity (fun _ -> Types.fresh ()))
        handlers
    in
    let remaining =
      List.fold_left
        (fun row ((h : Ast.desugared_stmt Ast.handler), args) ->
          try Types.rewrite_row h.Ast.handled args row with
          | Types.Type_error _ -> row)
        body_row
        instantiated
    in
    Types.unify_row remaining ctx.row;
    node
      (`Run
        (body, List.map (fun (h, args) -> infer_handler env ctx assigned ~args h) instantiated))
  | `Resume value ->
    let expected =
      match ctx.resume_type with
      | Some t -> t
      | None ->
        if ctx.in_final_arm
        then
          fail
            span
            "A 'final ctl' handler cannot resume — that is what lets its result \
             be whatever each call site needs."
        else fail span "'resume' outside of a 'ctl' handler."
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
and infer_handler env ctx assigned ~args (h : Ast.desugared_stmt Ast.handler)
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
    let body =
      in_ctx
        ctx
        ~set:(fun () ->
          ctx.resume_type
          <- (match a.Ast.arm_kind with
              | Ast.Op_ctl -> Some (annotated_or_fresh op.Ast.op_ret)
              | Ast.Op_fn | Ast.Op_final -> None);
          ctx.in_final_arm <- a.Ast.arm_kind = Ast.Op_final;
          ctx.return_type <- Some (annotated_or_fresh op.Ast.op_ret);
          ctx.saw_return <- false)
        (fun () -> List.map (infer_stmt scope ctx assigned) a.Ast.arm_body)
    in
    { Ast.arm_name = a.Ast.arm_name
    ; arm_kind = a.Ast.arm_kind
    ; arm_params = a.Ast.arm_params
    ; arm_body = body
    }
  in
  let names =
    List.map fst (Option.value ~default:[] (Hashtbl.find_opt ctx_effect_params h.Ast.handled))
  in
  with_type_params
    (try List.combine names args with
     | Invalid_argument _ -> [])
    (fun () -> { Ast.handled = h.Ast.handled; arms = List.map arm h.Ast.arms })

let rec resolve_expr (e : checked_expr) : Ast.typed_expr =
  let it : Ast.typed_expr_kind =
    match e.Ast.it with
    | #Ast.lit as l -> l
    | `Lambda (params, signature, body) ->
      `Lambda (params, signature, List.map resolve_stmt body)
    | #Ast.arrays as a -> (Ast.map_arrays resolve_expr a :> Ast.typed_expr_kind)
    | #Ast.strings as s -> (Ast.map_strings resolve_expr s :> Ast.typed_expr_kind)
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

and resolve_stmt (s : checked_stmt) : Ast.typed_stmt =
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
   Quantifying gives every call site its own. *)
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

let admits registry kind (t : Types.infer_ty) =
  match kind with
  | Types.Collection elem ->
    (match Types.infer_type_name t with
     | Some name when String.equal name Types.array_name ->
       (match Types.container_element t with
        | Some (_, held) ->
          Types.unify elem held;
          true
        | None -> false)
     | Some name ->
       (match Registry.container_element registry name t with
        | Some held ->
          Types.unify elem held;
          true
        | None -> false)
     | None -> false)
  (* Written `<T: Summary>`: the type must have an `impl Summary for` it. *)
  | Types.Bound traits ->
    (match Types.infer_type_name t with
     | Some name ->
       List.for_all
         (fun (trait, args) ->
           List.exists
             (fun declared ->
               List.length declared = List.length args
               &&
               try
                 List.iter2 Types.unify args declared;
                 true
               with
               | Types.Type_error _ -> false)
             (Hashtbl.find_all ctx_impls (name, trait)))
         traits
     | None -> false)
  | Types.Any -> true
  (* Nothing is known about what it stands for until its owner is, and by then
     it is no longer a variable with this kind. *)
  | Types.Projection _ -> true
  | Types.Addable | Types.Numeric ->
    (match Types.concrete t with
     | None -> false
     | Some ty ->
       let has op = Registry.find registry op ty ty <> None in
       (match kind with
        | Types.Addable -> has Ast.Add
        | _ -> List.exists has [ Ast.Sub; Ast.Mul; Ast.Div; Ast.Mod; Ast.Less; Ast.Greater ]))

let declare_builtins env =
  (* A receiver of unknown type has to see the length method to be ambiguous. *)
  Hashtbl.replace ctx_methods (Types.array_name, Types.array_len) ();
  Hashtbl.replace ctx_methods (Types.string_name, Types.array_len) ();
  List.iter
    (fun (name, arity) ->
      Hashtbl.replace ctx_types name (Opaque (List.init arity (fun _ -> Types.fresh ()))))
    Builtins.types;
  List.iter
    (fun (owner, name, signature) ->
      let params, ret = signature () in
      Hashtbl.replace ctx_methods (owner, name) ();
      bind env (Ast.method_name owner name) (pure params ret))
    Builtins.methods;
  List.iter
    (fun (name, signature) ->
      let params, ret = signature () in
      bind env name (pure params ret))
    Builtins.functions;
  List.iter
    (fun (name, result) ->
      let result = result () in
      let scheme =
        { Types.quantified = []
        ; quantified_rows = []
        ; quantified_fields = []
        ; body = Types.IFn ([], result, Types.REmpty)
        }
      in
      Hashtbl.replace ctx_variadic name (scheme, result);
      bind env name scheme)
    Builtins.variadic

let check ~registry (program : Ast.desugared_stmt list)
  : (Ast.typed_stmt list, error list) result
  =
  Types.reset ();
  Types.extra_admits := admits registry;
  Types.assoc_binding := (fun owner member -> Hashtbl.find_opt ctx_assoc (owner, member));
  reset_effects ();
  let env = new_env None in
  declare_builtins env;
  let ctx =
    { registry
    ; return_type = None
    ; saw_return = false
    ; row = Types.fresh_row ()
    ; resume_type = None
    ; in_final_arm = false
    }
  in
  let errors = ref [] in
  (* Per statement, so one bad declaration does not skip the rest — everything
     after it would then be checked without the signatures it needs. *)
  let each pass =
    List.iter
      (fun s ->
        try pass [ s ] with
        | Located e -> errors := e :: !errors)
      program
  in
  each declare_types;
  each declare_traits;
  each (declare_impls registry);
  each (declare_ops registry);
  each (hoist env);
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
      { span = { Ast.file = ""; line = 1; col = 1 }
      ; message =
          Printf.sprintf
            "Unhandled effect(s): %s."
            (String.concat ", " (List.map (Types.entry Types.string_of_ty) labels))
      }
      :: !errors
  in
  match List.rev errors with
  | [] -> Ok (List.map resolve_stmt checked)
  | errors -> Error errors
