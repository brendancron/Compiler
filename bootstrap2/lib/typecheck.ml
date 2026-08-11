(* Inference cannot finish a node's type when it first visits it — in
   `fn f(x) { return x + 1; }`, `x` is only pinned at the `+` — so the tree is
   built with mutable types and converted once everything is known. *)

type error =
  { span : Ast.span
  ; message : string
  }

(* [Types.Type_error] is raised deep in unification with no idea where it is;
   this is the same failure once a caller has attached a location. *)
exception Located of error

type checked_expr = (checked_expr_kind, Types.infer_ty) Ast.node

and checked_expr_kind =
  [ Ast.lit
  | checked_expr Ast.vars
  | checked_expr Ast.ops
  | checked_expr Ast.logic
  | checked_expr Ast.reflect
  ]

type checked_stmt = (checked_stmt_kind, Types.infer_ty) Ast.node

and checked_stmt_kind =
  [ (checked_expr, checked_stmt) Ast.stmts
  | (checked_expr, checked_stmt) Ast.effects
  ]

type env =
  { bindings : (string, Types.scheme) Hashtbl.t
  ; parent : env option
  }

type ctx =
  { mutable return_type : Types.infer_ty option
  ; mutable saw_return : bool
  (* The row of the enclosing function or run block. Effectful calls rewrite it
     to admit their label; it stays open until generalization closes it. *)
  ; mutable row : Types.infer_row
  (* The operation an enclosing `ctl` arm is handling, if any — what `resume`
     checks its argument against. *)
  ; mutable resume_type : Types.infer_ty option
  }

(* Operations declared by `effect` blocks, keyed by operation name and by the
   effect that owns them. *)
type effect_info =
  { ops : (string, Ast.op_decl) Hashtbl.t
  ; declared : (string, Ast.op_decl list) Hashtbl.t
  }

let ctx_effects = { ops = Hashtbl.create 16; declared = Hashtbl.create 8 }

let reset_effects () =
  Hashtbl.reset ctx_effects.ops;
  Hashtbl.reset ctx_effects.declared

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

let fail span fmt =
  Printf.ksprintf (fun message -> raise (Located { span; message })) fmt

(* Blames [span] rather than the enclosing statement. *)
let unify_at span expected actual =
  try Types.unify expected actual with
  | Types.Type_error message -> raise (Located { span; message })

(* ---- annotations ---- *)

let rec infer_ty_of_annotation (t : Ast.type_expr) : Types.infer_ty =
  match t.Ast.it with
  | Ast.Ty_name "int" -> Types.IInt
  | Ast.Ty_name "float" -> Types.IFloat
  | Ast.Ty_name "string" -> Types.IStr
  | Ast.Ty_name "bool" -> Types.IBool
  | Ast.Ty_name "unit" -> Types.IUnit
  | Ast.Ty_name other -> fail t.Ast.span "Unknown type '%s'." other
  | Ast.Ty_fn (params, ret, row) ->
    Types.IFn
      ( List.map infer_ty_of_annotation params
      , infer_ty_of_annotation ret
      , row_of_labels row )

(* A written row is closed: `(int) -> unit` is a promise of purity. *)
and row_of_labels labels =
  List.fold_right (fun label rest -> Types.RCons (label, rest)) labels Types.REmpty

let annotated_or_fresh = function
  | Some t -> infer_ty_of_annotation t
  | None -> Types.fresh ()

(* ---- the value restriction ---- *)

(* Generalizing a binding that is later assigned is unsound: every use
   instantiates fresh, so `var f = someGenericFn; f = otherFn;` would check and
   then misbehave. Hence both this and the never-assigned test below. *)
let is_syntactic_value (e : Ast.desugared_expr) =
  match e.Ast.it with
  | #Ast.lit | `Var _ -> true
  | _ -> false

let rec assigned_in_expr (e : Ast.desugared_expr) acc =
  match e.Ast.it with
  | #Ast.lit | `Var _ -> acc
  | `Assign (name, v) -> assigned_in_expr v (name :: acc)
  | `Unop (_, a) -> assigned_in_expr a acc
  | `Binop (_, a, b) | `And (a, b) | `Or (a, b) ->
    assigned_in_expr b (assigned_in_expr a acc)
  | `Call (callee, args) ->
    List.fold_left (fun acc a -> assigned_in_expr a acc) (assigned_in_expr callee acc) args
  | `Typeof e -> assigned_in_expr e acc

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
  | `Effect_decl _ -> acc
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

(* ---- inference ---- *)

(* No HM type describes "any number of arguments of any type", so calls to these
   are checked structurally and a bare reference gets an unconstrained type. *)
let variadic_builtins = [ "print" ]

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
    Types.unify a.Ast.ann b.Ast.ann;
    let result =
      match op with
      (* `+` is also string concatenation. *)
      | Ast.Add ->
        Types.unify a.Ast.ann (Types.fresh_with Types.Addable);
        a.Ast.ann
      | Ast.Sub | Ast.Mul | Ast.Div ->
        Types.unify a.Ast.ann (Types.fresh_with Types.Numeric);
        a.Ast.ann
      | Ast.Less | Ast.Less_equal | Ast.Greater | Ast.Greater_equal ->
        Types.unify a.Ast.ann (Types.fresh_with Types.Numeric);
        Types.IBool
      | Ast.Equal | Ast.Not_equal -> Types.IBool
    in
    node result (`Binop (op, a, b))
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
        (* Whatever the callee performs, the caller performs too. *)
        Types.unify_row row ctx.row;
        ret
    in
    node result (`Call (callee_node, args))
  (* The operand is checked for its type but never evaluated. *)
  | `Typeof e -> node Types.IStr (`Typeof (infer_expr env ctx e))

(* Binding every function before any body is checked is what lets them call each
   other in any order. The binding stays monomorphic until its own declaration
   is reached. *)
and hoist env (body : Ast.desugared_stmt list) =
  List.iter
    (fun (s : Ast.desugared_stmt) ->
      match s.Ast.it with
      | `Fn (name, params, signature, _) ->
        let param_types = List.map (fun (p : Ast.param) -> annotated_or_fresh p.Ast.ty) params in
        let row =
          match signature.Ast.row with
          | Some labels -> row_of_labels labels
          | None -> Types.fresh_row ()
        in
        bind
          env
          name
          (Types.mono (Types.IFn (param_types, annotated_or_fresh signature.Ast.ret, row)))
      | _ -> ())
    body

and infer_block env ctx (body : Ast.desugared_stmt list) : checked_stmt list =
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
        (* The evaluator gives an uninitialized variable unit. *)
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
    (* Recover the type [hoist] chose, so recursive calls in the body unify
       against the same variables. *)
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
    (* The body's effects accumulate into the row hoisting chose for it. *)
    ctx.row <- declared_row;
    let body = infer_block scope ctx body in
    (* Falling off the end yields unit. *)
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
         fn_type);
    Ast.annotated span fn_type (`Fn (name, params, signature, body))
  | `Effect_decl (name, ops) ->
    Hashtbl.replace ctx_effects.declared name ops;
    List.iter
      (fun (o : Ast.op_decl) ->
        Hashtbl.replace ctx_effects.ops o.Ast.op_name o;
        (* An operation is callable like a function whose row is its own effect,
           so calling it is what puts the label in the caller's row. *)
        let params =
          List.map (fun (p : Ast.param) -> annotated_or_fresh p.Ast.ty) o.Ast.op_params
        in
        let ret = annotated_or_fresh o.Ast.op_ret in
        (* The row is open in its tail: performing an operation adds its label
           to whatever the caller already performs rather than closing it. *)
        let op_type = Types.IFn (params, ret, Types.RCons (name, Types.fresh_row ())) in
        bind
          env
          o.Ast.op_name
          { Types.quantified = []
          ; quantified_rows = Types.free_row_vars op_type
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
    (* Each `handle` peels one occurrence of its label; what is left over is
       performed by the run block as a whole. *)
    let remaining =
      List.fold_left
        (fun row (h : Ast.desugared_stmt Ast.handler) ->
          (* A handler for an effect the block never performs is redundant, not
             an error. *)
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
    (* A `fn` arm's value is what the operation returns. *)
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

(* ---- resolve ---- *)

let rec resolve_expr (e : checked_expr) : Ast.typed_expr =
  let it : Ast.typed_expr_kind =
    match e.Ast.it with
    | #Ast.lit as l -> l
    | #Ast.vars as v -> (Ast.map_vars resolve_expr v :> Ast.typed_expr_kind)
    | #Ast.ops as o -> (Ast.map_ops resolve_expr o :> Ast.typed_expr_kind)
    | #Ast.logic as l -> (Ast.map_logic resolve_expr l :> Ast.typed_expr_kind)
    | #Ast.reflect as r -> (Ast.map_reflect resolve_expr r :> Ast.typed_expr_kind)
  in
  { Ast.it; span = e.Ast.span; ann = Types.resolve e.Ast.ann }

let rec resolve_stmt (s : checked_stmt) : Ast.typed_stmt =
  let it : Ast.typed_stmt_kind =
    match s.Ast.it with
    | #Ast.stmts as st -> (Ast.map_stmts resolve_expr resolve_stmt st :> Ast.typed_stmt_kind)
    | #Ast.effects as e ->
      (Ast.map_effects resolve_expr resolve_stmt e :> Ast.typed_stmt_kind)
  in
  { Ast.it; span = s.Ast.span; ann = Types.resolve s.Ast.ann }

(* ---- entry point ---- *)

let globals () =
  let env = new_env None in
  let alpha = Types.fresh () in
  let quantified = List.map fst (Types.free_vars alpha) in
  bind
    env
    "str"
    { Types.quantified
    ; quantified_rows = []
    ; body = Types.IFn ([ alpha ], Types.IStr, Types.REmpty)
    };
  bind env "clock" (Types.mono (Types.IFn ([], Types.IFloat, Types.REmpty)));
  (* See [variadic_builtins]. *)
  let beta = Types.fresh () in
  bind
    env
    "print"
    { Types.quantified = List.map fst (Types.free_vars beta)
    ; quantified_rows = []
    ; body = beta
    };
  env

let check (program : Ast.desugared_stmt list) : (Ast.typed_stmt list, error list) result =
  Types.reset ();
  reset_effects ();
  let env = globals () in
  let ctx =
    { return_type = None
    ; saw_return = false
    ; row = Types.fresh_row ()
    ; resume_type = None
    }
  in
  hoist env program;
  let assigned = assigned_names program in
  let errors = ref [] in
  let checked =
    List.filter_map
      (fun s ->
        try Some (infer_stmt env ctx assigned s) with
        | Located e ->
          errors := e :: !errors;
          None)
      program
  in
  (* Nothing encloses the top level, so anything still in its row is unhandled. *)
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
