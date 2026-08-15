(* A meta block is compiled and run where it stands, then removed. Everything it
   needs is already here — this pass is the rest of the pipeline applied to a
   fragment of the program it belongs to, which is what makes metaprocessing
   recursive compilation rather than a second language. *)

type error =
  { span : Ast.span
  ; message : string
  }

exception Failed of error

let fail span fmt =
  Printf.ksprintf (fun message -> raise (Failed { span; message })) fmt

(* Everything a block can refer to: what the program declared before it, and
   nothing it declared after. A meta block sees the program as it stood when
   compilation reached it. *)
let is_declaration (s : Ast.stmt) =
  match s.Ast.it with
  | `Fn _ | `Type_decl _ | `Trait_decl _ | `Impl_decl _ | `Op_decl _ | `Effect_decl _
  | `Handler_decl _ -> true
  | _ -> false

(* ---- what a block emits ---- *)

let emitter = Ast.generated [ "meta"; "emit" ]
let capturer = Ast.generated [ "meta"; "value" ]
let quoter = Ast.generated [ "meta"; "code" ]

(* Only a scalar has a literal form, so only a scalar is written out. Anything
   else keeps its name and is resolved in the program the code lands in — the
   same rule for an expression and for a name position, except that a name can
   only be a string. *)
let literal_of span (v : Value.value) : Ast.expr option =
  match v with
  (* Already substituted when it was captured, so it stands as it is. *)
  | Value.Code e -> Some e
  | Value.Int n -> Some (Ast.at span (`Int n))
  | Value.Float n -> Some (Ast.at span (`Float n))
  | Value.Str s -> Some (Ast.at span (`Str s))
  | Value.Bool b -> Some (Ast.at span (`Bool b))
  | Value.Chr c -> Some (Ast.at span (`Char c))
  | _ -> None

let name_of (v : Value.value) =
  match v with
  | Value.Name n -> Some n
  | _ -> None

let substitution (bound : (string, Value.value) Hashtbl.t) =
  let named name =
    match Hashtbl.find_opt bound name with
    | Some v -> Option.value (name_of v) ~default:name
    | None -> name
  in
  (* A generated declaration names types as well as values, so an annotation
     takes a computed name the same way a constructor does. *)
  let rec type_expr (t : Ast.type_expr) : Ast.type_expr =
    let it =
      match t.Ast.it with
      | Ast.Ty_name n -> Ast.Ty_name (named n)
      | Ast.Ty_app (n, args) -> Ast.Ty_app (named n, List.map type_expr args)
      | Ast.Ty_tuple items -> Ast.Ty_tuple (List.map type_expr items)
      | Ast.Ty_record fields ->
        Ast.Ty_record (List.map (fun (l, t) -> l, type_expr t) fields)
      | Ast.Ty_fn (args, ret, row) ->
        Ast.Ty_fn (List.map type_expr args, type_expr ret, row)
    in
    { t with Ast.it }
  in
  let param (p : Ast.param) = { p with Ast.ty = Option.map type_expr p.Ast.ty } in
  let signature (sg : Ast.signature) =
    { sg with
      Ast.ret = Option.map type_expr sg.Ast.ret
    ; comptime =
        List.map
          (fun (c : Ast.comptime_param) -> { c with Ast.cp_ty = Option.map type_expr c.Ast.cp_ty })
          sg.Ast.comptime
    }
  in
  let rec expr (e : Ast.expr) : Ast.expr =
    match e.Ast.it with
    | `Var name ->
      (match Option.bind (Hashtbl.find_opt bound name) (literal_of e.Ast.span) with
       | Some replacement -> replacement
       | None -> e)
    | it ->
      let it : Ast.expr_kind =
        match it with
        | `Lambda (params, sg, body) ->
          `Lambda (List.map param params, signature sg, List.map stmt body)
        | `New (name, fields) -> `New (named name, List.map (fun (l, v) -> l, expr v) fields)
        | `New_variant (ty, variant, payload) ->
          `New_variant (named ty, variant, Ast.map_payload expr payload)
        | `New_call (name, args, values) -> `New_call (named name, args, List.map expr values)
        (* A name position takes a name the meta program computed, which is
           what lets generated code reach a field it was told about. *)
        | `Method_call (receiver, name, as_function, args) ->
          `Method_call (expr receiver, named name, named as_function, List.map expr args)
        (* Captured syntax nested in captured syntax stands as it is; it is
           lowered when the code holding it runs. *)
        | `Code _ as c -> c
        | `Field (receiver, label) -> `Field (expr receiver, named label)
        | `Field_assign (receiver, label, v) ->
          `Field_assign (expr receiver, named label, expr v)
        | #Ast.lit as l -> l
        | #Ast.vars as v -> (Ast.map_vars expr v :> Ast.expr_kind)
        | #Ast.ops as o -> (Ast.map_ops expr o :> Ast.expr_kind)
        | #Ast.logic as l -> (Ast.map_logic expr l :> Ast.expr_kind)
        | #Ast.compound as c -> (Ast.map_compound expr c :> Ast.expr_kind)
        | #Ast.indexing as i -> (Ast.map_indexing expr i :> Ast.expr_kind)
        | #Ast.tuple as t -> (Ast.map_tuple expr t :> Ast.expr_kind)
        | #Ast.record as r -> (Ast.map_record expr r :> Ast.expr_kind)
        | #Ast.collection as c -> (Ast.map_collection expr c :> Ast.expr_kind)
        | #Ast.comptime_call as c -> (Ast.map_comptime_call expr c :> Ast.expr_kind)
        | #Ast.reflect as r -> (Ast.map_reflect expr r :> Ast.expr_kind)
      in
      { e with Ast.it }
  and stmt (s : Ast.stmt) : Ast.stmt =
    let it : Ast.stmt_kind =
      match s.Ast.it with
      | `Fn (name, params, sg, body) ->
        `Fn (named name, List.map param params, signature sg, List.map stmt body)
      (* An impl names the type it is written for, so a generated one can be
         written for a type the meta program named. *)
      | `Impl_decl (trait, type_name, params, methods) ->
        `Impl_decl
          ( Option.map named trait
          , named type_name
          , params
          , List.map
              (fun (m : (Ast.stmt, unit) Ast.method_def) ->
                { m with
                  Ast.md_params = List.map param m.Ast.md_params
                ; md_signature = signature m.Ast.md_signature
                ; md_body = List.map stmt m.Ast.md_body
                })
              methods )
      | `Derive (traits, target) -> `Derive (traits, named target)
      | `Type_decl (name, params, body) -> `Type_decl (named name, params, body)
      | `Trait_decl (name, methods) -> `Trait_decl (named name, methods)
      | `Gen inner -> `Gen (stmt inner)
      | `Meta body -> `Meta (List.map stmt body)
      | `Meta_fn (n, params, sg, body) ->
        `Meta_fn (named n, List.map param params, signature sg, List.map stmt body)
      | `Import decl -> `Import decl
      | `Var_decl (name, ty, init) ->
        `Var_decl (name, Option.map type_expr ty, Option.map expr init)
      | #Ast.stmts as st -> (Ast.map_stmts expr stmt st :> Ast.stmt_kind)
      | #Ast.loops as l -> (Ast.map_loops expr stmt l :> Ast.stmt_kind)
      | #Ast.effects as e ->
        let clause (c : Ast.stmt Ast.handler_clause) =
          match c with
          | Ast.Inline h -> Ast.Inline (Ast.map_handler stmt h)
          | Ast.Named n -> Ast.Named n
        in
        (Ast.map_effects expr stmt clause e :> Ast.stmt_kind)
      | `Handler_decl (n, h) -> `Handler_decl (n, Ast.map_handler stmt h)
      | #Ast.op_defs as o -> (Ast.map_op_defs stmt o :> Ast.stmt_kind)
      | #Ast.matching as m -> (Ast.map_matching expr stmt m :> Ast.stmt_kind)
    in
    { s with Ast.it }
  in
  expr, stmt

let substitute bound (root : Ast.stmt) : Ast.stmt = snd (substitution bound) root
let substitute_expr bound (e : Ast.expr) : Ast.expr = fst (substitution bound) e

(* A lowered `gen` or `code` carries its meta-bound names beside its index. *)
let bindings_of args =
  let bound = Hashtbl.create 8 in
  let rec pairs = function
    | Value.Str name :: value :: rest ->
      Hashtbl.replace bound (Utf8.encode name) value;
      pairs rest
    | _ -> ()
  in
  pairs args;
  bound

let run ~out ~codes ~emit ~capture (program : Ast.program) =
  match Desugar.program (Prelude.program () @ program) with
  | Error e -> fail e.Desugar.span "%s" e.Desugar.message
  | Ok desugared ->
    (match Monomorphize.program desugared with
     | Error e -> fail e.Monomorphize.span "%s" e.Monomorphize.message
     | Ok desugared ->
       let registry = Registry.builtins () in
       (match Typecheck.check ~registry desugared with
        | Error [] -> fail { Ast.file = ""; line = 1; col = 1 } "The meta block does not check."
        | Error (e :: _) -> fail e.Typecheck.span "%s" e.Typecheck.message
        | Ok typed ->
          (match Resolve.program ~registry (Specialize.program typed) with
           | Error e -> fail e.Resolve.span "%s" e.Resolve.message
           | Ok resolved ->
             (match Reflect.program resolved with
              | Error e -> fail e.Reflect.span "%s" e.Reflect.message
              | Ok reflected ->
             match Cps.program reflected with
              | Error e -> fail e.Cps.span "%s" e.Cps.message
              | Ok converted ->
                (match Verify.program converted with
                 | Error e -> fail e.Verify.span "%s" e.Verify.message
                 | Ok () ->
                   let env = Builtins.env ~out in
                   Value.define
                     env
                     capturer
                     (Value.Fn
                        { Value.name = capturer
                        ; arity = Some 1
                        ; apply =
                            (fun _ args ->
                              (match args with
                               | [ v ] -> capture v
                               | _ -> ());
                              Value.Unit)
                        });
                   Value.define
                     env
                     quoter
                     (Value.Fn
                        { Value.name = quoter
                        ; arity = None
                        ; apply =
                            (fun span args ->
                              match args with
                              | Value.Int index :: rest ->
                                (match Hashtbl.find_opt codes index with
                                 | Some captured ->
                                   Value.Code (substitute_expr (bindings_of rest) captured)
                                 | None -> Value.fail span "Nothing was captured here.")
                              | _ -> Value.fail span "Nothing was captured here.")
                        });
                   Value.define
                     env
                     emitter
                     (Value.Fn
                        { Value.name = emitter
                        ; arity = None
                        ; apply =
                            (fun span args ->
                              emit span args;
                              Value.Unit)
                        });
                   (match Interp.run env converted with
                    | Ok () -> ()
                    | Error e -> fail e.Value.span "%s" e.Value.message))))))

(* Lowering a `gen`: what follows it goes into a table, and the statement
   becomes a call carrying that entry's index and the meta-bound names it
   mentions. Nothing else can reach the interpreter — a captured statement is
   surface syntax, and every IR after this point has dropped it. *)
let lower ~table ~codes ~params (body : Ast.program) =
  let arguments sp scope =
    Ast.at sp (`Int (-1))
    :: List.concat_map
         (fun name -> [ Ast.at sp (`Str (Utf8.decode name)); Ast.at sp (`Var name) ])
         (List.sort_uniq String.compare scope)
  in
  let call sp index scope callee =
    match arguments sp scope with
    | _ :: rest -> Ast.at sp (`Call (Ast.at sp (`Var callee), Ast.at sp (`Int index) :: rest))
    | [] -> assert false
  in
  let rec expr scope (e : Ast.expr) : Ast.expr =
    let sp = e.Ast.span in
    match e.Ast.it with
    | `Code inner ->
      let index = Hashtbl.length codes in
      Hashtbl.replace codes index inner;
      { e with Ast.it = (call sp index scope quoter).Ast.it }
    | it ->
      let expr = expr scope in
      let it : Ast.expr_kind =
        match it with
        | `Code _ as c -> c
        | `Lambda (params, sg, body) -> `Lambda (params, sg, block scope body)
        | #Ast.lit as l -> l
        | #Ast.vars as v -> (Ast.map_vars expr v :> Ast.expr_kind)
        | #Ast.ops as o -> (Ast.map_ops expr o :> Ast.expr_kind)
        | #Ast.logic as l -> (Ast.map_logic expr l :> Ast.expr_kind)
        | #Ast.compound as c -> (Ast.map_compound expr c :> Ast.expr_kind)
        | #Ast.indexing as i -> (Ast.map_indexing expr i :> Ast.expr_kind)
        | #Ast.tuple as t -> (Ast.map_tuple expr t :> Ast.expr_kind)
        | #Ast.record as r -> (Ast.map_record expr r :> Ast.expr_kind)
        | #Ast.nominal as n -> (Ast.map_nominal expr n :> Ast.expr_kind)
        | #Ast.collection as c -> (Ast.map_collection expr c :> Ast.expr_kind)
        | #Ast.comptime_call as c -> (Ast.map_comptime_call expr c :> Ast.expr_kind)
        | #Ast.method_call as m -> (Ast.map_method_call expr m :> Ast.expr_kind)
        | #Ast.reflect as r -> (Ast.map_reflect expr r :> Ast.expr_kind)
      in
      { e with Ast.it }
  (* What a captured chunk is handed is what stands in scope where it was
     written, so lowering follows the scope rather than the block's full set:
     a `code` in an initializer cannot be given the name it is initializing. *)
  and stmt scope (s : Ast.stmt) : Ast.stmt * string list =
    let sp = s.Ast.span in
    let same it = { s with Ast.it = it }, scope in
    match s.Ast.it with
    | `Gen inner ->
      let index = Hashtbl.length table in
      Hashtbl.replace table index inner;
      same (`Expr (call sp index scope emitter))
    | `Var_decl (name, ty, init) ->
      { s with Ast.it = `Var_decl (name, ty, Option.map (expr scope) init) }, name :: scope
    | `Block body -> same (`Block (block scope body))
    | `While (cond, body) -> same (`While (expr scope cond, fst (stmt scope body)))
    | `If (cond, t, e) ->
      same (`If (expr scope cond, fst (stmt scope t), Option.map (fun e -> fst (stmt scope e)) e))
    | `For_in (name, iterable, body) ->
      same (`For_in (name, expr scope iterable, fst (stmt (name :: scope) body)))
    | `For (init, cond, step, body) ->
      let init, inner =
        match init with
        | None -> None, scope
        | Some i ->
          let i, inner = stmt scope i in
          Some i, inner
      in
      same
        (`For
           ( init
           , Option.map (expr inner) cond
           , Option.map (expr inner) step
           , fst (stmt inner body) ))
    | #Ast.stmts as st -> same (Ast.map_stmts (expr scope) (fun b -> fst (stmt scope b)) st :> Ast.stmt_kind)
    (* An arm binds its payload for its own body, the way a loop binds its
       element, so what it binds is in scope for a `code` written there. *)
    | `Match (subject, arms) ->
      same
        (`Match
           ( expr scope subject
           , List.map
               (fun ((p, body) : Ast.pattern * Ast.stmt list) ->
                 let inner =
                   match p with
                   | Ast.Pat_variant (_, _, payload) ->
                     List.map snd (Ast.payload_fields payload) @ scope
                   | Ast.Pat_wild -> scope
                 in
                 p, block inner body)
               arms ))
    | _ -> s, scope
  and block scope body =
    List.rev (fst (List.fold_left (fun (out, scope) s ->
      let s, scope = stmt scope s in
      s :: out, scope) ([], scope) body))
  in
  block params body

(* Every call to a meta function runs where it stands and is replaced by what it
   produced. Its arguments are whatever the meta program can see, so one that
   names a runtime variable fails there — which is the right place, since that
   is exactly what cannot be known yet. *)
let expand ~out ~codes ~meta_fns ~named ~seen ~table ~current (root : Ast.stmt) : Ast.stmt =
  let evaluate span (call : Ast.expr) =
    let captured = ref None in
    let sink v = captured := Some v in
    let ask : Ast.stmt =
      { Ast.it = `Expr (Ast.at span (`Call (Ast.at span (`Var capturer), [ call ])))
      ; span
      ; ann = ()
      }
    in
    let emit span args =
      match args with
      | Value.Int index :: rest ->
        (match Hashtbl.find_opt table index with
         | Some captured -> !current := substitute (bindings_of rest) captured :: !(!current)
         | None -> Value.fail span "Nothing was captured here.")
      | _ -> Value.fail span "Nothing was captured here."
    in
    run ~out ~codes ~emit ~capture:sink (List.rev !seen @ List.rev !meta_fns @ [ ask ]);
    match Option.bind !captured (literal_of span) with
    | Some literal -> literal
    | None -> fail span "A meta function's result must have a literal form."
  in
  let rec expr (e : Ast.expr) : Ast.expr =
    match e.Ast.it with
    | `Call ({ Ast.it = `Var name; _ }, args) when Hashtbl.mem named name ->
      evaluate e.Ast.span { e with Ast.it = `Call (Ast.at e.Ast.span (`Var name), List.map expr args) }
    | it ->
      let it : Ast.expr_kind =
        match it with
        | `Code _ as c -> c
        | `Lambda (params, sg, body) -> `Lambda (params, sg, List.map stmt body)
        | #Ast.lit as l -> l
        | #Ast.vars as v -> (Ast.map_vars expr v :> Ast.expr_kind)
        | #Ast.ops as o -> (Ast.map_ops expr o :> Ast.expr_kind)
        | #Ast.logic as l -> (Ast.map_logic expr l :> Ast.expr_kind)
        | #Ast.compound as c -> (Ast.map_compound expr c :> Ast.expr_kind)
        | #Ast.indexing as i -> (Ast.map_indexing expr i :> Ast.expr_kind)
        | #Ast.tuple as t -> (Ast.map_tuple expr t :> Ast.expr_kind)
        | #Ast.record as r -> (Ast.map_record expr r :> Ast.expr_kind)
        | #Ast.nominal as n -> (Ast.map_nominal expr n :> Ast.expr_kind)
        | #Ast.collection as c -> (Ast.map_collection expr c :> Ast.expr_kind)
        | #Ast.comptime_call as c -> (Ast.map_comptime_call expr c :> Ast.expr_kind)
        | #Ast.method_call as m -> (Ast.map_method_call expr m :> Ast.expr_kind)
        | #Ast.reflect as r -> (Ast.map_reflect expr r :> Ast.expr_kind)
      in
      { e with Ast.it }
  and stmt (s : Ast.stmt) : Ast.stmt =
    let it : Ast.stmt_kind =
      match s.Ast.it with
      | #Ast.stmts as st -> (Ast.map_stmts expr stmt st :> Ast.stmt_kind)
      | #Ast.loops as l -> (Ast.map_loops expr stmt l :> Ast.stmt_kind)
      | #Ast.matching as m -> (Ast.map_matching expr stmt m :> Ast.stmt_kind)
      | #Ast.op_defs as o -> (Ast.map_op_defs stmt o :> Ast.stmt_kind)
      | #Ast.method_defs as m -> (Ast.map_method_defs stmt Fun.id m :> Ast.stmt_kind)
      | other -> other
    in
    { s with Ast.it }
  in
  stmt root

(* A nested block is processed while its parent is compiled, so the innermost
   runs first, and the parent's control flow has no bearing on whether it does:
   what executes is decided later, what is processed is decided here. *)
let rec strip ~out ~inside ~hoist ~table ~codes ~current (program : Ast.program) =
  let seen = ref [] in
  let meta_fns = ref [] in
  let named = Hashtbl.create 8 in
  List.concat_map
    (fun (s : Ast.stmt) ->
      (* `derive A, B for X` is `meta` running one deriver per trait, so it is
         written as that and the block below does the rest. *)
      let s =
        match s.Ast.it with
        | `Derive (traits, target) ->
          let call trait =
            let name = Ast.deriver_name trait in
            if not (Hashtbl.mem named name)
            then fail s.Ast.span "Trait '%s' has no deriver." trait;
            let shape =
              Ast.at
                s.Ast.span
                (`Field (Ast.at s.Ast.span (`Typeof (Ast.at s.Ast.span (`Var target))), "shape"))
            in
            { s with
              Ast.it = `Expr (Ast.at s.Ast.span (`Call (Ast.at s.Ast.span (`Var name), [ shape ])))
            }
          in
          { s with Ast.it = `Meta (List.map call traits) }
        | _ -> s
      in
      (* A call to a meta function may generate as well as return, wherever it
         stands, so each statement gets a collector of its own and whatever it
         produced is spliced beside it. *)
      let produced = ref [] in
      let s =
        if Hashtbl.length named = 0
        then s
        else (
          let previous = !current in
          current := produced;
          let expanded = expand ~out ~codes ~meta_fns ~named ~seen ~table ~current s in
          current := previous;
          expanded)
      in
      let beside =
        match strip ~out ~inside ~hoist ~table ~codes ~current (List.rev !produced) with
        | [] -> []
        | generated ->
          let declarations, statements = List.partition is_declaration generated in
          List.iter
            (fun d ->
              seen := d :: !seen;
              hoist := d :: !hoist)
            declarations;
          statements
      in
      beside
      @
      match s.Ast.it with
      (* Every call to it has happened by the time the program runs, so it is
         recorded and dropped rather than emitted. *)
      | `Meta_fn (name, params, signature, body) ->
        if Hashtbl.mem named name
        then (
          match Ast.deriver_trait name with
          | Some trait -> fail s.Ast.span "Trait '%s' already has a deriver." trait
          | None -> fail s.Ast.span "'%s' is already a meta function." name);
        Hashtbl.replace named name ();
        let params' = List.map (fun (p : Ast.param) -> p.Ast.name) params in
        let body = lower ~table ~codes ~params:params' (strip ~out ~inside:true ~hoist ~table ~codes ~current body) in
        meta_fns := { s with Ast.it = `Fn (name, params, signature, body) } :: !meta_fns;
        []
      | `Meta body ->
        let body = strip ~out ~inside:true ~hoist ~table ~codes ~current body in
        let collected = ref [] in
        let previous = !current in
        current := collected;
        let emit span args =
          match args with
          | Value.Int index :: rest ->
            (match Hashtbl.find_opt table index with
             | Some captured ->
               !current := substitute (bindings_of rest) captured :: !(!current)
             | None -> Value.fail span "Nothing was captured here.")
          | _ -> Value.fail span "Nothing was captured here."
        in
        run ~out ~codes ~emit ~capture:(fun _ -> ()) (List.rev !seen @ List.rev !meta_fns @ lower ~table ~codes ~params:[] body);
        current := previous;
        (* A generated declaration is hoisted, because what generated it may
           stand below the code that uses it; a generated statement stays where
           the block was. Both may hold blocks of their own. *)
        let generated = strip ~out ~inside ~hoist ~table ~codes ~current (List.rev !collected) in
        let declarations, statements = List.partition is_declaration generated in
        List.iter
          (fun d ->
            seen := d :: !seen;
            hoist := d :: !hoist)
          declarations;
        statements
      (* A meta block inside a `gen` is still a nested block, so it is processed
         now and its output takes its place — inside the `gen`, which is where
         it stood. *)
      | `Gen inner when inside ->
        (match strip ~out ~inside ~hoist ~table ~codes ~current [ inner ] with
         | [] -> []
         | [ one ] -> [ { s with Ast.it = `Gen one } ]
         | many -> [ { s with Ast.it = `Gen { s with Ast.it = `Block many } } ])
      | `Gen _ -> fail s.Ast.span "'gen' is only allowed inside a meta block."
      | _ ->
        if is_declaration s then seen := s :: !seen;
        [ s ])
    program

let program ~out (p : Ast.program) : (Ast.program, error) result =
  let hoist = ref [] in
  let table = Hashtbl.create 16 in
  let codes = Hashtbl.create 16 in
  let current = ref (ref []) in
  try
    let program = strip ~out ~inside:false ~hoist ~table ~codes ~current p in
    Ok (List.rev !hoist @ program)
  with
  | Failed e -> Error e
