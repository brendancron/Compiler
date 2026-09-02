(* Several files become one program. A unit contributes declarations; only the
   entry contributes statements, so nothing is initialized in an order and a
   cycle is harmless. Names are made unique per unit here, which is why nothing
   after this point knows that modules exist. *)

type error =
  { span : Ast.span
  ; message : string
  }

exception Failed of error

let fail span fmt =
  Printf.ksprintf (fun message -> raise (Failed { span; message })) fmt

type unit_ =
  { path : string (* normalized: what a span reports and what `visited` keys on *)
  ; namespace : string
  ; program : Ast.program
  }

let namespace_of path = Filename.remove_extension (Filename.basename path)

(* Relative to the file holding the import, and never to a root. *)
let resolve_path ~from path =
  let path = if Filename.check_suffix path ".cx" then path else path ^ ".cx" in
  if Filename.is_relative path then Filename.concat (Filename.dirname from) path else path

(* Textual, so it needs no filesystem call: two spellings of one path must not
   load it twice, and the visited set only has to agree with itself. *)
let normalize path =
  let absolute = String.length path > 0 && path.[0] = '/' in
  let parts =
    String.split_on_char '/' path
    |> List.filter (fun part -> not (String.equal part "" || String.equal part "."))
    |> List.fold_left
         (fun acc part ->
           match part, acc with
           | "..", previous :: rest when not (String.equal previous "..") -> rest
           | part, acc -> part :: acc)
         []
    |> List.rev
  in
  (if absolute then "/" else "") ^ String.concat "/" parts

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let parse_unit span path =
  if not (Sys.file_exists path) then fail span "Cannot find module '%s'." path;
  let source = read_file path in
  match Scanner.scan_tokens ~file:path source with
  | Error (e :: _) -> fail { Ast.file = path; line = e.Scanner.line; col = e.Scanner.col } "%s" e.Scanner.message
  | Error [] -> fail span "'%s' does not scan." path
  | Ok tokens ->
    (match Parser.parse tokens with
     | Error (e :: _) ->
       fail { Ast.file = path; line = e.Parser.line; col = e.Parser.col } "%s" e.Parser.message
     | Error [] -> fail span "'%s' does not parse." path
     | Ok program -> program)

(* A directory import is one import per file in it, sorted so the expansion does
   not depend on the order a filesystem hands them back. *)
let expand_wildcards ~from (program : Ast.program) =
  List.concat_map
    (fun (s : Ast.stmt) ->
      match s.Ast.it with
      | `Import (Ast.Wildcard dir) ->
        let base = Filename.concat (Filename.dirname from) dir in
        if not (Sys.file_exists base && Sys.is_directory base)
        then fail s.Ast.span "Cannot find directory '%s'." dir;
        Sys.readdir base
        |> Array.to_list
        |> List.filter (fun name -> Filename.check_suffix name ".cx")
        |> List.sort String.compare
        |> List.map (fun name ->
          { s with
            Ast.it = `Import (Ast.Qualified (dir ^ "/" ^ Filename.remove_extension name))
          })
      | _ -> [ s ])
    program

let imports (program : Ast.program) =
  List.filter_map
    (fun (s : Ast.stmt) ->
      match s.Ast.it with
      | `Import decl -> Some (decl, s.Ast.span)
      | _ -> None)
    program

let path_of = function
  | Ast.Qualified path | Ast.Aliased (path, _) | Ast.Selective (_, path) | Ast.Wildcard path ->
    path

(* Breadth-first, and a unit already seen is skipped rather than rejected: a
   cycle is two units that import each other, which is legal. *)
let load entry =
  let visited = Hashtbl.create 8 in
  let units = ref [] in
  let rec walk span path =
    let path = normalize path in
    if not (Hashtbl.mem visited path)
    then (
      Hashtbl.replace visited path ();
      let program = expand_wildcards ~from:path (parse_unit span path) in
      units := { path; namespace = namespace_of path; program } :: !units;
      List.iter
        (fun (decl, span) -> walk span (resolve_path ~from:path (path_of decl)))
        (imports program))
  in
  let root = { Ast.file = entry; line = 1; col = 1 } in
  walk root entry;
  let all = List.rev !units in
  let entry_path = normalize entry in
  match List.partition (fun u -> String.equal u.path entry_path) all with
  | [ entry_unit ], rest -> entry_unit, rest
  | _ -> fail root "Cannot find the entry module."

(* ---- resolution ---- *)

(* What a unit exports and therefore what gets a unit-qualified name. An `impl`
   exports nothing of its own: its methods are reached through the type it is
   written for, which is renamed with everything else. *)
let declared_name (s : Ast.stmt) =
  match s.Ast.it with
  | `Fn (name, _, _, _) -> Some name
  | `Type_decl (name, _, _) -> Some name
  | `Trait_decl (name, _, _) -> Some name
  | _ -> None

let is_declaration (s : Ast.stmt) =
  match s.Ast.it with
  | `Fn _ | `Type_decl _ | `Trait_decl _ | `Impl_decl _ | `Op_decl _ | `Effect_decl _
  | `Handler_decl _ | `Import _ | `Meta _ | `Gen _ | `Meta_fn _ | `Derive _ -> true
  | _ -> false

let exports unit_ = List.filter_map declared_name unit_.program

(* Only the entry runs statements, so a statement anywhere else would silently
   never happen. Saying so costs less than explaining where it went. *)
let check_declarations_only unit_ =
  List.iter
    (fun (s : Ast.stmt) ->
      if not (is_declaration s)
      then fail s.Ast.span "A module may only hold declarations.")
    unit_.program

let renamed unit_ ~entry name =
  if entry then name else Ast.generated [ unit_.namespace; name ]

(* The names a unit binds locally, so a rename never touches something that only
   happens to be spelled like a top-level function. *)
let rec bound_by (s : Ast.stmt) =
  match s.Ast.it with
  | `Var_decl (name, _, _) -> [ name ]
  | `Block body -> List.concat_map bound_by body
  | _ -> []

let rewrite ~aliases ~direct ~own ~rename ~from (program : Ast.program) =
  let module S = Set.Make (String) in
  (* Brought in by name, or this unit's own. *)
  let resolve_local name =
    match Hashtbl.find_opt direct name with
    | Some target -> target
    | None -> if Hashtbl.mem own name then rename name else name
  in
  (* A written type name is resolved the same ways a value name is, plus one
     reached through a namespace. *)
  let resolve_type name =
    match String.index_opt name '.' with
    | Some at ->
      let namespace = String.sub name 0 at
      and field = String.sub name (at + 1) (String.length name - at - 1) in
      (match Hashtbl.find_opt aliases namespace with
       | Some qualify -> qualify field
       | None -> name)
    | None -> resolve_local name
  in
  let rec type_expr (t : Ast.type_expr) : Ast.type_expr =
    let it : Ast.type_expr_kind =
      match t.Ast.it with
      | Ast.Ty_variadic t -> Ast.Ty_variadic (type_expr t)
      | Ast.Ty_name name -> Ast.Ty_name (resolve_type name)
      | Ast.Ty_assoc ({ Ast.it = Ast.Ty_name namespace; _ }, member)
        when Hashtbl.mem aliases namespace ->
        Ast.Ty_name ((Hashtbl.find aliases namespace) member)
      | Ast.Ty_assoc (owner, member) -> Ast.Ty_assoc (type_expr owner, member)
      | Ast.Ty_bind (bound, t) -> Ast.Ty_bind (bound, type_expr t)
      | Ast.Ty_app (name, args) ->
        Ast.Ty_app (resolve_type name, List.map type_expr args)
      | Ast.Ty_tuple items -> Ast.Ty_tuple (List.map type_expr items)
      | Ast.Ty_record fields ->
        Ast.Ty_record (List.map (fun (l, t) -> l, type_expr t) fields)
      | Ast.Ty_fn (params, ret, row) ->
        Ast.Ty_fn (List.map type_expr params, type_expr ret, row)
    in
    { t with Ast.it }
  in
  let param (p : Ast.param) = { p with Ast.ty = Option.map type_expr p.Ast.ty } in
  let signature (sg : Ast.signature) =
    { sg with
      Ast.ret = Option.map type_expr sg.Ast.ret
    ; comptime =
        List.map
          (fun (c : Ast.comptime_param) ->
            { c with Ast.cp_ty = Option.map type_expr c.Ast.cp_ty })
          sg.Ast.comptime
    }
  in
  let rec expr locals (e : Ast.expr) : Ast.expr =
    let go = expr locals in
    let it : Ast.expr_kind =
      match e.Ast.it with
      | `Lambda (params, signature, body) ->
        let inner =
          List.fold_left (fun acc (p : Ast.param) -> S.add p.Ast.name acc) locals params
        in
        `Lambda (params, signature, List.map (stmt inner) body)
      (* Read here because this is what knows where the source it was written
         in lives. What the program keeps is the text, so nothing reads
         anything later. *)
      | `Call ({ Ast.it = `Var "embed"; _ }, [ { Ast.it = `Str path; _ } ]) ->
        let path = Utf8.encode path in
        let full =
          if Filename.is_relative path
          then Filename.concat (Filename.dirname from) path
          else path
        in
        (match In_channel.with_open_bin full In_channel.input_all with
         | contents -> `Bytes contents
         | exception Sys_error _ -> fail e.Ast.span "Cannot embed '%s'." path)
      | `Code inner -> `Code (go inner)
      | `Var name when not (S.mem name locals) -> `Var (resolve_local name)
      (* `math.add(…)` parses as a method call, and is one unless `math` names a
         module and nothing in scope has taken the name. *)
      | `Method_call ({ Ast.it = `Var receiver; _ }, name, _, args)
        when (not (S.mem receiver locals)) && Hashtbl.mem aliases receiver ->
        `Call
          ( { e with Ast.it = `Var (Hashtbl.find aliases receiver name) }
          , List.map go args )
      (* Whether this reaches a method or a function is not known here, so the
         name it would have as a function is resolved the same way a reference
         to it would be, and carried along for whoever can tell. *)
      | `Method_call (receiver, name, _, args) ->
        let as_function = if S.mem name locals then name else resolve_local name in
        `Method_call (go receiver, name, as_function, List.map go args)
      | `New (name, fields) ->
        `New (resolve_type name, List.map (fun (l, v) -> l, go v) fields)
      | `New_variant (ty, variant, payload) ->
        `New_variant (resolve_type ty, variant, Ast.map_payload go payload)
      | `New_call (name, args, values) ->
        `New_call (resolve_type name, List.map type_expr args, List.map go values)
      | #Ast.lit as l -> l
      | #Ast.vars as v -> (Ast.map_vars go v :> Ast.expr_kind)
      | #Ast.ops as o -> (Ast.map_ops go o :> Ast.expr_kind)
      | #Ast.logic as l -> (Ast.map_logic go l :> Ast.expr_kind)
      | #Ast.compound as c -> (Ast.map_compound go c :> Ast.expr_kind)
      | #Ast.indexing as i -> (Ast.map_indexing go i :> Ast.expr_kind)
      | #Ast.tuple as t -> (Ast.map_tuple go t :> Ast.expr_kind)
      | #Ast.record as r -> (Ast.map_record go r :> Ast.expr_kind)
      | #Ast.collection as c -> (Ast.map_collection go c :> Ast.expr_kind)
      | #Ast.comptime_call as c -> (Ast.map_comptime_call go c :> Ast.expr_kind)
      | #Ast.reflect as r -> (Ast.map_reflect go r :> Ast.expr_kind)
    in
    { e with Ast.it }
  and stmt locals (s : Ast.stmt) : Ast.stmt =
    let it : Ast.stmt_kind =
      match s.Ast.it with
      | `Type_decl (name, params, body) ->
        let body =
          match body with
          | Ast.T_fields fields ->
            Ast.T_fields (List.map (fun (l, t) -> l, type_expr t) fields)
          | Ast.T_variants variants ->
            Ast.T_variants
              (List.map
                 (fun (v : Ast.variant) ->
                   { v with
                     Ast.v_payload = Ast.map_payload type_expr v.Ast.v_payload
                   ; v_result = Option.map type_expr v.Ast.v_result
                   })
                 variants)
        in
        `Type_decl (resolve_type name, params, body)
      | `Trait_decl (name, params, body) ->
        `Trait_decl
          ( resolve_type name
          , params
          , { body with
              Ast.tb_super =
                List.map
                  (fun (super, args) -> resolve_type super, List.map type_expr args)
                  body.Ast.tb_super
            ; tb_methods =
                List.map
                  (fun (m : Ast.method_sig) ->
                    { m with
                      Ast.ms_params = List.map param m.Ast.ms_params
                    ; ms_signature = signature m.Ast.ms_signature
                    })
                  body.Ast.tb_methods
            } )
      | `Derive (traits, target) ->
        `Derive (List.map resolve_type traits, resolve_type target)
      | `Impl_decl (trait, type_name, params, impl) ->
        `Impl_decl
          ( Option.map (fun (t, args) -> resolve_type t, List.map type_expr args) trait
          , resolve_type type_name
          , params
          , { Ast.ib_assoc = List.map (fun (n, t) -> n, type_expr t) impl.Ast.ib_assoc
            ; ib_methods =
                List.map
                  (fun (m : (Ast.stmt, unit) Ast.method_def) ->
                    { m with
                      Ast.md_params = List.map param m.Ast.md_params
                    ; md_signature = signature m.Ast.md_signature
                    ; md_body = List.map (stmt locals) m.Ast.md_body
                    })
                  impl.Ast.ib_methods
            } )
      | `Op_decl (op, params, sg, body) ->
        `Op_decl (op, List.map param params, signature sg, List.map (stmt locals) body)
      | `Fn (name, params, sg, body) ->
        let inner =
          List.fold_left
            (fun acc (p : Ast.param) -> S.add p.Ast.name acc)
            locals
            params
        in
        let inner = List.fold_left (fun acc s -> S.union acc (S.of_list (bound_by s))) inner body in
        `Fn
          ( (if Hashtbl.mem own name then rename name else name)
          , List.map param params
          , signature sg
          , List.map (stmt inner) body )
      | `Block body ->
        let inner = List.fold_left (fun acc s -> S.union acc (S.of_list (bound_by s))) locals body in
        `Block (List.map (stmt inner) body)
      | `Var_decl (name, ty, init) ->
        `Var_decl (name, Option.map type_expr ty, Option.map (expr locals) init)
      | `Import _ -> `Block []
      | `Meta body -> `Meta (List.map (stmt locals) body)
      | `Gen inner -> `Gen (stmt locals inner)
      | `Meta_fn (name, params, sg, body) ->
        `Meta_fn (name, List.map param params, signature sg, List.map (stmt locals) body)
      | #Ast.stmts as st -> (Ast.map_stmts (expr locals) (stmt locals) st :> Ast.stmt_kind)
      (* A loop variable binds for the body alone, so it is added here rather
         than through [bound_by], which answers for a statement's own list. *)
      | `For_in (name, over, body) ->
        `For_in (name, expr locals over, stmt (S.add name locals) body)
      | `For (init, cond, step, body) ->
        let inner =
          match init with
          | Some s -> S.union locals (S.of_list (bound_by s))
          | None -> locals
        in
        `For
          ( Option.map (stmt locals) init
          , Option.map (expr inner) cond
          , Option.map (expr inner) step
          , stmt inner body )
      | #Ast.effects as e ->
        let clause (c : Ast.stmt Ast.handler_clause) =
          match c with
          | Ast.Inline h -> Ast.Inline (Ast.map_handler (stmt locals) h)
          | Ast.Named name -> Ast.Named name
        in
        (Ast.map_effects (expr locals) (stmt locals) clause e :> Ast.stmt_kind)
      | `Handler_decl (name, h) -> `Handler_decl (name, Ast.map_handler (stmt locals) h)
      (* A pattern names its sum type, so it is renamed like any other use. *)
      | `Match (scrutinee, cases) ->
        `Match
          ( expr locals scrutinee
          , List.map
              (fun (p, body) ->
                let payload =
                  match p with
                  | Ast.Pat_variant (_, _, payload) -> payload
                  | Ast.Pat_wild -> Ast.P_none
                in
                let p =
                  match p with
                  | Ast.Pat_variant (ty, variant, payload) ->
                    Ast.Pat_variant (resolve_type ty, variant, payload)
                  | Ast.Pat_wild -> Ast.Pat_wild
                in
                let inner =
                  List.fold_left
                    (fun acc (_, binding) -> S.add binding acc)
                    locals
                    (Ast.payload_fields payload)
                in
                p, List.map (stmt inner) body)
              cases )
    in
    { s with Ast.it }
  in
  List.map (stmt S.empty) program

(* Every unit's declarations, then the entry's statements. *)
let program entry_path =
  let entry_unit, rest = load entry_path in
  let table = Hashtbl.create 8 in
  List.iter check_declarations_only rest;
  List.iter
    (fun u -> Hashtbl.replace table u.namespace (u, exports u))
    (entry_unit :: rest);
  let resolve_unit u ~entry =
    let own = Hashtbl.create 8 in
    List.iter (fun name -> Hashtbl.replace own name ()) (exports u);
    let aliases = Hashtbl.create 4 in
    let direct = Hashtbl.create 4 in
    List.iter
      (fun (decl, span) ->
        let target = namespace_of (path_of decl) in
        match Hashtbl.find_opt table target with
        | None -> fail span "Module '%s' was not loaded." target
        | Some (target_unit, _) ->
          let is_entry = String.equal target_unit.path entry_unit.path in
          let bind under =
            if Hashtbl.mem aliases under
            then fail span "'%s' is already bound. Import one of them with `as`." under;
            Hashtbl.replace aliases under (fun name -> renamed target_unit ~entry:is_entry name)
          in
          (match decl with
           | Ast.Qualified _ -> bind target
           | Ast.Aliased (_, alias) -> bind alias
           | Ast.Selective (names, _) ->
             List.iter
               (fun name ->
                 if not (List.mem name (snd (Hashtbl.find table target)))
                 then fail span "Module '%s' does not export '%s'." target name;
                 if Hashtbl.mem direct name
                 then fail span "'%s' is already imported." name;
                 Hashtbl.replace direct name (renamed target_unit ~entry:is_entry name))
               names
           | Ast.Wildcard _ -> ()))
      (imports u.program);
    rewrite
      ~aliases
      ~direct
      ~own
      ~rename:(fun name -> renamed u ~entry name)
      ~from:u.path
      u.program
  in
  let declarations_of u ~entry =
    resolve_unit u ~entry |> List.filter (fun s -> is_declaration s || entry)
  in
  List.concat_map (fun u -> declarations_of u ~entry:false) rest
  @ declarations_of entry_unit ~entry:true
