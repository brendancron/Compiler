open Ast

type error =
  { span : span
  ; message : string
  }

exception Error of error

let declared : (string, stmt handler) Hashtbl.t = Hashtbl.create 8

let counter = ref 0

let fresh prefix =
  incr counter;
  generated [ prefix; string_of_int !counter ]

let rec expr (e : expr) : desugared_expr =
  let sp = e.span in
  let it : desugared_expr_kind =
    match e.it with
    | #lit as l -> l
    | #vars as v -> (map_vars expr v :> desugared_expr_kind)
    | #ops as o -> (map_ops expr o :> desugared_expr_kind)
    | #logic as l -> (map_logic expr l :> desugared_expr_kind)
    | #compound as c -> (map_compound expr c :> desugared_expr_kind)
    | #indexing as i -> (map_indexing expr i :> desugared_expr_kind)
    | #tuple as t -> (map_tuple expr t :> desugared_expr_kind)
    | #record as r -> (map_record expr r :> desugared_expr_kind)
    | #nominal as n -> (map_nominal expr n :> desugared_expr_kind)
    | #collection as c -> (map_collection expr c :> desugared_expr_kind)
    | #comptime_call as c -> (map_comptime_call expr c :> desugared_expr_kind)
    | #method_call as m -> (map_method_call expr m :> desugared_expr_kind)
    | `Lambda (params, signature, body) -> `Lambda (params, signature, List.map stmt body)
    | #reflect as r -> (map_reflect expr r :> desugared_expr_kind)
    (* Metaprocessing lowers every one it reaches, so one that arrives here
       stood where no meta program would ever have run it. *)
    | `Code _ ->
      raise
        (Error
           { span = sp; message = "'code' is only allowed inside a meta block." })
  in
  { it; span = sp; ann = () }

and stmt (s : stmt) : desugared_stmt =
  let sp = s.span in
  let it : desugared_stmt_kind =
    match s.it with
    (* The loader strips imports and metaprocessing strips meta blocks, both
       before anything else runs. *)
    | `Import _ | `Meta _ | `Gen _ | `Meta_fn _ | `Derive _ -> assert false
    (* for (x in xs) body  ⇒  { var seq = xs; var i = 0;
                                 while (i < seq.len()) { var x = seq[i]; body; i = i + 1; } }

       The sequence is bound once so an iterable that is an expression is
       evaluated once, and `len` and `[]` are written as ordinary source so the
       checker resolves them for whatever type the sequence turns out to be. *)
    | `For_in (name, iterable, body) ->
      let sp = iterable.span in
      let seq = fresh "seq"
      and index = fresh "i" in
      let var_decl_of at_span n init : stmt =
        { it = `Var_decl (n, None, Some init); span = at_span; ann = () }
      in
      let read n = at sp (`Var n) in
      let step : stmt =
        { it = `Expr (at body.span (`Assign (index, at body.span (`Binop (Add, read index, at body.span (`Int 1))))))
        ; span = body.span
        ; ann = ()
        }
      in
      let inner : stmt =
        { it =
            `Block
              [ var_decl_of body.span name (at body.span (`Index (read seq, read index)))
              ; body
              ; step
              ]
        ; span = body.span
        ; ann = ()
        }
      in
      (stmt
         { it =
             `Block
               [ var_decl_of sp seq iterable
               ; var_decl_of sp index (at sp (`Int 0))
               ; at sp (`While (at sp (`Binop (Less, read index, at sp (`Method_call (read seq, "len", "len", [])))), inner))
               ]
         ; span = sp
         ; ann = ()
         })
        .it
    (* for (init; cond; step) body  ⇒  { init; while (cond) { body; step; } } *)
    | `For (init, cond, step, body) ->
      let cond =
        match cond with
        | None -> at sp (`Bool true)
        | Some c -> expr c
      in
      let body =
        let body = stmt body in
        match step with
        | None -> body
        | Some st ->
          let step = expr st in
          (* Synthesized nodes take the span of the source they stand in for, so
             an error in the step is not reported at the `for`. *)
          { it = `Block [ body; { it = `Expr step; span = step.span; ann = () } ]
          ; span = body.span
          ; ann = ()
          }
      in
      let loop = at sp (`While (cond, body)) in
      `Block
        (match init with
         | Some i -> [ stmt i; loop ]
         | None -> [ loop ])
    | #stmts as s -> (map_stmts expr stmt s :> desugared_stmt_kind)
    | #effects as e -> (map_effects expr stmt (clause sp) e :> desugared_stmt_kind)
    | #type_defs as t -> t
    | #matching as m -> (map_matching expr stmt m :> desugared_stmt_kind)
    | #op_defs as o -> (map_op_defs stmt o :> desugared_stmt_kind)
    | #method_defs as m -> (map_method_defs stmt Fun.id m :> desugared_stmt_kind)
    | `Handler_decl _ -> `Block []
  in
  { it; span = sp; ann = () }

and clause span (c : stmt handler_clause) : desugared_stmt handler =
  match c with
  | Inline h -> map_handler stmt h
  | Named name ->
    (match Hashtbl.find_opt declared name with
     | Some h -> map_handler stmt h
     | None ->
       raise (Error { span; message = Printf.sprintf "Unknown handler '%s'." name }))

let program (p : program) : (desugared_stmt list, error) result =
  Hashtbl.reset declared;
  List.iter
    (fun (s : stmt) ->
      match s.it with
      | `Handler_decl (name, h) -> Hashtbl.replace declared name h
      | _ -> ())
    p;
  try Ok (List.map stmt p) with
  | Error e -> Result.Error e
