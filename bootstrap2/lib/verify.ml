(* Checks that a tree's type annotations agree with its structure.

   Passes that only copy annotations cannot break this. Passes that construct
   nodes invent annotations, and a wrong one is silent — a synthesized call
   whose callee is not typed as a function still runs, and a synthesized call
   carrying the wrong effect row makes CPS skip a conversion it needed to make.

   This is a local check: it compares each node against its children and does
   not carry an environment, so a `Var` is taken at its word. *)

type error =
  { span : Ast.span
  ; message : string
  }

exception Failed of error

let fail span fmt =
  Printf.ksprintf (fun message -> raise (Failed { span; message })) fmt

let expect span what expected actual =
  if expected <> actual
  then
    fail
      span
      "%s should be %s but is annotated %s."
      what
      (Types.string_of_ty expected)
      (Types.string_of_ty actual)

let rec expr (e : Ast.cps_expr) : unit =
  let span = e.Ast.span
  and ann = e.Ast.ann in
  match e.Ast.it with
  | `Int _ -> expect span "An int literal" Types.Int ann
  | `Float _ -> expect span "A float literal" Types.Float ann
  | `Str _ -> expect span "A string literal" Types.Str ann
  | `Bool _ -> expect span "A bool literal" Types.Bool ann
  | `Var _ -> ()
  | `Assign (_, v) ->
    expr v;
    expect span "An assignment" v.Ast.ann ann
  | `Unop (Ast.Neg, a) ->
    expr a;
    expect span "A negation" a.Ast.ann ann
  | `Unop (Ast.Not, a) ->
    expr a;
    expect a.Ast.span "The operand of '!'" Types.Bool a.Ast.ann;
    expect span "A negation" Types.Bool ann
  | `Binop (op, a, b) ->
    expr a;
    expr b;
    expect b.Ast.span "Both operands" a.Ast.ann b.Ast.ann;
    (match op with
     | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div ->
       expect span "An arithmetic result" a.Ast.ann ann
     | Ast.Equal | Ast.Not_equal | Ast.Less | Ast.Less_equal | Ast.Greater
     | Ast.Greater_equal -> expect span "A comparison" Types.Bool ann)
  | `And (a, b) | `Or (a, b) ->
    expr a;
    expr b;
    expect a.Ast.span "The operand of a logical operator" Types.Bool a.Ast.ann;
    expect b.Ast.span "The operand of a logical operator" Types.Bool b.Ast.ann;
    expect span "A logical operator" Types.Bool ann
  | `Array_lit items ->
    List.iter expr items;
    (match ann with
     | Types.Array elem ->
       List.iter
         (fun (i : Ast.cps_expr) -> expect i.Ast.span "An array element" elem i.Ast.ann)
         items
     | other ->
       fail span "An array literal is annotated %s." (Types.string_of_ty other))
  | `Tuple items ->
    List.iter expr items;
    expect
      span
      "A tuple"
      (Types.Tuple (List.map (fun (i : Ast.cps_expr) -> i.Ast.ann) items))
      ann
  (* A nominal type is a record at run time, so a literal may carry either. *)
  | `Record_lit fields ->
    List.iter (fun (_, v) -> expr v) fields;
    let actual =
      List.sort compare (List.map (fun (l, (v : Ast.cps_expr)) -> l, v.Ast.ann) fields)
    in
    (match ann with
     | Types.Record declared | Types.Named (_, _, declared) ->
       if declared <> actual
       then
         fail
           span
           "A record literal is annotated %s but its fields are %s."
           (Types.string_of_ty ann)
           (Types.string_of_ty (Types.Record actual))
     | other ->
       fail span "A record literal is annotated %s." (Types.string_of_ty other))
  | `Field (target, label) ->
    expr target;
    (match target.Ast.ann with
     | Types.Record fields | Types.Named (_, _, fields) ->
       (match List.assoc_opt label fields with
        | Some ty -> expect span "A field" ty ann
        | None -> fail span "A record has no field '%s'." label)
     | other ->
       fail target.Ast.span "Taking a field of %s." (Types.string_of_ty other))
  | `Field_assign (target, label, v) ->
    expr target;
    expr v;
    (match target.Ast.ann with
     | Types.Record fields | Types.Named (_, _, fields) ->
       (match List.assoc_opt label fields with
        | Some ty ->
          expect v.Ast.span "An assigned field" ty v.Ast.ann;
          expect span "A field assignment" ty ann
        | None -> fail span "A record has no field '%s'." label)
     | other ->
       fail target.Ast.span "Taking a field of %s." (Types.string_of_ty other))
  | `Tuple_get (target, index) ->
    expr target;
    (match target.Ast.ann with
     | Types.Tuple items ->
       (match List.nth_opt items index with
        | Some ty -> expect span "A tuple field" ty ann
        | None -> fail span "A tuple has no field %d." index)
     | other ->
       fail target.Ast.span "Taking a field of %s." (Types.string_of_ty other))
  | `Index (target, i) ->
    expr target;
    expr i;
    expect i.Ast.span "An index" Types.Int i.Ast.ann;
    (match target.Ast.ann with
     | Types.Array elem -> expect span "An indexing result" elem ann
     | other ->
       fail target.Ast.span "Indexing a %s." (Types.string_of_ty other))
  | `Index_assign (target, i, v) ->
    expr target;
    expr i;
    expr v;
    expect i.Ast.span "An index" Types.Int i.Ast.ann;
    (match target.Ast.ann with
     | Types.Array elem ->
       expect v.Ast.span "An assigned element" elem v.Ast.ann;
       expect span "An index assignment" elem ann
     | other ->
       fail target.Ast.span "Indexing a %s." (Types.string_of_ty other))
  (* The payload's types are the declaration's business; only the tag survives
     here, so there is nothing local to check beyond the parts. *)
  | `Variant (_, fields) ->
    List.iter (fun (_, v) -> expr v) fields;
    (match ann with
     | Types.Sum _ -> ()
     | other -> fail span "A variant is annotated %s." (Types.string_of_ty other))
  | `Call (callee, args) ->
    expr callee;
    List.iter expr args;
    (match callee.Ast.ann with
     | Types.Fn (params, ret, _) ->
       if List.length params <> List.length args
       then
         fail
           span
           "A call passes %d argument(s) to a function of %d."
           (List.length args)
           (List.length params);
       List.iter2
         (fun param (a : Ast.cps_expr) ->
           expect a.Ast.span "An argument" param a.Ast.ann)
         params
         args;
       expect span "A call" ret ann
     (* A generic callee has not been monomorphized; nothing to check yet. *)
     | Types.Generic _ -> ()
     | other ->
       fail
         callee.Ast.span
         "A callee is annotated %s, which is not a function."
         (Types.string_of_ty other))

let rec stmt (s : Ast.cps_stmt) : unit =
  match s.Ast.it with
  | `Expr e -> expr e
  | `Var_decl (_, _, init) -> Option.iter expr init
  | `Block body -> List.iter stmt body
  | `If (cond, then_branch, else_branch) ->
    expr cond;
    expect cond.Ast.span "A condition" Types.Bool cond.Ast.ann;
    stmt then_branch;
    Option.iter stmt else_branch
  | `While (cond, body) ->
    expr cond;
    expect cond.Ast.span "A condition" Types.Bool cond.Ast.ann;
    stmt body
  | `Fn (_, _, _, body) -> List.iter stmt body
  | `Return e -> Option.iter expr e
  | `Match (scrutinee, cases) ->
    expr scrutinee;
    List.iter (fun (_, body) -> List.iter stmt body) cases

let program (p : Ast.cps_stmt list) : (unit, error) result =
  try
    List.iter stmt p;
    Ok ()
  with
  | Failed e -> Error e
