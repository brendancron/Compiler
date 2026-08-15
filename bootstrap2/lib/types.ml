
type fields = (string * ty) list

(* An effect and what it was instantiated at. `Yield<int>` and `Yield<string>`
   are different entries, which is what lets one program hold both. *)
and row = (string * ty list) list

and ty =
  | Int
  | Float
  | Str
  | Byte
  | Chr
  | Bool
  | Unit
  | Tuple of ty list
  | Record of fields
  | Named of string * ty list * fields
  | Sum of string * ty list
  | Fn of ty list * ty * row
  (* Quantified, not unresolved. Reaching codegen means a call site was missed. *)
  | Generic of int

type infer_ty =
  | IInt
  | IFloat
  | IStr
  | IByte
  | IChr
  | IBool
  | IUnit
  | ITuple of infer_ty list
  | IRecord of infer_fields
  | INamed of string * infer_ty list * infer_fields
  | ISum of string * infer_ty list
  | IFn of infer_ty list * infer_ty * infer_row
  | IVar of tv ref

and kind =
  | Any
  | Addable (* int, float, str — the operand type of `+` *)
  | Numeric (* int, float *)
  | Collection of infer_ty
  (* Trait names a written `<T: Summary>` demands. A method call resolves
     through the trait's declared signature, so the owner need not be known. *)
  (* A trait and what it was named at: `TryFrom<string>` rather than `TryFrom`. *)
  | Bound of (string * infer_ty list) list

and tv =
  | Unbound of int * kind
  | Link of infer_ty

(* Labels ending in [REmpty] (closed) or a variable (open). Rewriting an open
   tail is what lets a pure function pass where an effectful one is expected. *)
and infer_row =
  | REmpty
  | RCons of string * infer_ty list * infer_row
  | RVar of rv ref

and rv =
  | RUnbound of int
  | RLink of infer_row

and infer_fields =
  | FEmpty
  | FCons of string * infer_ty * infer_fields
  | FVar of fv ref

and fv =
  | FUnbound of int
  | FLink of infer_fields

type scheme =
  { quantified : int list
  ; quantified_rows : int list
  ; quantified_fields : int list
  ; body : infer_ty
  }

exception Type_error of string

let error fmt = Printf.ksprintf (fun message -> raise (Type_error message)) fmt

let counter = ref 0

let fresh_with kind =
  incr counter;
  IVar (ref (Unbound (!counter, kind)))

let fresh () = fresh_with Any

let fresh_row () =
  incr counter;
  RVar (ref (RUnbound !counter))

let fresh_fields () =
  incr counter;
  FVar (ref (FUnbound !counter))

(* Variables introduced by a written `<T>`. Never defaulted: the author said the
   function is generic in T, so resolving it to int would contradict them. *)
let declared_params : (int, unit) Hashtbl.t = Hashtbl.create 8

let reset () =
  counter := 0;
  Hashtbl.reset declared_params

let rec repr (t : infer_ty) : infer_ty =
  match t with
  | IVar ({ contents = Link inner } as r) ->
    let target = repr inner in
    r := Link target;
    target
  | t -> t

let rec repr_row (r : infer_row) : infer_row =
  match r with
  | RVar ({ contents = RLink inner } as v) ->
    let target = repr_row inner in
    v := RLink target;
    target
  | r -> r

let rec repr_fields (f : infer_fields) : infer_fields =
  match f with
  | FVar ({ contents = FLink inner } as v) ->
    let target = repr_fields inner in
    v := FLink target;
    target
  | f -> f

(* A primitive like [Int], written with an argument. Every other container is
   built from one, so a literal that narrowed to nothing else is one. *)
let array_name = "Array"

(* What `typeof` answers with. Its fields are the reflection a program can ask
   for; it is not a runtime value, so nothing constructs one — [Reflect] folds
   each projection to the data it names. *)
let reflection_name = "Type"
let shape_name = "TypeShape"
let field_name = "TypeField"
let variant_name = "TypeVariant"

let reflection_fields = [ "name", Str; "shape", Sum (shape_name, []) ]

let ireflected =
  INamed
    ( reflection_name
    , []
    , FCons ("name", IStr, FCons ("shape", ISum (shape_name, []), FEmpty)) )

let reflected = Named (reflection_name, [], reflection_fields)

(* A captured chunk of program. Like a type, it exists only while compiling. *)
let code_name = "Code"
let icode = INamed (code_name, [], FEmpty)

(* An identifier generated code can be given. Compile-time only, like the
   others: a program that still holds one when it runs has kept a promise the
   compiler made to itself. *)
let name_name = "Name"
let iname = INamed (name_name, [], FEmpty)
let name = Named (name_name, [], [])
let string_name = "string"

(* Reading a length is the one array operation with a method's syntax. *)
let array_len = "len"
let array elem = Named (array_name, [ elem ], [])
let iarray elem = INamed (array_name, [ elem ], FEmpty)

let is_array (t : ty) =
  match t with
  | Named (name, [ _ ], _) -> String.equal name array_name
  | _ -> false

let string_of_kind = function
  | Any -> "any"
  | Collection _ -> "a collection"
  | Bound traits -> String.concat " and " (List.map fst traits)
  | Addable -> "int, float or string"
  | Numeric -> "int or float"

let rec labels_of_infer_row (r : infer_row) : (string * infer_ty list) list * bool =
  match repr_row r with
  | REmpty -> [], false
  | RVar _ -> [], true
  | RCons (label, args, rest) ->
    let labels, open_ = labels_of_infer_row rest in
    (label, args) :: labels, open_

let entry render (label, args) =
  match args with
  | [] -> label
  | args -> Printf.sprintf "%s<%s>" label (String.concat ", " (List.map render args))

let rec string_of_infer_args (args : infer_ty list) =
  match args with
  | [] -> ""
  | args -> Printf.sprintf "<%s>" (String.concat ", " (List.map string_of_infer_ty args))

and string_of_infer_ty (t : infer_ty) : string =
  match repr t with
  | IInt -> "int"
  | IFloat -> "float"
  | IStr -> "string"
  | IByte -> "byte"
  | IChr -> "char"
  | IBool -> "bool"
  | IUnit -> "unit"
  | ITuple items ->
    Printf.sprintf "(%s)" (String.concat ", " (List.map string_of_infer_ty items))
  | INamed (name, args, _) | ISum (name, args) ->
    name ^ string_of_infer_args args
  | IRecord f ->
    let rec fields f =
      match repr_fields f with
      | FEmpty -> []
      | FVar _ -> [ "..." ]
      | FCons (label, ty, rest) ->
        Printf.sprintf "%s: %s" label (string_of_infer_ty ty) :: fields rest
    in
    Printf.sprintf "{ %s }" (String.concat ", " (List.sort compare (fields f)))
  | IFn (params, ret, row) ->
    Printf.sprintf
      "(%s) ->%s %s"
      (String.concat ", " (List.map string_of_infer_ty params))
      (string_of_infer_row row)
      (string_of_infer_ty ret)
  | IVar { contents = Unbound (id, _) } -> Printf.sprintf "'%d" id
  | IVar { contents = Link _ } -> assert false (* repr collapsed these *)

and string_of_infer_row (r : infer_row) =
  match labels_of_infer_row r with
  | [], false -> ""
  | labels, open_ ->
    Printf.sprintf
      " <%s%s>"
      (String.concat ", " (List.map (entry string_of_infer_ty) labels))
      (if open_ then "|_" else "")

let rec string_of_args (args : ty list) =
  match args with
  | [] -> ""
  | args -> Printf.sprintf "<%s>" (String.concat ", " (List.map string_of_ty args))

and string_of_row (r : row) =
  match r with
  | [] -> ""
  | labels ->
    Printf.sprintf " <%s>" (String.concat ", " (List.map (entry string_of_ty) labels))

and string_of_ty (t : ty) : string =
  match t with
  | Int -> "int"
  | Float -> "float"
  | Str -> "string"
  | Byte -> "byte"
  | Chr -> "char"
  | Bool -> "bool"
  | Unit -> "unit"
  | Tuple items ->
    Printf.sprintf "(%s)" (String.concat ", " (List.map string_of_ty items))
  | Named (name, args, _) | Sum (name, args) -> name ^ string_of_args args
  | Record fields ->
    Printf.sprintf
      "{ %s }"
      (String.concat
         ", "
         (List.map (fun (l, t) -> Printf.sprintf "%s: %s" l (string_of_ty t)) fields))
  | Fn (params, ret, row) ->
    Printf.sprintf
      "(%s) ->%s %s"
      (String.concat ", " (List.map string_of_ty params))
      (string_of_row row)
      (string_of_ty ret)
  | Generic id -> Printf.sprintf "'%d" id

let type_name (t : ty) : string option =
  match t with
  | Int -> Some "int"
  | Float -> Some "float"
  | Str -> Some "string"
  | Byte -> Some "byte"
  | Chr -> Some "char"
  | Bool -> Some "bool"
  | Unit -> Some "unit"
  | Named (name, _, _) | Sum (name, _) -> Some name
  | Tuple _ | Record _ | Fn _ | Generic _ -> None

let rec subst_generic mapping (t : ty) : ty =
  match t with
  | Generic id ->
    (match List.assoc_opt id mapping with
     | Some replacement -> replacement
     | None -> t)
  | Tuple items -> Tuple (List.map (subst_generic mapping) items)
  | Record fields -> Record (List.map (fun (l, t) -> l, subst_generic mapping t) fields)
  | Named (name, args, fields) ->
    Named
      ( name
      , List.map (subst_generic mapping) args
      , List.map (fun (l, t) -> l, subst_generic mapping t) fields )
  | Sum (name, args) -> Sum (name, List.map (subst_generic mapping) args)
  | Fn (params, ret, row) ->
    Fn (List.map (subst_generic mapping) params, subst_generic mapping ret, row)
  | scalar -> scalar

let rec match_generic_fields a b acc =
  List.fold_left
    (fun acc (label, ty) ->
      match List.assoc_opt label b with
      | Some other -> match_generic ty other acc
      | None -> acc)
    acc
    a

and match_generic (general : ty) (concrete : ty) acc =
  match general, concrete with
  | Generic id, _ -> if List.mem_assoc id acc then acc else (id, concrete) :: acc
  | Tuple a, Tuple b when List.length a = List.length b ->
    List.fold_left2 (fun acc a b -> match_generic a b acc) acc a b
  | Record a, Record b -> match_generic_fields a b acc
  (* Arguments as well as fields, for the same reason [has_generic] needs them:
     an opaque container carries its parameter nowhere else. *)
  | Named (_, ga, a), Named (_, gb, b) when List.length ga = List.length gb ->
    match_generic_fields a b (List.fold_left2 (fun acc x y -> match_generic x y acc) acc ga gb)
  | Named (_, _, a), Named (_, _, b) -> match_generic_fields a b acc
  | Sum (_, a), Sum (_, b) when List.length a = List.length b ->
    List.fold_left2 (fun acc a b -> match_generic a b acc) acc a b
  | Fn (pa, ra, _), Fn (pb, rb, _) when List.length pa = List.length pb ->
    match_generic ra rb (List.fold_left2 (fun acc a b -> match_generic a b acc) acc pa pb)
  | _ -> acc

let rec has_generic (t : ty) =
  match t with
  | Generic _ -> true
  | Tuple items -> List.exists has_generic items
  | Record fields -> List.exists (fun (_, t) -> has_generic t) fields
  (* Arguments as well as fields: an opaque container has no fields, so
     `Array<T>` would otherwise report as concrete. *)
  | Named (_, args, fields) ->
    List.exists has_generic args || List.exists (fun (_, t) -> has_generic t) fields
  | Sum (_, args) -> List.exists has_generic args
  | Fn (params, ret, _) -> List.exists has_generic params || has_generic ret
  | _ -> false

let container_element (t : infer_ty) : (string * infer_ty) option =
  match repr t with
  | INamed (name, [ elem ], _) -> Some (name, elem)
  | _ -> None

let infer_type_name (t : infer_ty) : string option =
  match repr t with
  | IInt -> Some "int"
  | IFloat -> Some "float"
  | IStr -> Some "string"
  | IByte -> Some "byte"
  | IChr -> Some "char"
  | IBool -> Some "bool"
  | IUnit -> Some "unit"
  | INamed (name, _, _) | ISum (name, _) -> Some name
  | ITuple _ | IRecord _ | IFn _ | IVar _ -> None

(* ---- row unification ---- *)

let rec row_occurs id (r : infer_row) =
  match repr_row r with
  | REmpty -> false
  | RVar { contents = RUnbound id' } -> id = id'
  | RVar { contents = RLink _ } -> assert false
  | RCons (_, _, rest) -> row_occurs id rest

(* A variable used by both `+` and `-` must end up Numeric, not Addable. *)
let extra_admits : (kind -> infer_ty -> bool) ref = ref (fun _ _ -> false)

(* Finding the label is also agreeing on what it was instantiated at, so the
   arguments are unified here rather than compared. *)
let rec rewrite_row label args (r : infer_row) : infer_row =
  match repr_row r with
  | RCons (l, found, rest) when String.equal l label ->
    (try List.iter2 unify args found with
     | Invalid_argument _ ->
       error "Effect '%s' is used with %d argument(s) and %d here." l
         (List.length found) (List.length args));
    rest
  | RCons (l, found, rest) -> RCons (l, found, rewrite_row label args rest)
  | RVar ({ contents = RUnbound _ } as v) ->
    let tail = fresh_row () in
    v := RLink (RCons (label, args, tail));
    tail
  | RVar { contents = RLink _ } -> assert false
  | REmpty -> error "This code does not handle the effect '%s'." label

(* A call may perform only effects its caller admits. Containment rather than
   equality: forcing the caller's row onto the callee would give the callee's
   annotation effects it does not perform, and the CPS pass reads that
   annotation to decide how many evidence arguments to pass. An open caller row
   grows to admit what it calls; a closed one rejects it. *)
and row_within (inner : infer_row) (outer : infer_row) : unit =
  match repr_row inner with
  | REmpty -> ()
  (* Nothing is known about the callee's row yet, so nothing is required. *)
  | RVar _ -> ()
  | RCons (label, args, rest) ->
    ignore (rewrite_row label args outer);
    row_within rest outer

and unify_row (a : infer_row) (b : infer_row) : unit =
  match repr_row a, repr_row b with
  | REmpty, REmpty -> ()
  | RVar v1, RVar v2 when v1 == v2 -> ()
  | RVar ({ contents = RUnbound id } as v), other
  | other, RVar ({ contents = RUnbound id } as v) ->
    if row_occurs id other then error "This effect row is recursive.";
    v := RLink other
  | RCons (label, args, rest_a), (RCons _ as b) ->
    let rest_b = rewrite_row label args b in
    unify_row rest_a rest_b
  | REmpty, RCons (label, _, _) | RCons (label, _, _), REmpty ->
    error "This code does not handle the effect '%s'." label
  | RVar { contents = RLink _ }, _ | _, RVar { contents = RLink _ } -> assert false

(* ---- field rows ---- *)

and fields_occurs id (f : infer_fields) =
  match repr_fields f with
  | FEmpty -> false
  | FVar { contents = FUnbound id' } -> id = id'
  | FVar { contents = FLink _ } -> assert false
  | FCons (_, _, rest) -> fields_occurs id rest

and rewrite_fields label (f : infer_fields) : infer_ty * infer_fields =
  match repr_fields f with
  | FCons (l, ty, rest) when String.equal l label -> ty, rest
  | FCons (l, ty, rest) ->
    let found, rest = rewrite_fields label rest in
    found, FCons (l, ty, rest)
  | FVar ({ contents = FUnbound _ } as v) ->
    let ty = fresh () in
    let tail = fresh_fields () in
    v := FLink (FCons (label, ty, tail));
    ty, tail
  | FVar { contents = FLink _ } -> assert false
  | FEmpty -> error "This value has no field '%s'." label

(* ---- unification ---- *)

and occurs id (t : infer_ty) =
  match repr t with
  | IVar { contents = Unbound (id', _) } -> id = id'
  | ITuple items -> List.exists (occurs id) items
  | IRecord f | INamed (_, _, f) ->
    let rec walk f =
      match repr_fields f with
      | FEmpty | FVar _ -> false
      | FCons (_, ty, rest) -> occurs id ty || walk rest
    in
    walk f
  | IFn (params, ret, _) -> List.exists (occurs id) params || occurs id ret
  | _ -> false

and unify_fields (a : infer_fields) (b : infer_fields) : unit =
  match repr_fields a, repr_fields b with
  | FEmpty, FEmpty -> ()
  | FVar v1, FVar v2 when v1 == v2 -> ()
  | FVar ({ contents = FUnbound id } as v), other
  | other, FVar ({ contents = FUnbound id } as v) ->
    if fields_occurs id other then error "This record type is recursive.";
    v := FLink other
  | FCons (label, ty, rest_a), (FCons _ as b) ->
    let found, rest_b = rewrite_fields label b in
    unify ty found;
    unify_fields rest_a rest_b
  | FEmpty, FCons (label, _, _) | FCons (label, _, _), FEmpty ->
    error "This value has no field '%s'." label
  | FVar { contents = FLink _ }, _ | _, FVar { contents = FLink _ } -> assert false

and strongest a b =
  match a, b with
  | Collection x, Collection y ->
    unify x y;
    Collection x
  | Collection _, Any -> a
  | Any, Collection _ -> b
  | Bound x, Bound y -> Bound (List.sort_uniq compare (x @ y))
  | Bound _, Any -> a
  | Any, Bound _ -> b
  | Collection _, other | other, Collection _ ->
    error "A collection is not %s." (string_of_kind other)
  | Bound _, other | other, Bound _ ->
    error "A bounded type parameter is not %s." (string_of_kind other)
  | Numeric, _ | _, Numeric -> Numeric
  | Addable, _ | _, Addable -> Addable
  | Any, Any -> Any

and kind_admits kind (t : infer_ty) =
  match kind, t with
  | Any, _ -> true
  | Numeric, (IInt | IFloat) -> true
  | Addable, (IInt | IFloat | IStr) -> true
  | (Numeric | Addable | Collection _ | Bound _), _ -> !extra_admits kind t

and unify (a : infer_ty) (b : infer_ty) : unit =
  let a = repr a
  and b = repr b in
  match a, b with
  | IVar r1, IVar r2 when r1 == r2 -> ()
  | ( IVar ({ contents = Unbound (id1, k1) } as r1)
    , IVar ({ contents = Unbound (id2, k2) } as r2) ) ->
    r2 := Unbound (id2, strongest k1 k2);
    (* The survivor inherits the mark, or a declared parameter stops being one
       the moment an operator gives it a kind. *)
    if Hashtbl.mem declared_params id1 then Hashtbl.replace declared_params id2 ();
    r1 := Link (IVar r2)
  | IVar ({ contents = Unbound (id, kind) } as r), t
  | t, IVar ({ contents = Unbound (id, kind) } as r) ->
    if occurs id t
    then error "This expression would have an infinitely recursive type.";
    if not (kind_admits kind t)
    then error "Expected %s, got %s." (string_of_kind kind) (string_of_infer_ty t);
    r := Link t
  | IInt, IInt | IFloat, IFloat | IStr, IStr | IByte, IByte | IChr, IChr | IBool, IBool | IUnit, IUnit -> ()
  | IRecord a, IRecord b -> unify_fields a b
  | INamed (a, xs, _), INamed (b, ys, _)
    when String.equal a b && List.length xs = List.length ys -> List.iter2 unify xs ys
  | ISum (a, xs), ISum (b, ys)
    when String.equal a b && List.length xs = List.length ys -> List.iter2 unify xs ys
  | ITuple a, ITuple b ->
    if List.length a <> List.length b
    then
      error
        "Expected a tuple of %d element(s), got one of %d."
        (List.length a)
        (List.length b);
    List.iter2 unify a b
  | IFn (p1, r1, e1), IFn (p2, r2, e2) ->
    if List.length p1 <> List.length p2
    then
      error
        "Expected a function of %d argument(s), got one of %d."
        (List.length p1)
        (List.length p2);
    List.iter2 unify p1 p2;
    unify r1 r2;
    unify_row e1 e2
  | _ -> error "Expected %s, got %s." (string_of_infer_ty a) (string_of_infer_ty b)

(* ---- schemes ---- *)

let rec walk_fields walk f =
  match repr_fields f with
  | FEmpty | FVar _ -> ()
  | FCons (_, ty, rest) ->
    walk ty;
    walk_fields walk rest

let free_vars (t : infer_ty) : (int * kind) list =
  let acc = ref [] in
  let rec walk t =
    match repr t with
    | IVar { contents = Unbound (id, kind) } ->
      if not (List.mem_assoc id !acc) then acc := (id, kind) :: !acc
    | ITuple items -> List.iter walk items
    | IRecord f -> walk_fields walk f
    | INamed (_, args, f) ->
      List.iter walk args;
      walk_fields walk f
    | ISum (_, args) -> List.iter walk args
    | IFn (params, ret, _) ->
      List.iter walk params;
      walk ret
    | _ -> ()
  in
  walk t;
  !acc

let free_row_vars (t : infer_ty) : int list =
  let acc = ref [] in
  (* An entry's arguments are types and may hold row variables of their own. *)
  let rec walk_row r =
    match repr_row r with
    | REmpty -> ()
    | RVar { contents = RUnbound id } -> if not (List.mem id !acc) then acc := id :: !acc
    | RVar { contents = RLink _ } -> assert false
    | RCons (_, args, rest) ->
      List.iter walk args;
      walk_row rest
  and walk t =
    match repr t with
    | ITuple items -> List.iter walk items
    | IRecord f -> walk_fields walk f
    | INamed (_, args, f) ->
      List.iter walk args;
      walk_fields walk f
    | ISum (_, args) -> List.iter walk args
    | IFn (params, ret, row) ->
      List.iter walk params;
      walk ret;
      walk_row row
    | _ -> ()
  in
  walk t;
  !acc

let mono body = { quantified = []; quantified_rows = []; quantified_fields = []; body }

let free_field_vars (t : infer_ty) : int list =
  let acc = ref [] in
  let rec walk_fields f =
    match repr_fields f with
    | FEmpty -> ()
    | FVar { contents = FUnbound id } -> if not (List.mem id !acc) then acc := id :: !acc
    | FVar { contents = FLink _ } -> assert false
    | FCons (_, ty, rest) ->
      walk ty;
      walk_fields rest
  and walk t =
    match repr t with
    | ITuple items -> List.iter walk items
    | IRecord f -> walk_fields f
    | INamed (_, args, f) ->
      List.iter walk args;
      walk_fields f
    | ISum (_, args) -> List.iter walk args
    | IFn (params, ret, _) ->
      List.iter walk params;
      walk ret
    | _ -> ()
  in
  walk t;
  !acc

let generalize ~env_vars ~env_rows ~env_fields body =
  { quantified =
      free_vars body |> List.map fst |> List.filter (fun id -> not (List.mem id env_vars))
  ; quantified_rows = free_row_vars body |> List.filter (fun id -> not (List.mem id env_rows))
  ; quantified_fields =
      free_field_vars body |> List.filter (fun id -> not (List.mem id env_fields))
  ; body
  }

let instantiate ?(bound = []) (s : scheme) : infer_ty =
  if s.quantified = [] && s.quantified_rows = [] && s.quantified_fields = []
  then s.body
  else (
    let types = Hashtbl.create 8
    and rows = Hashtbl.create 8
    and fields = Hashtbl.create 8 in
    List.iter (fun (id, t) -> Hashtbl.replace types id t) bound;
    let rec walk_row r =
      match repr_row r with
      | REmpty -> REmpty
      | RVar { contents = RUnbound id } as original ->
        if List.mem id s.quantified_rows
        then (
          match Hashtbl.find_opt rows id with
          | Some copy -> copy
          | None ->
            let copy = fresh_row () in
            Hashtbl.add rows id copy;
            copy)
        else original
      | RVar { contents = RLink _ } -> assert false
      | RCons (label, args, rest) -> RCons (label, List.map walk args, walk_row rest)
    and walk t =
      match repr t with
      | IVar { contents = Unbound (id, kind) } as original ->
        if List.mem id s.quantified
        then (
          match Hashtbl.find_opt types id with
          | Some copy -> copy
          | None ->
            (* A kind may carry a type of its own, and sharing it across copies
               would pin every instantiation to whatever the first one chose. *)
            let copy =
              fresh_with
                (match kind with
                 | Collection elem -> Collection (walk elem)
                 | other -> other)
            in
            Hashtbl.add types id copy;
            copy)
        else original
      | ITuple items -> ITuple (List.map walk items)
      | ISum (name, args) -> ISum (name, List.map walk args)
      | (IRecord _ | INamed _) as r ->
        let f =
          match r with
          | IRecord f | INamed (_, _, f) -> f
          | _ -> assert false
        in
        let rec copy f =
          match repr_fields f with
          | FEmpty -> FEmpty
          | FVar { contents = FUnbound id } as original ->
            if List.mem id s.quantified_fields
            then (
              match Hashtbl.find_opt fields id with
              | Some copy -> copy
              | None ->
                let copy = fresh_fields () in
                Hashtbl.add fields id copy;
                copy)
            else original
          | FVar { contents = FLink _ } -> assert false
          | FCons (label, ty, rest) -> FCons (label, walk ty, copy rest)
        in
        (match r with
         | INamed (name, args, _) -> INamed (name, List.map walk args, copy f)
         | _ -> IRecord (copy f))
      | IFn (params, ret, row) -> IFn (List.map walk params, walk ret, walk_row row)
      | concrete -> concrete
    in
    walk s.body)

let declare_param (t : infer_ty) =
  match repr t with
  | IVar { contents = Unbound (id, _) } -> Hashtbl.replace declared_params id ()
  | _ -> ()

let var_id (t : infer_ty) =
  match repr t with
  | IVar { contents = Unbound (id, _) } -> id
  | other -> error "Not a type variable: %s." (string_of_infer_ty other)

let rec substitute mapping (t : infer_ty) : infer_ty =
  match repr t with
  | IVar { contents = Unbound (id, _) } as original ->
    (match List.assoc_opt id mapping with
     | Some replacement -> replacement
     | None -> original)
  | ITuple items -> ITuple (List.map (substitute mapping) items)
  | IRecord f -> IRecord (substitute_fields mapping f)
  | INamed (name, args, f) ->
    INamed
      (name, List.map (substitute mapping) args, substitute_fields mapping f)
  | ISum (name, args) -> ISum (name, List.map (substitute mapping) args)
  | IFn (params, ret, row) ->
    IFn (List.map (substitute mapping) params, substitute mapping ret, row)
  | concrete -> concrete

and substitute_fields mapping (f : infer_fields) : infer_fields =
  match repr_fields f with
  | FCons (label, ty, rest) ->
    FCons (label, substitute mapping ty, substitute_fields mapping rest)
  | other -> other

let rec concrete_all (args : infer_ty list) : ty list option =
  List.fold_right
    (fun a acc -> Option.bind acc (fun acc -> Option.map (fun a -> a :: acc) (concrete a)))
    args
    (Some [])

and concrete (t : infer_ty) : ty option =
  let ( let* ) = Option.bind in
  match repr t with
  | IInt -> Some Int
  | IFloat -> Some Float
  | IStr -> Some Str
  | IByte -> Some Byte
  | IChr -> Some Chr
  | IBool -> Some Bool
  | IUnit -> Some Unit
  | ISum (name, args) ->
    let* args = concrete_all args in
    Some (Sum (name, args))
  | INamed (name, args, f) ->
    let* args = concrete_all args in
    let rec collect f =
      match repr_fields f with
      | FEmpty | FVar _ -> Some []
      | FCons (label, ty, rest) ->
        let* ty = concrete ty in
        let* rest = collect rest in
        Some ((label, ty) :: rest)
    in
    let* fields = collect f in
    Some (Named (name, args, List.sort compare fields))
  | IRecord f ->
    let rec collect f =
      match repr_fields f with
      | FEmpty -> Some []
      | FVar _ -> Some []
      | FCons (label, ty, rest) ->
        let* ty = concrete ty in
        let* rest = collect rest in
        Some ((label, ty) :: rest)
    in
    let* fields = collect f in
    Some (Record (List.sort compare fields))
  | ITuple items ->
    let* items =
      List.fold_right
        (fun i acc ->
          let* acc = acc in
          let* i = concrete i in
          Some (i :: acc))
        items
        (Some [])
    in
    Some (Tuple items)
  | IFn (params, ret, row) ->
    let* params =
      List.fold_right
        (fun p acc ->
          let* acc = acc in
          let* p = concrete p in
          Some (p :: acc))
        params
        (Some [])
    in
    let* ret = concrete ret in
    let entries =
      List.filter_map
        (fun (label, args) ->
          let rec each acc = function
            | [] -> Some (List.rev acc)
            | a :: rest ->
              (match concrete a with
               | Some a -> each (a :: acc) rest
               | None -> None)
          in
          Option.map (fun args -> label, args) (each [] args))
        (fst (labels_of_infer_row row))
    in
    Some (Fn (params, ret, List.sort compare entries))
  | IVar _ -> None

let rec of_ty (t : ty) : infer_ty =
  match t with
  | Int -> IInt
  | Float -> IFloat
  | Str -> IStr
  | Byte -> IByte
  | Chr -> IChr
  | Bool -> IBool
  | Unit -> IUnit
  | Tuple items -> ITuple (List.map of_ty items)
  | Record fields ->
    IRecord
      (List.fold_right (fun (l, t) rest -> FCons (l, of_ty t, rest)) fields FEmpty)
  | Named (name, args, fields) ->
    INamed
      ( name
      , List.map of_ty args
      , List.fold_right (fun (l, t) rest -> FCons (l, of_ty t, rest)) fields FEmpty )
  | Sum (name, args) -> ISum (name, List.map of_ty args)
  | Fn (params, ret, row) ->
    IFn
      ( List.map of_ty params
      , of_ty ret
      , List.fold_right
          (fun (l, args) rest -> RCons (l, List.map of_ty args, rest))
          row
          REmpty )
  | Generic _ -> fresh ()

(* ---- resolve ---- *)

(* Unbound numeric variables default to int; the rest stay polymorphic. *)
let rec resolve_row (r : infer_row) : row =
  let labels, _ = labels_of_infer_row r in
  List.sort compare (List.map (fun (l, args) -> l, List.map resolve args) labels)

and resolve (t : infer_ty) : ty =
  match repr t with
  | IInt -> Int
  | IFloat -> Float
  | IStr -> Str
  | IByte -> Byte
  | IChr -> Chr
  | IBool -> Bool
  | IUnit -> Unit
  | ITuple items -> Tuple (List.map resolve items)
  | ISum (name, args) -> Sum (name, List.map resolve args)
  | IRecord f | INamed (_, _, f) ->
    let rec collect f =
      match repr_fields f with
      | FEmpty | FVar _ -> []
      | FCons (label, ty, rest) -> (label, resolve ty) :: collect rest
    in
    let fields = List.sort compare (collect f) in
    (match repr t with
     | INamed (name, args, _) -> Named (name, List.map resolve args, fields)
     | _ -> Record fields)
  | IFn (params, ret, row) -> Fn (List.map resolve params, resolve ret, resolve_row row)
  | IVar { contents = Unbound (id, kind) } ->
    if Hashtbl.mem declared_params id
    then Generic id
    else (
      match kind with
      | Numeric | Addable -> Int
      | Collection elem -> array (resolve elem)
      (* A bound is discharged by unification, so one still unbound here
         belongs to a definition nothing ever instantiated. *)
      | Bound _ -> Generic id
      | Any -> Generic id)
  | IVar { contents = Link _ } -> assert false (* repr collapsed these *)
