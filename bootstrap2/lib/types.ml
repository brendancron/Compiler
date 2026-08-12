(* [infer_ty] is mutable and may hold unresolved variables; [ty] cannot, so
   consumers never have to handle a type that is still being inferred.
   [resolve] is the only way across. *)

(* Effect labels a function may perform. A multiset, following Koka's scoped
   rows: two nested handlers for the same effect are distinguished by there
   being two occurrences, and each `handle` peels one. *)
type row = string list

(* Field name to type, sorted, so two records with the same fields are the same
   type however they were written. *)
type fields = (string * ty) list

and ty =
  | Int
  | Float
  | Str
  | Bool
  | Unit
  | Array of ty
  | Tuple of ty list
  | Record of fields
  (* Nominal: two declarations with identical fields are different types. The
     fields ride along so a field access needs no lookup. *)
  | Named of string * fields
  | Fn of ty list * ty * row
  (* Quantified, not unresolved. Reaching codegen means the call site was never
     monomorphized. *)
  | Generic of int

(* Ordered by strength: [Any] admits everything, [Numeric] the least. *)
type kind =
  | Any
  | Addable (* int, float, str — the operand type of `+` *)
  | Numeric (* int, float *)

type infer_ty =
  | IInt
  | IFloat
  | IStr
  | IBool
  | IUnit
  | IArray of infer_ty
  | ITuple of infer_ty list
  | IRecord of infer_fields
  | INamed of string * infer_fields
  | IFn of infer_ty list * infer_ty * infer_row
  | IVar of tv ref

and tv =
  | Unbound of int * kind
  | Link of infer_ty

(* A row is a list of labels ending in either [REmpty] (closed) or a variable
   (open). Unification rewrites the tail to admit a label, which is what lets a
   pure function be passed where an effectful one is expected. *)
and infer_row =
  | REmpty
  | RCons of string * infer_row
  | RVar of rv ref

and rv =
  | RUnbound of int
  | RLink of infer_row

(* The same shape as an effect row, carrying a type per label. An open tail is
   what lets a function accept any record that has the field it reads. *)
and infer_fields =
  | FEmpty
  | FCons of string * infer_ty * infer_fields
  | FVar of fv ref

and fv =
  | FUnbound of int
  | FLink of infer_fields

(* Variables listed here are copied by [instantiate]; every other variable in
   [body] stays shared with the enclosing scope. *)
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

let reset () = counter := 0

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

let string_of_kind = function
  | Any -> "any"
  | Addable -> "int, float or string"
  | Numeric -> "int or float"

let string_of_row (r : row) =
  match r with
  | [] -> ""
  | labels -> Printf.sprintf " <%s>" (String.concat ", " labels)

let rec labels_of_infer_row (r : infer_row) : string list * bool =
  match repr_row r with
  | REmpty -> [], false
  | RVar _ -> [], true
  | RCons (label, rest) ->
    let labels, open_ = labels_of_infer_row rest in
    label :: labels, open_

let string_of_infer_row (r : infer_row) =
  match labels_of_infer_row r with
  | [], false -> ""
  | labels, open_ ->
    Printf.sprintf " <%s%s>" (String.concat ", " labels) (if open_ then "|_" else "")

let rec string_of_infer_ty (t : infer_ty) : string =
  match repr t with
  | IInt -> "int"
  | IFloat -> "float"
  | IStr -> "string"
  | IBool -> "bool"
  | IUnit -> "unit"
  | IArray elem -> Printf.sprintf "Array<%s>" (string_of_infer_ty elem)
  | ITuple items ->
    Printf.sprintf "(%s)" (String.concat ", " (List.map string_of_infer_ty items))
  | INamed (name, _) -> name
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

let rec string_of_ty (t : ty) : string =
  match t with
  | Int -> "int"
  | Float -> "float"
  | Str -> "string"
  | Bool -> "bool"
  | Unit -> "unit"
  | Array elem -> Printf.sprintf "Array<%s>" (string_of_ty elem)
  | Tuple items ->
    Printf.sprintf "(%s)" (String.concat ", " (List.map string_of_ty items))
  | Named (name, _) -> name
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

(* ---- row unification ---- *)

let rec row_occurs id (r : infer_row) =
  match repr_row r with
  | REmpty -> false
  | RVar { contents = RUnbound id' } -> id = id'
  | RVar { contents = RLink _ } -> assert false
  | RCons (_, rest) -> row_occurs id rest

(* Produce what remains of [r] after removing one occurrence of [label]. An open
   tail grows a new link rather than failing, which is how a row variable comes
   to admit an effect it did not previously mention. *)
let rec rewrite_row label (r : infer_row) : infer_row =
  match repr_row r with
  | RCons (l, rest) when String.equal l label -> rest
  | RCons (l, rest) -> RCons (l, rewrite_row label rest)
  | RVar ({ contents = RUnbound _ } as v) ->
    let tail = fresh_row () in
    v := RLink (RCons (label, tail));
    tail
  | RVar { contents = RLink _ } -> assert false
  | REmpty -> error "This code does not handle the effect '%s'." label

let rec unify_row (a : infer_row) (b : infer_row) : unit =
  match repr_row a, repr_row b with
  | REmpty, REmpty -> ()
  | RVar v1, RVar v2 when v1 == v2 -> ()
  | RVar ({ contents = RUnbound id } as v), other
  | other, RVar ({ contents = RUnbound id } as v) ->
    if row_occurs id other then error "This effect row is recursive.";
    v := RLink other
  | RCons (label, rest_a), (RCons _ as b) ->
    let rest_b = rewrite_row label b in
    unify_row rest_a rest_b
  | REmpty, RCons (label, _) | RCons (label, _), REmpty ->
    error "This code does not handle the effect '%s'." label
  | RVar { contents = RLink _ }, _ | _, RVar { contents = RLink _ } -> assert false

(* ---- field rows ---- *)

let rec fields_occurs id (f : infer_fields) =
  match repr_fields f with
  | FEmpty -> false
  | FVar { contents = FUnbound id' } -> id = id'
  | FVar { contents = FLink _ } -> assert false
  | FCons (_, _, rest) -> fields_occurs id rest

(* What remains of [f] after removing [label], along with the type it had. An
   open tail grows the field rather than failing, which is what lets a function
   read a field from any record that has one. *)
let rec rewrite_fields label (f : infer_fields) : infer_ty * infer_fields =
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

(* A variable used by both `+` and `-` must end up Numeric, not Addable. *)
let strongest a b =
  match a, b with
  | Numeric, _ | _, Numeric -> Numeric
  | Addable, _ | _, Addable -> Addable
  | Any, Any -> Any

let kind_admits kind (t : infer_ty) =
  match kind, t with
  | Any, _ -> true
  | Numeric, (IInt | IFloat) -> true
  | Addable, (IInt | IFloat | IStr) -> true
  | (Numeric | Addable), _ -> false

let rec occurs id (t : infer_ty) =
  match repr t with
  | IVar { contents = Unbound (id', _) } -> id = id'
  | IArray elem -> occurs id elem
  | ITuple items -> List.exists (occurs id) items
  | IRecord f | INamed (_, f) ->
    let rec walk f =
      match repr_fields f with
      | FEmpty | FVar _ -> false
      | FCons (_, ty, rest) -> occurs id ty || walk rest
    in
    walk f
  | IFn (params, ret, _) -> List.exists (occurs id) params || occurs id ret
  | _ -> false

let rec unify_fields (a : infer_fields) (b : infer_fields) : unit =
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

and unify (a : infer_ty) (b : infer_ty) : unit =
  let a = repr a
  and b = repr b in
  match a, b with
  | IVar r1, IVar r2 when r1 == r2 -> ()
  | ( IVar ({ contents = Unbound (_, k1) } as r1)
    , IVar ({ contents = Unbound (id2, k2) } as r2) ) ->
    r2 := Unbound (id2, strongest k1 k2);
    r1 := Link (IVar r2)
  | IVar ({ contents = Unbound (id, kind) } as r), t
  | t, IVar ({ contents = Unbound (id, kind) } as r) ->
    if occurs id t
    then error "This expression would have an infinitely recursive type.";
    if not (kind_admits kind t)
    then error "Expected %s, got %s." (string_of_kind kind) (string_of_infer_ty t);
    r := Link t
  | IInt, IInt | IFloat, IFloat | IStr, IStr | IBool, IBool | IUnit, IUnit -> ()
  | IArray a, IArray b -> unify a b
  | IRecord a, IRecord b -> unify_fields a b
  (* Nominal, so the name decides and the fields follow from it. *)
  | INamed (a, _), INamed (b, _) when String.equal a b -> ()
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
    | IArray elem -> walk elem
    | ITuple items -> List.iter walk items
    | IRecord f | INamed (_, f) -> walk_fields walk f
    | IFn (params, ret, _) ->
      List.iter walk params;
      walk ret
    | _ -> ()
  in
  walk t;
  !acc

let free_row_vars (t : infer_ty) : int list =
  let acc = ref [] in
  let rec walk_row r =
    match repr_row r with
    | REmpty -> ()
    | RVar { contents = RUnbound id } -> if not (List.mem id !acc) then acc := id :: !acc
    | RVar { contents = RLink _ } -> assert false
    | RCons (_, rest) -> walk_row rest
  in
  let rec walk t =
    match repr t with
    | IArray elem -> walk elem
    | ITuple items -> List.iter walk items
    | IRecord f | INamed (_, f) -> walk_fields walk f
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
    | IArray elem -> walk elem
    | ITuple items -> List.iter walk items
    | IRecord f | INamed (_, f) -> walk_fields f
    | IFn (params, ret, _) ->
      List.iter walk params;
      walk ret
    | _ -> ()
  in
  walk t;
  !acc

(* [env_vars] and [env_rows] are the enclosing scope's free sets: anything in
   them stays shared. *)
let generalize ~env_vars ~env_rows ~env_fields body =
  { quantified =
      free_vars body |> List.map fst |> List.filter (fun id -> not (List.mem id env_vars))
  ; quantified_rows = free_row_vars body |> List.filter (fun id -> not (List.mem id env_rows))
  ; quantified_fields =
      free_field_vars body |> List.filter (fun id -> not (List.mem id env_fields))
  ; body
  }

let instantiate (s : scheme) : infer_ty =
  if s.quantified = [] && s.quantified_rows = [] && s.quantified_fields = []
  then s.body
  else (
    let types = Hashtbl.create 8
    and rows = Hashtbl.create 8
    and fields = Hashtbl.create 8 in
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
      | RCons (label, rest) -> RCons (label, walk_row rest)
    in
    let rec walk t =
      match repr t with
      | IVar { contents = Unbound (id, kind) } as original ->
        if List.mem id s.quantified
        then (
          match Hashtbl.find_opt types id with
          | Some copy -> copy
          | None ->
            let copy = fresh_with kind in
            Hashtbl.add types id copy;
            copy)
        else original
      | IArray elem -> IArray (walk elem)
      | ITuple items -> ITuple (List.map walk items)
      | (IRecord _ | INamed _) as r ->
        let f = (match r with IRecord f | INamed (_, f) -> f | _ -> assert false) in
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
         | INamed (name, _) -> INamed (name, copy f)
         | _ -> IRecord (copy f))
      | IFn (params, ret, row) -> IFn (List.map walk params, walk ret, walk_row row)
      | concrete -> concrete
    in
    walk s.body)

(* The resolved type, but only when nothing is still being inferred. Used where
   a lookup needs a concrete type and must not force a defaulting decision. *)
let rec concrete (t : infer_ty) : ty option =
  let ( let* ) = Option.bind in
  match repr t with
  | IInt -> Some Int
  | IFloat -> Some Float
  | IStr -> Some Str
  | IBool -> Some Bool
  | IUnit -> Some Unit
  | IArray elem ->
    let* elem = concrete elem in
    Some (Array elem)
  | INamed (name, f) ->
    let rec collect f =
      match repr_fields f with
      | FEmpty | FVar _ -> Some []
      | FCons (label, ty, rest) ->
        let* ty = concrete ty in
        let* rest = collect rest in
        Some ((label, ty) :: rest)
    in
    let* fields = collect f in
    Some (Named (name, List.sort compare fields))
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
    Some (Fn (params, ret, List.sort String.compare (fst (labels_of_infer_row row))))
  | IVar _ -> None

(* Back the other way, for a rule that has a concrete type in hand and needs to
   unify against it. *)
let rec of_ty (t : ty) : infer_ty =
  match t with
  | Int -> IInt
  | Float -> IFloat
  | Str -> IStr
  | Bool -> IBool
  | Unit -> IUnit
  | Array elem -> IArray (of_ty elem)
  | Tuple items -> ITuple (List.map of_ty items)
  | Record fields ->
    IRecord
      (List.fold_right (fun (l, t) rest -> FCons (l, of_ty t, rest)) fields FEmpty)
  | Named (name, fields) ->
    INamed
      (name, List.fold_right (fun (l, t) rest -> FCons (l, of_ty t, rest)) fields FEmpty)
  | Fn (params, ret, row) ->
    IFn
      ( List.map of_ty params
      , of_ty ret
      , List.fold_right (fun l rest -> RCons (l, rest)) row REmpty )
  | Generic _ -> fresh ()

(* ---- resolve ---- *)

(* An unconstrained row is closed to empty, so a pure function prints as
   `(int) -> int` rather than exposing its row variable. *)
let resolve_row (r : infer_row) : row =
  let labels, _ = labels_of_infer_row r in
  List.sort String.compare labels

(* Numeric variables still unbound default to int, so `fn double(x) { return x + x; }`
   is `(int) -> int`. The rest were never constrained at all and are polymorphic. *)
let rec resolve (t : infer_ty) : ty =
  match repr t with
  | IInt -> Int
  | IFloat -> Float
  | IStr -> Str
  | IBool -> Bool
  | IUnit -> Unit
  | IArray elem -> Array (resolve elem)
  | ITuple items -> Tuple (List.map resolve items)
  | IRecord f | INamed (_, f) ->
    let rec collect f =
      (* An unconstrained tail closes, the way an unconstrained effect row
         does. *)
      match repr_fields f with
      | FEmpty | FVar _ -> []
      | FCons (label, ty, rest) -> (label, resolve ty) :: collect rest
    in
    let fields = List.sort compare (collect f) in
    (match repr t with
     | INamed (name, _) -> Named (name, fields)
     | _ -> Record fields)
  | IFn (params, ret, row) -> Fn (List.map resolve params, resolve ret, resolve_row row)
  | IVar { contents = Unbound (id, kind) } ->
    (match kind with
     | Numeric | Addable -> Int
     | Any -> Generic id)
  | IVar { contents = Link _ } -> assert false (* repr collapsed these *)
