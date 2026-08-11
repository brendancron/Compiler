(* [infer_ty] is mutable and may hold unresolved variables; [ty] cannot, so
   consumers never have to handle a type that is still being inferred.
   [resolve] is the only way across. *)

(* Effect labels a function may perform. A multiset, following Koka's scoped
   rows: two nested handlers for the same effect are distinguished by there
   being two occurrences, and each `handle` peels one. *)
type row = string list

type ty =
  | Int
  | Float
  | Str
  | Bool
  | Unit
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

(* Variables listed here are copied by [instantiate]; every other variable in
   [body] stays shared with the enclosing scope. *)
type scheme =
  { quantified : int list
  ; quantified_rows : int list
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
  | IFn (params, ret, row) ->
    Printf.sprintf
      "(%s) -> %s%s"
      (String.concat ", " (List.map string_of_infer_ty params))
      (string_of_infer_ty ret)
      (string_of_infer_row row)
  | IVar { contents = Unbound (id, _) } -> Printf.sprintf "'%d" id
  | IVar { contents = Link _ } -> assert false (* repr collapsed these *)

let rec string_of_ty (t : ty) : string =
  match t with
  | Int -> "int"
  | Float -> "float"
  | Str -> "string"
  | Bool -> "bool"
  | Unit -> "unit"
  | Fn (params, ret, row) ->
    Printf.sprintf
      "(%s) -> %s%s"
      (String.concat ", " (List.map string_of_ty params))
      (string_of_ty ret)
      (string_of_row row)
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
  | IFn (params, ret, _) -> List.exists (occurs id) params || occurs id ret
  | _ -> false

let rec unify (a : infer_ty) (b : infer_ty) : unit =
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

let free_vars (t : infer_ty) : (int * kind) list =
  let acc = ref [] in
  let rec walk t =
    match repr t with
    | IVar { contents = Unbound (id, kind) } ->
      if not (List.mem_assoc id !acc) then acc := (id, kind) :: !acc
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
    | IFn (params, ret, row) ->
      List.iter walk params;
      walk ret;
      walk_row row
    | _ -> ()
  in
  walk t;
  !acc

let mono body = { quantified = []; quantified_rows = []; body }

(* [env_vars] and [env_rows] are the enclosing scope's free sets: anything in
   them stays shared. *)
let generalize ~env_vars ~env_rows body =
  { quantified =
      free_vars body |> List.map fst |> List.filter (fun id -> not (List.mem id env_vars))
  ; quantified_rows = free_row_vars body |> List.filter (fun id -> not (List.mem id env_rows))
  ; body
  }

let instantiate (s : scheme) : infer_ty =
  if s.quantified = [] && s.quantified_rows = []
  then s.body
  else (
    let types = Hashtbl.create 8
    and rows = Hashtbl.create 8 in
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
      | IFn (params, ret, row) -> IFn (List.map walk params, walk ret, walk_row row)
      | concrete -> concrete
    in
    walk s.body)

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
  | IFn (params, ret, row) -> Fn (List.map resolve params, resolve ret, resolve_row row)
  | IVar { contents = Unbound (id, kind) } ->
    (match kind with
     | Numeric | Addable -> Int
     | Any -> Generic id)
  | IVar { contents = Link _ } -> assert false (* repr collapsed these *)
