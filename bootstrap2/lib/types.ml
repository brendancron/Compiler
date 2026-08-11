(* [infer_ty] is mutable and may hold unresolved variables; [ty] cannot, so
   consumers never have to handle a type that is still being inferred.
   [resolve] is the only way across. *)

type ty =
  | Int
  | Float
  | Str
  | Bool
  | Unit
  | Fn of ty list * ty
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
  | IFn of infer_ty list * infer_ty
  | IVar of tv ref

and tv =
  | Unbound of int * kind
  | Link of infer_ty

(* Variables listed in [quantified] are copied by [instantiate]; every other
   variable in [body] stays shared with the enclosing scope. *)
type scheme =
  { quantified : int list
  ; body : infer_ty
  }

exception Type_error of string

let error fmt = Printf.ksprintf (fun message -> raise (Type_error message)) fmt

let counter = ref 0

let fresh_with kind =
  incr counter;
  IVar (ref (Unbound (!counter, kind)))

let fresh () = fresh_with Any
let reset () = counter := 0

let rec repr (t : infer_ty) : infer_ty =
  match t with
  | IVar ({ contents = Link inner } as r) ->
    let target = repr inner in
    r := Link target;
    target
  | t -> t

let string_of_kind = function
  | Any -> "any"
  | Addable -> "int, float or string"
  | Numeric -> "int or float"

let rec string_of_infer_ty (t : infer_ty) : string =
  match repr t with
  | IInt -> "int"
  | IFloat -> "float"
  | IStr -> "string"
  | IBool -> "bool"
  | IUnit -> "unit"
  | IFn (params, ret) ->
    Printf.sprintf
      "(%s) -> %s"
      (String.concat ", " (List.map string_of_infer_ty params))
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
  | Fn (params, ret) ->
    Printf.sprintf
      "(%s) -> %s"
      (String.concat ", " (List.map string_of_ty params))
      (string_of_ty ret)
  | Generic id -> Printf.sprintf "'%d" id

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
  | IFn (params, ret) -> List.exists (occurs id) params || occurs id ret
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
  | IFn (p1, r1), IFn (p2, r2) ->
    if List.length p1 <> List.length p2
    then
      error
        "Expected a function of %d argument(s), got one of %d."
        (List.length p1)
        (List.length p2);
    List.iter2 unify p1 p2;
    unify r1 r2
  | _ -> error "Expected %s, got %s." (string_of_infer_ty a) (string_of_infer_ty b)

(* ---- schemes ---- *)

let free_vars (t : infer_ty) : (int * kind) list =
  let acc = ref [] in
  let rec walk t =
    match repr t with
    | IVar { contents = Unbound (id, kind) } ->
      if not (List.mem_assoc id !acc) then acc := (id, kind) :: !acc
    | IFn (params, ret) ->
      List.iter walk params;
      walk ret
    | _ -> ()
  in
  walk t;
  !acc

let mono body = { quantified = []; body }

(* [env_vars] is the enclosing scope's free set: anything in it stays shared. *)
let generalize ~env_vars body =
  let quantified =
    free_vars body |> List.map fst |> List.filter (fun id -> not (List.mem id env_vars))
  in
  { quantified; body }

let instantiate (s : scheme) : infer_ty =
  if s.quantified = []
  then s.body
  else (
    let copies = Hashtbl.create 8 in
    let rec walk t =
      match repr t with
      | IVar { contents = Unbound (id, kind) } as original ->
        if List.mem id s.quantified
        then (
          match Hashtbl.find_opt copies id with
          | Some fresh -> fresh
          | None ->
            let fresh = fresh_with kind in
            Hashtbl.add copies id fresh;
            fresh)
        else original
      | IFn (params, ret) -> IFn (List.map walk params, walk ret)
      | concrete -> concrete
    in
    walk s.body)

(* ---- resolve ---- *)

(* Numeric variables still unbound default to int, so `fn double(x) { return x + x; }`
   is `(int) -> int`. The rest were never constrained at all and are polymorphic. *)
let rec resolve (t : infer_ty) : ty =
  match repr t with
  | IInt -> Int
  | IFloat -> Float
  | IStr -> Str
  | IBool -> Bool
  | IUnit -> Unit
  | IFn (params, ret) -> Fn (List.map resolve params, resolve ret)
  | IVar { contents = Unbound (id, kind) } ->
    (match kind with
     | Numeric | Addable -> Int
     | Any -> Generic id)
  | IVar { contents = Link _ } -> assert false (* repr collapsed these *)
