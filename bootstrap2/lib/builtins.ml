(* What a program can use without declaring it: the types, their signatures, the
   entries that make them containers, and the values that implement them.

   Nothing that consumes a program reads this. The checker registers these the
   same way it registers a declaration, and the interpreter is handed the values
   without being told which are builtin. *)

let types = [ "Array", 1 ]

(* Array is the one the others are built from, so a literal nothing narrowed is
   one. *)
let default_container = "Array"

let array elem = Types.iopaque "Array" [ elem ]
let methods : (string * string * (unit -> Types.infer_ty list * Types.infer_ty)) list =
  [ ( "Array"
    , "len"
    , fun () ->
        let elem = Types.fresh () in
        [ array elem ], Types.IInt )
  ; ( "Array"
    , "contains"
    , fun () ->
        let elem = Types.fresh () in
        [ array elem; elem ], Types.IBool )
  ; "string", "len", (fun () -> [ Types.IStr ], Types.IInt)
  ; "string", "trim", (fun () -> [ Types.IStr ], Types.IStr)
  ; "string", "contains", (fun () -> [ Types.IStr; Types.IStr ], Types.IBool)
  ; "string", "split", (fun () -> [ Types.IStr; Types.IStr ], array Types.IStr)
  ; "string", "chars", (fun () -> [ Types.IStr ], array Types.IStr)
  ]

(* No HM type describes these: any number of arguments, of any type. A call is
   checked structurally and a bare reference is unconstrained. *)
let variadic = [ "print" ]

let functions : (string * (unit -> Types.infer_ty list * Types.infer_ty)) list =
  [ ( "__array_new"
    , fun () ->
        let elem = Types.fresh () in
        [ Types.IInt; elem ], array elem )
  ; "clock", (fun () -> [], Types.IFloat)
  ; ("str", fun () -> [ Types.fresh () ], Types.IStr)
  ]

(* The constructors and accessors [Resolve] emits. They are named here and never
   type checked: the call it builds carries the type it derived. *)
let register registry =
  Types.default_container := default_container;
  Registry.register_constructor registry "Array" "__array_new";
  Registry.register_container registry "Array" "__array_of";

  Registry.register_indexed
    registry
    "Array"
    { Registry.get = "__array_get"; set = "__array_set" };
  (* List is declared in the prelude; these are the two entries any type uses to
     opt into literals and indexing. *)
  Registry.register_container registry "List" "__list_of";
  Registry.register_indexed
    registry
    "List"
    { Registry.get = "__list_get"; set = "__list_set" }


(* ---- values ---- *)

let elements span name = function
  | Value.Array items -> items
  | _ -> Value.fail span "Cannot apply %s to these arguments." name

let checked span name items i =
  if i < 0 || i >= Array.length items
  then Value.fail span "Index %d is out of bounds for length %d." i (Array.length items);
  i

let values ~out =
  let native name arity apply = name, Value.Fn { Value.name; arity; apply } in
  let variadic name apply = native name None apply in
  let two name f =
    native name (Some 2) (fun span args ->
      match args with
      | [ a; b ] -> f span a b
      | _ -> Value.fail span "Cannot apply %s to these arguments." name)
  in
  let one name f =
    native name (Some 1) (fun span args ->
      match args with
      | [ a ] -> f span a
      | _ -> Value.fail span "Cannot apply %s to these arguments." name)
  in
  [ variadic "print" (fun _ args ->
      out (String.concat " " (List.map Value.string_of_value args));
      out "\n";
      Value.Unit)
  ; one "str" (fun _ v -> Value.Str (Value.string_of_value v))
  ; native "clock" (Some 0) (fun _ _ -> Value.Float (Sys.time ()))
  ; variadic "__array_of" (fun _ args -> Value.Array (Array.of_list args))
  ; two "__array_new" (fun span n fill ->
      match n with
      | Value.Int n when n >= 0 -> Value.Array (Array.make n fill)
      | _ -> Value.fail span "An array's length must not be negative.")
  ; two "__array_get" (fun span container i ->
      match container, i with
      | Value.Array items, Value.Int i -> items.(checked span "__array_get" items i)
      | _ -> Value.fail span "Cannot apply __array_get to these arguments.")
  ; native "__array_set" (Some 3) (fun span args ->
      match args with
      | [ Value.Array items; Value.Int i; v ] ->
        items.(checked span "__array_set" items i) <- v;
        v
      | _ -> Value.fail span "Cannot apply __array_set to these arguments.")
  ; one (Ast.method_name "Array" "len") (fun span v ->
      Value.Int (Array.length (elements span "len" v)))
  ; two (Ast.method_name "Array" "contains") (fun span v needle ->
      Value.Bool (Array.exists (Value.values_equal needle) (elements span "contains" v)))
  ; one (Ast.method_name "string" "len") (fun span v ->
      match v with
      | Value.Str s -> Value.Int (String.length s)
      | _ -> Value.fail span "Cannot apply len to these arguments.")
  ; one (Ast.method_name "string" "trim") (fun span v ->
      match v with
      | Value.Str s -> Value.Str (String.trim s)
      | _ -> Value.fail span "Cannot apply trim to these arguments.")
  ; two (Ast.method_name "string" "contains") (fun span v needle ->
      match v, needle with
      | Value.Str s, Value.Str needle ->
        let n = String.length needle in
        let rec search i =
          i + n <= String.length s
          && (String.equal (String.sub s i n) needle || search (i + 1))
        in
        Value.Bool (n = 0 || search 0)
      | _ -> Value.fail span "Cannot apply contains to these arguments.")
  ; two (Ast.method_name "string" "split") (fun span v sep ->
      match v, sep with
      | Value.Str s, Value.Str sep when String.length sep = 1 ->
        Value.Array
          (Array.of_list (List.map (fun p -> Value.Str p) (String.split_on_char sep.[0] s)))
      | _ -> Value.fail span "Cannot apply split to these arguments.")
  ; one (Ast.method_name "string" "chars") (fun span v ->
      match v with
      | Value.Str s ->
        Value.Array (Array.init (String.length s) (fun i -> Value.Str (String.make 1 s.[i])))
      | _ -> Value.fail span "Cannot apply chars to these arguments.")
  ]

(* The scope a program starts in. *)
let env ~out =
  let env = Value.new_env None in
  List.iter (fun (name, v) -> Value.define env name v) (values ~out);
  env
