
`[1, 2, 3]` is one syntax that builds any of the collection types. Which one it builds is decided by inference, not by the literal.

```cronyx
var a: Array<int>          = [1, 2, 3];
var l: List<int>           = [1, 2, 3];
var s: Set<int>            = [1, 2, 3];
var m: Map<string, int>    = [("alice", 30), ("bob", 25)];
```

## Inference

The literal does not produce a type. It produces a variable carrying a constraint — *a collection whose element is `T`* — and the candidates are the ones the compiler knows, so checking it is a membership test rather than a search.

```ocaml
| Collection of infer_ty        (* the element type *)

kind_admits (Collection elem) t =
  match t with
  | Array e | List e | Set e -> unify elem e
  | Map (k, v)               -> unify elem (Tuple [ k; v ])
  | _                        -> false
```

A map needs no separate rule: its element type is the pair `(K, V)`, so an array of tuples is already the right shape.

## Narrowing

Anything that supplies a type narrows the variable, because this is ordinary unification:

```cronyx
var l: List<int> = [1, 2, 3];              // annotation
takes_a_list([1, 2, 3]);                   // parameter type
fn f(): List<int> { return [1, 2, 3]; }    // return type
var xs = [1, 2, 3];  xs.push(4);           // push exists only on List
```

The last is a rule rather than a coincidence. A method call on a literal that is still a collection variable looks at `Array` and every registered container declaring that method: the array wins if it declares it, a single other container narrows to it, and several is a diagnostic naming them. So an unrelated `Stack.push` does not disturb it, and the rule degrades gracefully as a standard library adds containers.

It resolves where it is written, not later, so `xs.len()` before `xs.push(4)` narrows to an array on the first line and rejects the second. Annotate when a literal is used both ways.

## Defaulting

Nothing narrowed it, so it is an **array** — the same rule that turns an unconstrained numeric variable into `int`.

```cronyx
var d = [1, 2, 3];

print(typeof(d));        // Array<int>
```

`[]` is the empty literal, and narrows the same way:

```cronyx
var results: List<int> = [];
var seen: Set<string> = [];
```

Array is the right default because it is the primitive the others are built from.

## Lowering

The literal stays a single node through type checking. A pass afterwards reads its resolved type and emits the construction — the same shape as `typeof`, which also waits for the checker and is then replaced.

Arrays are the one construction the compiler knows. Everything else is a call:

```ocaml
let lower elems (ty : Types.ty) =
  match ty with
  | Array _         -> `Array_lit elems
  | Named (name, _) -> `Call (`Var (name ^ ".of"), [ `Array_lit elems ])
  | _               -> assert false   (* the checker rejected anything else *)
```

So `[1, 2, 3]` at `List<int>` becomes a call to `List`'s `from_array`, ordinary code like any other. Adding a collection type means implementing `FromArray` for it; the lowering does not change.

The literal node lives in the parsed, desugared, and typed trees and is **gone** after lowering, so nothing downstream can receive an undecided literal.

## Consequences

**Two allocations.** `List`'s `from_array` receives a materialized array and copies out of it. Acceptable to start; the lowering can special-case known types later as an optimization rather than a requirement.

**The constructor is a trait.** A type is a candidate because it implements `FromArray`, and the impl is what registers the entry lowering calls, so the two cannot disagree — a missing constructor is a missing impl, reported where it is written.

**Set and map literals do not denote what they look like.** `[1, 1, 2]` as a `Set` has two elements, and `[("a", 1), ("a", 2)]` as a `Map` has one entry. Each `from_array` decides whether a duplicate is last-wins or an error.

**A map is not a set of pairs.** `Set<(string, int)>` holds `("a", 1)` and `("a", 2)` both, because uniqueness is on the whole element; a map's uniqueness is on the key. They are different types built from the same shape, and the annotation picks.

**Literals must never generalize.** A collection literal looks like a syntactic value but is a mutable allocation, so the value restriction has to exclude it. Otherwise `var xs = [];` followed by `xs.push(1)` and `xs.push("a")` would both check against separate instantiations.

**Narrowing can happen at a distance.** `var xs = [1,2,3];` may be a list because of a `push` several lines below, so a mismatch can be reported far from its cause.

## Deferred

Map literal sugar is deferred.
