# Implementation plan

Ordered work for `bootstrap2`, with the fixtures each step should light up and the ones the design has invalidated.

`bootstrap2` currently passes 48 cases: scalars, functions, control flow, effects, and `typeof`. Everything below is designed and unbuilt.

## Order

### 0 · Pipeline restructure

No new language features. Introduce the registry as a value threaded through the compiler, move the builtin operator rules out of the checker's hardcoded arms and the interpreter's match into it, add the `Resolve` pass that reads it, and move `+=` and `++` out of `Desugar` so they reach the checker intact. Add tree verification after every node-constructing pass.

This is the only step that touches working code without adding capability, which is why it goes first — every later step registers into a table that has to exist.

| Fixtures | |
|---|---|
| Should still pass | all 48, unchanged |
| Newly passing | none |
| Needing change | none |

### 1 · Arrays

`Array<T>`, the `[1, 2, 3]` literal defaulting to it, indexing, index assignment, `len`, and `new Array<int>(n)`. Indexing and the literal are the first real registry entries.

| Fixtures | |
|---|---|
| Newly passing | `core/lists/index_access`, `core/lists/index_assign` |
| Needing change | `core/slices/*` — negative indexing and range slicing are not in the design |

### 2 · Tuples

`(a, b)` and positional access. Small, and the map literal depends on it later.

| Fixtures | |
|---|---|
| Newly passing | `core/tuples/tuple_basic`, `reflection/typeof_tuple` |

### 3 · Records and product `type`

Structural records over the existing row machinery, `type Point { … }` for nominal ones, `new Point { … }`, field access and field assignment.

| Fixtures | |
|---|---|
| Newly passing | `reflection/typeof_record` |
| Needing change | `core/structs/*` — `struct` → `type`, and literals gain `new` |

### 4 · Sums and `match`

`type Color { Red, Green }`, variants carrying tuples or fields, and `match` with binding patterns.

| Fixtures | |
|---|---|
| Newly passing | `reflection/typeof_enum` after rewrite |
| Needing change | `core/enums/*` — `enum` → `type`, constructors gain `new` |

### 5 · Reify

The structural value-to-syntax walk. Deferred: it has no producer and no consumer on its own, and step 6 turned out not to need it — see there. It becomes real with metaprocessing, where a compile-time value that was computed rather than written has to be turned back into code.

| Fixtures | |
|---|---|
| Newly passing | none directly — exercised through step 11 |

### 6 · Comptime params and monomorphization

`<>` parsing, specialization per distinct argument, inferred operator constraints, and `Generic` finally getting a consumer.

Type parameters and value parameters are handled in different places, which is what the two checking regimes in [Comptime Params](Comptime%20Params.md) come to in practice. A type parameter is a variable the checker generalizes, so one copy serves every use. A value parameter is substituted before checking, by a pass over the desugared tree, because a value can decide a type — so the copy is what gets checked.

Reify turned out not to be needed for this: a written comptime argument is already syntax, and forwarding one substitutes a literal for a name. It becomes necessary when an argument is a *computed* compile-time value, which is metaprocessing.

A generic body is copied per concrete type it is called at, because an operator or a method inside one cannot be selected while it is generic. Only bodies containing such a construct are copied; a generic function that merely moves values around is served by generalization from a single copy, as before.

Constraints are inferred rather than written, so `fn sum<T>` needs no bound: the `+` in its body is what requires an `op +` at the instantiating type, and the requirement is checked when the copy is made. A written `T: Add` would name a predicate over the operator table as though it were declarable.

| Fixtures | |
|---|---|
| Newly passing | `meta/params/func`, `core/traits/trait_bound` — the latter with its `<T: Summary>` dropped, since the constraint is now inferred |
| Needing change | `core/generics` → `core/comptime`, off `struct`; `type_reuse` needs an annotation, since `.0` on an unannotated parameter never had a type to read; `traits/errors/unknown_receiver` became `unknown_method`, because a generic receiver is no longer an error |

### 7 · Operator declarations

`op +(a: Vec2, b: Vec2)`, now that there are user types to overload on. Registry entries from source rather than only builtins.

| Fixtures | |
|---|---|
| Needing change | `operators/*` — `impl Add for Vec2` → `op +`, since traits are gone |

### 7b · Methods

`trait` declares signatures, `impl [Trait for] Type` supplies bodies, and `x.m(a)` dispatches on the receiver's type. Each method compiles to a function named `Type__method` taking the receiver first, so the interpreter sees ordinary calls. Dispatch needs the receiver's type at the call site: a method on a value whose type is still a variable has no answer until monomorphization, so `fn notify(item) { item.summarize(); }` is an error until step 6.

| Fixtures | |
|---|---|
| Needing change | `core/traits/*` — `struct` → `type`, literals → `new`, `to_string` → `str` |
| Still blocked | `core/traits/trait_bound` — needs inferred method constraints, which arrive with step 6 |

### 8 · List, Set, Map

`List<T>` with `push`, `Set<T>`, `Map<K, V>`, and literal narrowing across all four containers.

| Fixtures | |
|---|---|
| Newly passing | much of `stdlib/collections` once it is rewritten |

### 9 · Iteration

`for (x in xs)`, resolved by the iterable's type, which puts it in `Resolve` rather than `Desugar`.

| Fixtures | |
|---|---|
| Newly passing | `core/for_tuple` |
| Still blocked | `effects/logic/*` — also needs resume inside a loop |

### 10 · Modules

`import`, and the rule that a meta block sees imported definitions but not imported runtime values.

| Fixtures | |
|---|---|
| Newly passing | `core/modules/*` |

### 11 · Metaprocessing

`meta`, `gen`, the dependency graph, the worklist, and recursive compilation with the evaluator supplied.

| Fixtures | |
|---|---|
| Newly passing | `meta/execution/*`, `meta/codegen/*`, `meta/functions/*` |
| Needing change | `meta/derive/basic` — declares its `meta fn derive` inside a `trait` |

### 12 · `typeof` yields a `Type`

Only possible once records and sums exist, since `Type` is one. Every existing `typeof` fixture prints a string today.

| Fixtures | |
|---|---|
| Needing change | all of `reflection/*`, depending on how a `Type` prints |

## Fixtures the design has invalidated

Independent of ordering, these no longer describe the language:

| What changed | Where | Count |
|---|---|---|
| `struct X { }` → `type X { }` | `core/structs`, `core/generics`, `types/gadt` | 15 files, 10 in stdlib |
| `enum X { }` → `type X { }` | `core/enums`, `types/gadt` | 9 files, 2 in stdlib |
| `trait` and `impl` removed | `core/traits`, `operators`, `meta/derive` | 13 files, 33 in stdlib |
| Construction takes `new` | every struct and enum literal | with the above |
| `[T]` → `Array<T>` in annotations | anywhere a sequence is annotated | mostly stdlib |
| `typeof` prints `Array<int>`, not `[int]` | `reflection/typeof_slice` | 1 |

The trait removal is the largest single item, and most of it is `stdlib/ops/*` — fourteen files that exist only to declare operator traits, which `op` declarations replace outright.

## Not covered by the design

`core/slices`, `core/defer`, `core/embed`, and `core/resolution` exercise behaviour no document describes. Each is either a feature to design or a fixture to retire — see [TODO](TODO.md), where all four are recorded with what deciding them would involve.
