# Remediation of builtins

What stands between `bootstrap2` and the property this design is for, and the order to fix it in.

File references are relative to `bootstrap2/lib/`, and line numbers are as of writing. [architecture](architecture.md) names the passes; the ones that recur below are `Typecheck`, `Specialize`, `Resolve` and the CPS transform, in that order.

## The property

Two halves. Every step is graded against them rather than against how badly it fails.

**A builtin can be replaced by a Cronyx definition without the replacement being weaker.** Moving `List` out of the compiler and into the prelude should change where it is written and nothing else.

**Adding one touches only where builtins are declared.** A standard library should be able to add `Set` and `Map` — both specified in [Data Structures](Data%20Structures.md) — without editing OCaml.

**The second half now holds for containers.** A type declared in Cronyx becomes one by declaring `op []` and `op []=`, with no OCaml edit — `tests/core/collections/user_container` is a `Ring<T>` that does exactly that, and `List` itself is now declared the same way. What is left of this half is elsewhere: `new T(x)` has no declaration form, and a variadic function like `print` cannot be declared at all.

**The first half does not hold.** A prelude `List` still cannot print like a container, and cannot have a constructor that performs an effect.

Methods on a builtin type *are* already replaceable, and that is the part to preserve. `impl string { fn len(self) -> int { … } }` in the prelude overrides the compiler's `string.len` with no pass edit, `tests/core/traits/builtin_receiver` declares methods on `int`, `string` and `Array<T>` from source, and `Array.contains` is now a prelude `impl Array<T>` written over `len` and `[]` rather than an OCaml entry. The gap is everything that is not a method.

## What is clean, and what only looks it

`cps.ml`, `desugar.ml`, `reflect.ml`, `monomorphize.ml`, `printer.ml`, `parser.ml`, `scanner.ml` and `ast.ml` contain no builtin name and no reference to `Registry` or `Builtins`. `interp.ml` still has no way to ask where a binding came from — it receives an environment from `bin/main.ml:136` — but it is no longer indifferent to types: it evaluates the array and string forms directly.

Three files are not clean, and the line under `Array` moved deliberately rather than being crossed by accident.

`value.ml` names `Array` at `:10`, `:57`, `:61`, `:62` and `:81` — a dedicated constructor, a hardcoded type name, a hardcoded rendering and identity equality. `interp.ml` and `verify.ml` name it too, and `types.ml:127` holds the name they share. Accepted below, deliberately: `Array` is a primitive type, not a builtin.

`typecheck.ml` names five builtin types outright. `typecheck.ml:184-188` matches `Ty_name "int" | "float" | "string" | "bool" | "unit"` as literal strings, and `types.ml:226-230` and `types.ml:288-292` reproduce the same five twice more — three sites agreeing by convention, so renaming `string` means editing two passes. `Array` is a sixth, and the only one routed through a shared constant. `typecheck.ml` also reaches around the registry to `Builtins` by module name at four sites and hardcodes `print`'s result type.

`resolve.ml` names no builtin, and no longer branches on which one is privileged.

## The shape to aim at

Stated once, because most steps are a piece of it rather than an independent repair.

**One boundary.** A registry is populated by any number of populators — the OCaml `Builtins`, the prelude, later a stdlib unit — and every pass sees a projection of it and nothing else. `Builtins` stops being a module any pass names.

**Registration is checked.** Every writer today is `Hashtbl.replace` (`registry.ml:34-43`), so a second populator that re-registers `List` wins silently. `declare_ops` at `typecheck.ml:767` already rejects a duplicate operator — the one table Cronyx can write to is the only one that checks. With three populators, load order is semantics unless registration can fail.

**An entry is a record binding a name, a signature and an implementation at one site**, so a drifted name is a build error rather than a runtime one. Seven kinds, matching the seven things a builtin can be:

```
type_entry      : { name; params : kind list
                  ; shape : Opaque | Product of fields | Sum of variants }
method_entry    : { owner; name; signature : scheme }
function_entry  : { name; signature : scheme; arity : Fixed of int | Variadic of ty }
container_entry : { entry : string; scheme : scheme }
indexed_entry   : { get : string * scheme; set : string * scheme }
constructor     : { name : string; signature : scheme }
operator        : { result : ty option; emit : Primitive | Call of string }
```

Every field exists because a pass currently assumes it. `shape` because `typecheck.ml:1477` forces every builtin type to `Opaque`, so a builtin can never be a product or a sum. A `scheme` on `method_entry` rather than a fixed `pure` because `typecheck.ml:1483` forces purity. `Variadic of ty` because `typecheck.ml:460` writes `print`'s result into the checker. A signature on every emitted name because a later pass invents one.

**Each pass gets a projection.** Typecheck: declarations, `signature_of`, operator, container, indexed, constructor — and no `Ty_name "string"`, because scalars are declarations like any other and `infer_ty_of_annotation` resolves every name through `ctx_types`. Specialize: `roots`, every name the registry can emit. Resolve: operator, container with projection and construction as data, indexed, and `signature_of` including rows; `fn_ref` and `declared_rows` disappear. Interp: an environment, plus the array forms. Verify: the array forms. Reflect, Desugar, Monomorphize, Cps: nothing.

**An empty registry is a valid configuration end to end.** It builds, and it runs any program that uses no containers, no operators and no prelude.

## Ordering

Blocks the property, then admits a wrong program, then costs a diagnostic. No step depends on another; the one ordering constraint, step 7 after step 6, lapsed before either landed and step 7 is done.

**Step 1 has landed, so the rest is no longer hypothetical.** Steps 3, 4, 8, 9 and 15 describe entry shapes that only mattered once something in Cronyx could write one, and now something can. What they buy has changed accordingly: they are about the *quality* of an entry — its signature, its element projection, its shape — rather than about whether a Cronyx declaration counts at all.

Sizes are lines of OCaml, which measures typing and not risk. Numbers are stable: a step that lands keeps its number and is marked done rather than removed, because the rest of the document refers to them.

**Most of the fixtures named below do not exist.** `tests/core/collections/` is not a directory, and the harness's `expected_failing` list keys its blockers to [Implementation Plan](Implementation%20Plan.md) steps, so most of these steps are untracked by the suite. Writing the fixture is part of the step.

## Blocks the property

| # | Step | Size |
|---|---|---|
| 1 | Registration writable from Cronyx | **done** |
| 2 | Generalize an impl's methods as a group | **done** |
| 3 | Every emitted name has a declared signature, including its row | ~90 lines |
| 4 | Container entries carry a projection and a construction | **done** |
| 5 | Scalar types are declarations, not literal strings | ~50 lines |
| 6 | Registry-emitted names are specialization roots | ~40 lines |
| 7 | `has_generic` walks type arguments | **done** |
| 8 | A variadic entry is declarable and carries a result type | half done — the rest is `print` becoming an effect |
| 9 | Typecheck reads declarations off the registry | ~60 lines |
| 10 | Registration rejects a conflict | mostly done, ~10 lines |

### 1 · Registration writable from Cronyx — done

Declaring a container entirely in the prelude — the type, its constructor, its getter and setter — used to produce something that was not a container until five lines of OCaml named it. That was the second half of the property, negated by construction.

`op` now covers the bracket forms, and **arity decides what `[]` means**:

```cronyx
op []<T>(items: Array<T>) -> Ring<T> { … }        // one operand  → builds from a literal
op []<T>(r: Ring<T>, i: int) -> T { … }           // two operands → reads
op []=<T>(r: Ring<T>, i: int, v: T) -> T { … }    // three        → writes
```

[Elaboration](Elaboration.md) already specified the indexing half; the literal half had no form, and this is it. `[]=` is kept rather than folded into a three-operand `[]` because it reads as an assignment, so the arity rule only separates the two `[]` forms and a three-operand `[]` is rejected pointing at `[]=`.

`declare_ops` at `typecheck.ml:786` writes `containers` and `indexed`, keyed by type name. Keying by *name* rather than by type is what lets an index entry be generic while a binary operator still demands concrete operands — the same registry, two keying disciplines, now stated rather than discovered. `builtins.ml` has no `register` function at all, and `Registry.register_container`, `register_index_get` and `register_index_set` have exactly one caller each, driven by Cronyx source.

Three things came out of it that were not on the plan.

**`op` declarations could not carry type parameters.** The parser accepted `op []<T>`, but the checker called `type_params_of` only for its side effect and never bound the result, so `T` was `Unknown type 'T'`. `Op_decl` now gets the `ctx_fn_params` treatment `Fn` has.

**An entry's function is the declaration**, so step 3's first defect cannot occur for these: there is no second place to name it, and `print(__op_index_Array([1, 2]))` type checks and runs rather than being `Undefined variable`.

**The mangled name has to come from what a literal entry *builds*.** Naming it after its operand made every container built from an `Array<T>` collide, and the later declaration answered for both — a `List<int>` literal returning a `Ring`, caught only because the fixture used both in one program. `Ast.op_entry_name` names a one-operand `[]` by its result and everything else by its operands.

Still open: `Registry.constructor` has no writer, so `new T(x)` remains unreachable for every `T`. Giving it a declaration form is the same exercise as this step.

| Fixture | `core/collections/user_container` — a `Ring<T>` declared in Cronyx with all three entries, used at two element types, indexed past its end and written through, alongside a `List` in the same program |

### 2 · Generalize an impl's methods as a group — done

`typecheck.ml:951` creates one variable per impl type parameter and shares it across every method. Generalizing them in turn removed only *that* method's binding before computing the free set, so its siblings still bound the shared variable monomorphically, `env_free_vars` reported it as in use, and nothing was quantified.

```cronyx
type Box<T> { v: T }

impl Box<T> {
    fn get(self) -> T { return self.v; }
    fn same(self) -> T { return self.v; }
}

var a = new Box { v: 1 };
var b = new Box { v: "s" };
```

Deleting `same` made this compile. Keeping it gave `Expected int, got string`. The prelude's `List` has four methods, so `List<int>` and `List<string>` could not coexist.

This is the trap [type-system](type-system.md) records for functions — leaving the binding in place makes the function's own variables look free — applied to `Fn` and half-applied to `Impl_decl`.

`typecheck.ml:1139` now infers every method body first, with the hoisted monomorphic bindings in place so a method can still call its siblings, then removes all of the impl's bindings, computes the free set once, and generalizes every method against it. An impl generalizes as a group or not at all.

One consequence: a method calling a sibling unifies against that sibling's hoisted type rather than a fresh instantiation of an already-generalized scheme, since nothing is generalized mid-list. For a receiver of the impl's own type these are the same, which `tests/core/traits/inherent` exercises — `describe` calls `self.area()`.

| Fixture | `core/traits/generic_impl_two_methods` — the `Box` above, plus a `List<int>` and a `List<string>` in one program |

### 3 · Every emitted name has a declared signature, including its row

Two defects, one fix.

**Nothing checks that an entry's function exists.** `typecheck.ml:1458` admits a container candidate on `Registry.container registry name <> None` and never looks the named function up. `print(__list_of([1, 2]))` is `Undefined variable '__list_of'` — the name `Resolve` emits is in no signature table — yet `Resolve` emits calls to it. `Resolve.fn_ref` invents the callee's type from the argument annotations, and `verify.ml:38` takes a `` `Var `` at its word, so the fabricated type is the only thing the call is ever checked against. A typo in a registration type checks clean and dies at run time.

The container and indexing instances are gone, for two different reasons. `__array_of`, `__array_get` and `__array_set` were the array lowering, and the array forms are now nodes the checker builds and `verify.ml` checks structurally. `__list_of`, `__list_get` and `__list_set` are now `op` declarations, and an `op`'s emitted name is derived from the declaration rather than written beside it, so there is no second place for it to drift from — `declare_ops` binds it, and calling it directly type checks. What remains is every method name, which is the same defect at a smaller radius.

**A synthesized call claims purity.** `Resolve.record` writes `declared_rows` for `Impl_decl` and `Op_decl`; its `` `Fn `` arm only recurses into the body and never records the function's own row. Every container-entry and indexing-entry function is a plain `fn`, so `fn_ref` defaults its row to `[]`. A prelude constructor that performs an effect gets an evidence parameter from CPS and a call site that passes none — a run-time arity failure. Builtin *methods* are accidentally immune because their row comes from `declared_rows`, which is this table masking the defect for one construct while creating it for another.

Fix: an entry names a function, its type and its row. Admission looks it up and unifies. `fn_ref` reads the signature instead of synthesizing, for container lowering, indexing, operators and methods alike, and `declared_rows` disappears.

This is what makes an OCaml entry and a Cronyx entry indistinguishable to the checker, which is the whole design.

| Fixture | `core/collections/effectful_entry` — a container whose `of` performs an effect, called under a handler. An OCaml-side typo cannot be fixtured without shipping a broken registration; the Cronyx-side version needs step 1 |

### 4 · Container entries carry a projection and a construction — done

Three halves, taken in three sittings.

**The privilege.** `Types.default_container` was a global ref set by `Builtins` and compared against by name in `Resolve`. Making `Array` a primitive removed both, along with the `Array<K, V>` a two-parameter target used to synthesize.

**The construction.** `op []` with one operand *is* the statement that this type is built by calling one function with one array, checked at its declaration site by arity — step 1.

**The projection**, which was the last of it. `types.ml` matched only `INamed (name, [ elem ], _)`, so `var m: Map<string, int> = [("a", 1)];` was `Expected a collection, got Map<string, int>` — unreachable in shape, not merely unimplemented, however `Map` was declared.

The fix is that an entry already carries the projection and nobody was reading it. A literal entry's signature is `(element) -> container`, so the element a target holds comes from instantiating that signature and unifying its result against the target:

```ocaml
match Types.repr (Types.instantiate c.scheme) with
| Types.IFn ([ element ], result, _) -> Types.unify result target; Some element
```

`op []<K, V>(pairs: Array<(K, V)>) -> Map<K, V>` is therefore the whole statement that a `Map<string, int>` literal holds `(string, int)`. [Collection Literals](Collection%20Literals.md) records the rule as `Map (k, v) -> unify elem (Tuple [ k; v ])`; that is now what the code does, with the rule supplied by the declaration rather than by the compiler.

`Registry.container_element` is the one implementation, used by both `typecheck.admits` and `Resolve`'s literal lowering. `Types.container_element` survives for exactly one caller — the `Array` case, which is primitive and has no entry.

Sum-shaped containers remain excluded: the target is matched as a whole, so an `ISum` result would unify, but nothing has tried it and no fixture covers it.

| Fixture | `core/collections/map` — the first two-parameter container the language has had, written entirely in the prelude |

### 5 · Scalar types are declarations, not literal strings

`typecheck.ml:184-188` matches `Ty_name "int"`, `"float"`, `"string"`, `"bool"` and `"unit"` as literal strings. `types.ml:226-230` and `types.ml:288-292` reproduce the same five in `type_name` and `infer_type_name`. `Builtins.types` is now empty, so no scalar is a registry entry — and `builtins.ml`'s `"string"` method owners resolve only because the second and third copies happen to agree with the first.

Three sites agreeing by convention is the same defect as a name in one list and an implementation in another, and it is the reason `typecheck.ml` fails the property on its face rather than only through `Builtins`.

`Array` is a sixth name, and shows what the fix should look like: it appears in `named_type` rather than in `infer_ty_of_annotation`'s literal list, and every pass that needs it reads `Types.array_name` rather than spelling it. One constant, one resolution site. That is cheaper than the scalars have it, and short of what this step asks for, which is that `infer_ty_of_annotation` resolve every name through `ctx_types` uniformly and `type_name` read the declaration rather than re-deriving it.

Whether `Array` follows the scalars into `ctx_types` or stays a resolution-site primitive is open. It is written with an argument, so it is the one that would test whether a declared scalar table can carry arity at all.

| Fixture | none directly — the check is that `typecheck.ml` contains no builtin type name outside `named_type`, and that a program annotating `int` still checks |

### 6 · Registry-emitted names are specialization roots

`bin/main.ml:121` runs `Resolve.program ~registry (Specialize.program typed)`. Every call the container lowering synthesizes is created in `Resolve`, after `Specialize` has dropped every template it collected. A Cronyx container constructor that `collect` classifies as a type-directed generic is deleted before its only caller exists, and the failure is `Undefined variable`.

Dropping a template with no call sites is correct for user code and wrong for anything a later pass will synthesize a call to.

**Fix: retain templates the registry can name, and leave the pass order alone.** Moving `Specialize` after `Resolve` is not available: step 13 needed it to run first, because it is what makes a method body's annotations concrete so an operator can be selected. It is also not a scheduling change — after `Resolve` the IR is `resolved_stmt`, with no `Method_call`, no `Collection_lit` and no `Index`, so `type_directed_expr` would be rewritten against a tree missing most of the constructors it inspects.

Retention suffices for the constructors at issue: a constructor's body indexes its backing array, which is now an `` `Array_get `` node needing no lookup at all. Operator selection is what needs concreteness, and a constructor performs none.

**Constraint on modules**, Step 10 of the [Implementation Plan](Implementation%20Plan.md). `Specialize` runs on `typed_stmt`, so an importing unit can only specialize a template it receives as pre-`Resolve` typed IR. Either a compiled unit carries the typed body of every exported generic, or specialization is whole-program. Rust ships generic MIR in rlib metadata, C++ puts templates in headers, OCaml declines to monomorphize.

| Fixture | `core/collections/constructor_survives` — a container whose constructor is generic and called only from a lowered literal |

### 7 · `has_generic` walks type arguments — done

**It was two functions with the same omission, and this document recorded one.** `has_generic` decides whether a definition is a specialization template; `match_generic` decides what its copy's types become. Both matched a `Named`'s fields and neither matched its arguments, and an opaque container carries its parameter nowhere else. Fixing only `has_generic` collects the template and then produces a copy still annotated `Generic`; fixing only `match_generic` never collects it. Both directions verified.

The case that needs them is a definition whose *only* generic occurrence is inside a container — every parameter and the return type mention `T` nowhere else:

```cronyx
fn accumulate<T>(xs: Array<T>) {
    xs[0] = xs[0] + xs[1];
}
```

At `Array<Vec2>` that was `Operator is not defined for record and record`; it now selects the declared `op +`. Two earlier attempts at a repro did not isolate it, because a generic return type or a generic parameter already makes the function type report generic — worth knowing, since it is why this went unnoticed.

**The ordering constraint against step 6 had already lapsed** before this landed, and for a reason worth keeping: patching `has_generic` used to turn `tests/core/lists/list_methods` into `Undefined variable '__list_of'`, because `__list_of` became visibly generic and `Specialize` dropped it before `Resolve` synthesized its only call. `items.len()` is an `` `Array_len `` node now, so nothing classifies it as type-directed, and the container entry is an `op` rather than a synthesized call at all.

| Fixture | `core/comptime/element_generic` — `accumulate` at `Vec2`, `int` and `string` |

### 8 · A variadic entry is declarable and carries a result type — half done

`typecheck.ml` tested the written callee name against `Builtins.variadic` and hardcoded the result as `IUnit` — `print`'s signature, written into the checker.

**The entry is data now.** `Builtins.variadic` carries a result type per name, and the checker reads it, so nothing in a pass knows what `print` returns.

**The callee resolves through scope rather than spelling.** `declare_builtins` records the scheme each variadic is bound to and a call site asks whether the name still refers to it, so a program declaring its own `print` gets its own — `types/errors/shadowed_print` pins the arity error against the user's version.

**Both consequences are closed.** A variadic's binding is `() -> unit` rather than an unconstrained variable, so `var p = print; var n: int = p;` is a type error instead of being accepted and printing `<fn print>`. And `verify.ml`'s generic-callee escape is **deleted**: once a variadic call carries a signature built from its arguments, `Types.Generic` cannot appear as a callee annotation at all — anything used as a callee unifies to an `IFn`, so `print` was the only thing that escape ever covered. Higher-order generics were checked separately and are unaffected.

**What is left is not this step's, and it is worth saying where it went.** A *prelude-declared* variadic needs surface syntax, and the syntax needs a type: a rest parameter over one element type — `fn join(sep: string, parts: string...)` — cannot express `print`, whose arguments differ in type. The alternatives are a top type, which lets every unrelated mistake unify; trait objects, which are a runtime dispatch feature; or a one-argument `print`, which costs 72 call sites across the fixtures and reads worse at every one.

**The metaprogramming answer is closed off, and that is settled rather than pending.** It looked as though `print` could become a `meta fn` expanding to `__write(str(a) + " " + str(b) + "\n")`. It cannot: a meta function's parameters take *values* — see [Metaprocessing](Metaprocessing.md#parameters-take-values) — and `print(a, b)` with a runtime `a` has no value to take. Reducing that call at compile time is not something the design declines to do; it is not possible.

**The answer is that `print` becomes an algebraic effect**, and until then it stays an entry alongside `clock` — both are I/O, which is the floor this document already accepts.

```cronyx
effect out { fn print(s: string): unit; }
```

A handler installed at the program's root writes to the right descriptor, and a test or a tool installs its own instead. That is what an effect system is for, and it is newly viable: before step 11, a function that printed could not call anything, because a call equated rows rather than containing them.

**It also settles the variadic question by removing it.** An effect operation has a declared signature, so `print` takes one string. The 72 multi-argument call sites in the fixtures become a formatting problem rather than a typing one, which is an argument for string interpolation arriving with it. A homogeneous rest parameter remains worth having on its own account, for `join` and `max`, and is unrelated.

**The cost is that printing becomes visible in a row.** `fn debug(x) { print(x); }` carries `<out>` and cannot be called from code claiming purity. That is the feature working rather than a defect, but it means the root must be handled implicitly, or every program begins with a `run`.

Homogeneous rest parameters are worth having on their own account, for `join` and `max` and `concat`, and are cheap: `...` in the parser, `Array<T>` in the checker, an `Array_lit` around the trailing arguments in `Resolve`. They do nothing for `print`.

| Fixture | `types/errors/print_is_not_a_value` and `types/errors/shadowed_print`. A prelude-declared variadic waits on metaprocessing |

### 9 · Typecheck reads declarations off the registry

`typecheck.ml:1478`, `:1484`, `:1489` and `:1501` read `Builtins.types`, `.methods`, `.functions` and `.variadic` by module name. `typecheck.ml:1477` forces every declared builtin type to `Opaque`, so a builtin can never be a product or a sum, and `typecheck.ml:1483` binds every builtin method through `pure`, so none can carry a row.

Emptying the four tables and `register` still builds and still runs a program that uses no containers — so the coupling is the dependency edge and the two forced shapes, not a compile failure. An earlier draft claimed the build breaks; that is only true of deleting the module, which is the less interesting experiment.

**The prelude and the registry are a matched pair, and nothing says so.** `bin/main.ml:97` prepends `Prelude.program ()` unconditionally. Emptying `methods` and `register` no longer breaks every program — `tests/core/arrays/basics` runs untouched, because everything it uses is primitive — but `var xs: List<int> = [1, 2];` becomes `Expected a collection, got List<int>`: the prelude declares `List` and the OCaml table is what makes it a container. A prelude type is only as good as the entries registered for it, and the two halves are written in different languages in different files with nothing tying them together. Either the prelude is conditional on what is registered, or it is part of the same populator.

Fix: `declare_builtins` becomes "install the registry's declarations", identical for an OCaml entry and a prelude entry, with `type_entry.shape` and a `scheme` on `method_entry` replacing the two forced shapes. `Builtins` becomes one of several populators called from `bin/main.ml`.

| Fixture | none — the check is that `lib/` builds and runs with every populator empty |

### 10 · Registration rejects a conflict — mostly done

Mostly done, and by accident of having a declaration site to put the check at. `declare_ops` rejects a second literal, getter, setter or binary operator for a type it already has — `A literal is already declared for Bag.` — because step 1 gave every one of them a place in Cronyx where a conflict is visible.

What remains is `register_constructor` and `register_homogeneous`, still bare `Hashtbl.replace` (`registry.ml:47`, `:53`), and the fact that the check lives in `declare_ops` rather than in the registry: a second populator calling `Registry.register_container` directly still wins silently. The check belongs one level down.

`declare_ops` at `typecheck.ml:767` explicitly rejects a duplicate operator, so the one table Cronyx can write to is the only one that checks. The moment there are three populators — `Builtins`, the prelude, a stdlib unit — load order becomes semantics.

Fix: registration returns a conflict, or the entry point rejects a redefinition the way `declare_ops` does.

| Fixture | `core/collections/errors/duplicate_container` — written and passing |

## Admits a wrong program

| # | Step | Size |
|---|---|---|
| 11 | Effect rows: containment, not equality, at call sites | **done** |
| 12 | Method receiver selection is unsound, and narrowing depends on it | **done** |
| 13 | Select operators in a specialized method body | **done** |
| 14 | An out-of-range index answers with a wrong value | **done** |
| 15 | One table per set of builtins | ~200 lines, mostly moved |
| 16 | Structural equality, plus identity and cycles | **done** |

### 11 · Effect rows: containment, not equality, at call sites — done

A call unified the callee's row with the caller's, so the callee's annotation acquired the caller's effects and the CPS transform, reading that annotation, appended evidence arguments the callee had no parameters for.

```cronyx
fn work(n: int): string {
    log("start");
    return str(n) + "!";
}
```

`str expects 1 argument(s) but got 2`, at run time. Every builtin function was on the wrong side of it, and so was every Cronyx function: a helper factored out of effectful code failed identically, which made effects unusable with any factored-out code at all. Equality also failed in the other direction, rejecting a caller that performs *more* effects than its callee — `This code does not handle the effect 'ask'`, blaming the callee for the caller's row.

`Types.row_within` requires every label in the callee's row to be in the caller's, growing an open caller row and rejecting a closed one, and never writes the caller's labels back into the callee.

**One exception, and it is load-bearing.** `admits_row` in `typecheck.ml` still ties the two rows together when the callee's row is an unbound variable *and* its scheme quantifies no rows — a forward or recursive reference sharing a row variable with its own definition, about which nothing is known yet. Without it, mutual recursion through an effectful function breaks:

```cronyx
fn mutual(n: int) { if (n > 0) { later(n - 1); } }
fn later(n: int) { log("later"); mutual(n); }
```

**Two accidental protections stopped being load-bearing.** Prelude methods survived this because their rows come from `Resolve.declared_rows` — step 3's fabrication masking this defect for one construct — and `print` survived because `typecheck.ml:460` short-circuits a variadic call without unifying rows at all, which is step 8's defect protecting against this one. Neither is needed now, so neither is an argument against fixing 3 or 8.

This diverges from [effects](effects.md#unification-not-subsumption), which records Koka's position that unification alone suffices. It does — for Koka, where evidence is decided from a function's definition. It does not here, because the CPS pass reads the *call site's* annotation, so a subsumed row is a wrong arity. [TODO](TODO.md) named both fixes; this is the first.

| Fixture | `effects/rows/builtin_in_effectful_fn` — a builtin, a user helper, a prelude method, and a caller with more effects than its callee |

### 12 · Method receiver selection is unsound, and narrowing depends on it — done

The checker picked a receiver when exactly one type owned a method of that name, and raised `Deferred` otherwise, annotating the node with a fresh variable pinned by whatever the caller did with it and by nothing else.

**It was unsound.** `fn f(x) { return x.len(); }` with `var s: string = f([1, 2, 3]);` printed `3` — an `int` bound to a `string`.

**And a documented rule rested on the same accident.** `var xs = [1,2,3]; xs.push(4);` narrowed to a `List` only because exactly one type owned `push`; declaring an unrelated `Stack` with a `push` made those two lines type check and then fail with `Undefined variable 'Array__push'`.

Three parts replaced it.

**A · Written trait bounds — built.** `fn notify<T: Summary>(item: T)`. The trait declares the signature, so `item.summarize()` has a result type without the owner being known — which is the thing inference cannot supply, because a method *name* determines nothing. The bound is a **kind**, `Types.Bound of string list`, so unification enforces it: passing a type with no `impl Summary for` it fails at the call site through the ordinary kind machinery, with no new discharge site. Dispatch stays `Specialize` copying per concrete type, so each copy calls `Article__summarize` directly — no vtables, no dictionaries.

**B · An unknown receiver is an error.** `Deferred` and its fresh variable are gone. The diagnostic names the types that do declare the method, and the "no type has this method" case stays distinct from "the receiver is not known", which `core/traits/errors/unknown_method` pins.

**C · A literal narrows by ownership.** Among `Array` and the registered containers owning the method: prefer `Array` when it owns it, take the unique one otherwise, and name them all when several remain. `Stack` is not a container, so it is filtered out and the program above works. The documented rule is a rule now rather than an accident, and it degrades gracefully as a standard library adds containers instead of monotonically worse.

**`Monomorphize` had to learn what a trait is.** It runs before anything else and reads an *annotated* comptime parameter as a value parameter, so `<T: Summary>` made it delete `notify` as an unspecialized template — `Undefined variable 'notify'`, with no other diagnostic. It now collects trait names from the program itself. That is the cost of `cp_ty` carrying two meanings, and it bites earlier than in the checker.

The two fixtures this step was predicted to cost, it cost: `list_methods` gained `: List<int>` and `trait_bound` got its `<T: Summary>` back, having had it removed when operator constraints became inferred.

| Fixture | `core/traits/errors/ambiguous_receiver` and `core/collections/narrowing` — the `Stack` program — plus `list_methods` and `trait_bound` rewritten |

### 13 · Select operators in a specialized method body — done

The constraint check was never missing: a generic `impl` method using `+` at a type with no `op +` still gives a clean `Expected int, float or string, got Foo` at the call site, from the kind mechanism in `Types.unify`. Declaring `op +` for that type is what used to fail — the call reached the interpreter as a raw `` `Binop `` and died with `Operator is not defined for record and record`.

**It had a prerequisite this document did not record, and that was the actual cause.** An impl's type parameters were created with a bare `Types.fresh ()`, while a written `fn f<T>` goes through `type_params_of`, which also calls `Types.declare_param`. An undeclared variable is defaulted by kind at resolution, so the `+` in the body put an `Addable` kind on `T` and `T` resolved to `int` — the body was monomorphized before `Specialize` ever ran:

```
(impl Pair
  (return (+ (field self:Pair<int> a):int (field self:Pair<int> b):int):int)
```

The binding still generalized, so `Pair<Vec2>` type checked and failed at run time. `typecheck.ml:956` now declares them, and the rule is written down in [type-system](type-system.md#numeric-constraints-and-defaulting) where defaulting is specified.

**Then the lowering, which needed all three sites the paragraph above predicted.** `collect` at `specialize.ml:221` registers each generic, type-directed method as a template keyed by its mangled name — a method is a function whose first parameter is the receiver, and `md_params` already includes `self`, so the template is a synthesized `` `Fn `` and the copy needs no new statement kind. `rewrite` at `specialize.ml:151` redirects a `` `Method_call `` whose receiver names an owner with a template and whose call type is concrete.

**The call type carries the template's row, not an empty one** (`specialize.ml:135`). The CPS pass reads the callee's row to decide what evidence to pass, so a synthesized empty row is a run-time arity failure on any effectful method — the same defect as step 3, reached from a different direction.

**Method templates are not dropped from their impls**, unlike `` `Fn `` templates. A dead `Pair__total` is still emitted, holding an operator nothing can select and nothing calls. Dropping it means rebuilding the `Impl_decl` without those methods, which is only safe once every call site is known to have been rewritten.

| Fixture | `core/traits/generic_impl_operator` — three instantiations, a method calling a sibling, and a declared `op ==`; plus `core/traits/errors/generic_impl_no_operator` guarding the diagnostic |

### 14 · An out-of-range index answers with a wrong value — done

The prelude answered an out-of-range index with `self.items[self.count]`, which is *in bounds* whenever the backing array has spare capacity, so it returned whatever value last grew the list.

```cronyx
var l: List<int> = [1, 2];
l.push(9);
print(l[7]);
print(l[-1]);
```

Printed `9` twice. Both the getter and the setter now reject.

**Nothing in Cronyx can raise**, so the read is aimed past the backing array and the array's own bounds check does the rejecting — a `__past_end` helper in the prelude, carrying the comment that says why the index cannot simply be passed through. That is the route this step always described: rejecting the index needs no [fallibility](#fallibility) decision, only answering *well* does. The message still names the array rather than the list, which is step 17's.

**The suite could not express this, and now can.** `error_cases` stops at `Resolve`, so a program that must be accepted and then fail while running had nowhere to live. `test_bootstrap2.ml` gained a `runtime_cases` list pairing a `.cx` with a `.rt`. A `.rt` may hold the whole `[line:col] message` or only the message — the second form because this diagnostic is raised inside the prelude, whose line number is not the user's and should not be pinned by a fixture.

| Fixture | `core/collections/index_out_of_range`, the first `runtime_cases` entry |

### 15 · One table per set of builtins

`builtins.ml` declares `string.bytes` in one list and implements it in another, with a string literal the only link, so a typo in either type checks clean and dies at run time. A record binding name, signature and implementation at one site is what OCaml's `external` and Rust's `#[lang_item]` are for.

The table is much smaller than it was: the five `Array` entries left with the type, and four of the five string methods moved to the prelude. What is left is `bytes`, `same`, `print`, `str` and `clock` — five entries, of which only one has an owner.

The primitive operator set is written three times and this step takes all three. `registry.ml:70-91` writes the whole arithmetic and comparison set against `Types.Int` / `Float` / `Str` — a second builtin table inside the module whose job is indifference. `registry.ml:57` and `registry.ml:64` hardcode which kind each operator demands and what it returns unresolved. And `types.ml:406` decides `Addable` for `IInt | IFloat | IStr` *before* consulting `extra_admits` at `types.ml:407`, so removing `Add` on `Str` from the registry leaves the kind still admitting it.

An unwrapping layer — `as_int`, `as_str`, `arg2` — belongs here, because it decides what an entry looks like.

Whether an entry's implementation exists is checked by step 3, not here.

| Fixture | none that proves the property — a third OCaml registration exercises the OCaml table. Step 1's `user_container` is the fixture that would matter |

### 16 · Structural equality, plus identity and cycles — done

`value.ml` compared arrays and records with OCaml's `==` and variants structurally, so which question `==` asked depended on which constructor the value happened to be. `Some(1) == Some(1)` held; `Point { x: 1 } == Point { x: 1 }` did not. The defect was the incoherence rather than the default.

It had stopped being theoretical: `Set<T>` and `Map<K, V>` are prelude code whose membership and key lookup are `==`, so a `Set<Point>` kept duplicates and a `Map<Point, _>` treated equal keys as distinct. Both are correct now, and `core/operators/structural_equality` pins them.

**`equal_with` is structural and written out**, because OCaml's own comparison raises on functional values.

**Cycles terminate.** A pair of values already under comparison counts as equal — the coinductive reading rather than a shortcut. Nothing can construct a cycle today, so this is groundwork: graphs and parent pointers will not be a semantics change.

**Identity survived as `same(a, b)`**, a builtin rather than an operator, matching `ptr::eq` rather than OCaml's `==`/`=` pair. One decision the plan did not specify: **`same` on scalars is equality.** A scalar cannot be mutated, so nothing can tell two equal ones apart — there is no identity to ask about, and `same(1, 1)` answering `false` would have described OCaml's boxing rather than Cronyx. On arrays, records, functions and the aggregates holding them it is address comparison.

It does not depend on step 15, as an earlier draft said: `same` is an ordinary entry in `Builtins.functions`, and step 15 will restructure it with everything else.

**The fixture was rewritten before the semantics changed**, in that order. `core/arrays/identity` no longer pins `print([1] == [1])` to `false`; it aliases through `ys[1] = 77`, separates `same` from `==`, and shows `a == b` going false when `b` is mutated — the property that actually matters, and one the old fixture could not express.

| Fixture | `core/operators/structural_equality`, and `core/arrays/identity` rewritten rather than re-expected |

## Costs a diagnostic

| # | Step | Size |
|---|---|---|
| 17 | Give the prelude a filename and file-relative spans | **done** |
| 18 | Reconcile the one-argument array constructor | ~30 lines, or a doc change |
| 19 | Per-unit compiler state | only for separate compilation |

### 17 · Prelude identity — done

Two halves, and the first arrived as the module prerequisite.

**Spans carry a file.** `Ast.span` gained one, threaded through the *token* rather than the parser, so no `span_of_token` call site changed. `Ast.locate ~entry` prints a bare `[3:5]` while a span is in the unit being compiled and `[lib.cx 2:12]` once it is not, rendered relative to the entry's directory — which is why the 49 existing `.err` fixtures were untouched. `bin/main.ml` and the harness both printed line and column directly, so wiring them through `locate` is what made any of it visible.

**The prelude is named.** It scans as `<prelude>` — not a path, because a diagnostic pointing at a file the user does not have is worse than one that says where it came from. An out-of-range index inside `List` now reads `[<prelude> 34:27]` rather than claiming a line in the user's file. It is still a string in `bin/main.ml` rather than a unit the loader reads; the name is what the diagnostics needed.

**Generated names are unforgeable.** `Ast.generated` joins with `#`, a character the scanner cannot produce in an identifier, and backs method names, operator entries, specialization and comptime copies, CPS evidence and continuations, desugared temporaries, and a module's prefix. The collision was reachable and was verified before the change: a user's `fn List__len` produced four type errors *inside the prelude*, which the new filename had just made legible. It now runs.

What is left is display rather than correctness — `typeof` prints `geom#Point` — and that belongs with the `typeof` rework.

| Fixture | `core/modules/errors/imported_type_error`, which pins that an error inside an imported unit names its file |

### 18 · Reconcile the one-argument array constructor

[Data Structures](Data%20Structures.md) shows `var zeros = new Array<int>(16);` with `zeros[0]` printing `0`. The array constructor takes a length and a fill, so that program is `An array takes a length and a fill value, but 1 argument(s) were given`.

One of the two is wrong. A one-argument form needs a zero value per element type, which is a rule nothing in the language has and which records and sums make a real design question. The alternative is to change the document to the two-argument form.

Listed here because it is a documented feature that does not exist, not a defect in something that does.

### 19 · Per-unit compiler state

`Types.extra_admits` is a closure ref set inside `Typecheck.check`; `ctx_types`, `ctx_traits`, `ctx_methods`, `ctx_type_params` and `Resolve.declared_rows` are module-level. `Types.reset` clears none of them.

**Modules did not need this.** Whole-program compilation means one program and one set of tables, so the state is correct rather than merely tolerated — see [Modules](Modules.md). What still wants it is *separate* compilation, and metaprocessing if a meta block ever compiles a second unit in the same process. `declared_rows` disappears in step 3.

## Accepted, and stated as such

**`Array` is a primitive type, not a builtin, and is not subject to the property.** [Collection Literals](Collection%20Literals.md) says it is the primitive the others are built from, and something has to be a contiguous mutable block. Every language keeps one — GHC's primops, Rust's `[T; N]` over raw pointers, Java's bytecode array ops.

It was previously half in and half out: `value.ml` hardcoded the representation while the type pretended to be a registry entry, which cost a type declaration, a container entry, an indexing entry, a constructor entry, two method entries, five environment values and a global ref naming it the default container. It is now one line in `Ast` — `` `Array_lit ``, `` `Array_new ``, `` `Array_get ``, `` `Array_set ``, `` `Array_len ``, carried from the checked IR through the CPS one — evaluated by `interp.ml` and checked by `verify.ml`. `builtins.ml` does not mention it.

The line this draws: a *primitive* is what no Cronyx definition could express, and it is spelled as syntax the compiler owns. A *builtin* is a library entry that happens to be written in OCaml, and it goes through the registry. `Array`'s `contains` moved to the prelude under the same rule, because a loop over `len` and `[]` is expressible.

**`string` is the same, and its representation is what made that possible.** `Value.Str` is an array of scalar values rather than an OCaml byte string, so a literal decodes once at `parser.ml:418` and indexing is constant time and character-indexed. `` `Str_get `` and `` `Str_len `` are the intrinsics — no setter, because a string is immutable — and `chars`, `contains`, `split` and `trim` are all prelude code written over them.

What survives is `bytes`, the UTF-8 encoder, and it is the boundary rather than a gap: `print` writes real octets and something has to produce them. Writing the encoder in Cronyx is possible but not sensible — the language has no bitwise operators, so it would be division and multiplication faking shifts and masks, thirty lines of subtle code replacing three, in the one place where being wrong is silent.

**Rendering is not accepted, and an earlier draft filed it as though it were.** `print(xs)` gives `[1, 2]` for an `Array<int>` and `{ items: [1, 2], count: 2 }` for a `List<int>`, because both render through `Value.string_of_value` and a `List` is a record. `value.ml:57` also reports `array` where the type system reports `Array`. A replacement that cannot print like the thing it replaces is a demotion the property forbids, so this belongs with printing through a declared interface. That is no longer blocked: [method constraints](#method-constraints) are decided, and what remains is a bound on a container's element and a `ToString` to write.

**Encapsulation is deferred to modules.** `List.items` and `List.count` are public, so `l.count = 99` makes `l.len()` return `99`. Visibility is a module-system feature everywhere it exists — `pub`, `.mli`.

**A third implementation of the operator table is the price of the backend.** Step 15 consolidates the two in the compiler. `interp.ml` implements primitive arithmetic as hand-written arms, and routing it through the builtin table would put a closure call on every integer addition in a tree-walker that is already slow. The LLVM backend will need its own; the cost is paid there.

## Open questions

Neither is scheduled work and neither is decided here.

### Method constraints — decided

**Methods take written bounds; operators stay inferred.** `fn f<T: Summary>(x: T)` uses the traits the language already has, and a method call on `x` resolves through the trait's declared signature. Step 12 has the implementation.

The asymmetry was the objection to this, and it is the point rather than a cost. An operator's meaning is unique per operand pair and `Registry.find` answers it, so the constraint can be inferred and discharged without the author saying anything. A method *name* determines nothing — many types declare `len`, with different signatures — so there is nothing to infer *to*, and the trait is what supplies the missing information. Requiring it is requiring what is actually absent. [Comptime Params](Comptime%20Params.md) rules out written bounds for operators, and that still holds.

**What a literal narrows to** — the part none of the candidates addressed — is answered separately and structurally: among `Array` and the registered containers owning the method, prefer the array, then the unique one, then say which several declare it. A method requirement is not a constraint carried around; it is resolved where it is written.

Two of the three candidates are therefore rejected on the record. *Infer it* cannot supply a result type. *Structural bounds* would express "has a method `m`" without saying which `m`, which is the same gap, and cut against `impl` being nominal for no gain.

**Still open: a bound on a container's element.** `Set<T>` and `Map<K, V>` want `T: Hash`, and printing through a declared interface wants `ToString`. That is this mechanism one level down, and the machinery to check it exists — a kind on the element variable, discharged the same way. What is missing is the decision about where such a bound is written: on the type declaration, on the method that needs it, or inferred from the prelude's use of `==`. Writing `HashMap.get` under each is still what would settle it.

### Fallibility

There is no `panic`, no `Option`, no `Result`, and the effect machinery is untouched by every builtin — which is why step 14 exists.

**Effects.** `tests/stdlib/fallible` and `tests/stdlib/error` were written this way. It is the mechanism the language has and the only one costing nothing where a call cannot fail. Step 11 is a prerequisite: until rows compose, making indexing effectful makes every indexing call crash.

**`Option` / `Result` in the prelude.** Works today. Puts a `match` at every index, which is why no language with indexing does it for indexing.

**A panic that is a real construct.** Cheapest, honest about current semantics, not composable.

What turns on it: every prelude and library function that can fail. What would settle it: deciding whether indexing out of range is recoverable.
