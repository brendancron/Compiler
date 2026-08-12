# Builtins audit

An audit of how `bootstrap2` supplies what a program can use without declaring it, and of what a standard library written on top of it would run into. Findings were confirmed against the compiled binary, not read off the source.

Audited at `4697dc4` plus the uncommitted `builtins.ml` / `prelude.ml` / `value.ml` split.

## Grade: C+

The abstractions are the right ones. The registry is a real table rather than a fallthrough, the prelude is Cronyx rather than OCaml, `op` declarations plumb into constraint inference correctly, and `Verify` exists because someone thought about annotation drift. That is a better foundation than most compilers have at this point.

What holds the grade down is that four of the ten findings below are *silent*: the checker accepts the program, and the failure arrives at run time with a message pointing somewhere else. Findings 1 and 2 are miscompiles. Finding 3 makes a hash map that cannot find its keys. All four get worse in proportion to how much standard library exists, and three of them live in exactly the construct a collections library is made of — a generic `impl` method that compares its elements.

The plan is right that modules are the biggest unlock. The argument this document makes is about what should go *before* them.

## What a builtin is

Six unrelated mechanisms sharing one flat namespace.

| | Mechanism | Type checked | Implemented in |
|---|---|---|---|
| 1 | Registry operator entries (`registry.ml:70`) | yes | hand-written arms in `interp.ml:10` |
| 2 | `Builtins.types` — opaque declarations (`Array`) | n/a | — |
| 3 | `Builtins.methods` / `Builtins.functions` | yes | `Builtins.values` |
| 4 | Registry lowering entries (`__array_get`, `__list_of`, …) | **no** | `Builtins.values`, or the prelude |
| 5 | `Builtins.variadic` (`print`) | **no** | `Builtins.values` |
| 6 | `Prelude.source` — Cronyx text in an OCaml string literal | yes | itself |

Three of the six are keyed by string with nothing verifying the key resolves.

## Findings

Ordered by how quietly they fail.

### 1 · Method dispatch is a name-uniqueness heuristic, and it miscompiles

`typecheck.ml:508` — when a receiver's type is unknown, the checker scans `ctx_methods` for types owning a method of that name. Unique wins; several means `Deferred`, and the result becomes a fresh type variable. `len` already has three owners.

```cronyx
fn describe(x) { return x.len(); }
print(describe("hello"));
```

```
Runtime error: Undefined variable 'describe'.
```

It type checks. `--dump-types` shows the call annotated `describe : (string) -> '75` — the deferred method left the return unpinned, so `specialize.ml:134` will not rewrite the call site, and `specialize.ml:194` drops the definition regardless. A function is deleted from the program and the deletion is reported as an undefined variable.

Adding `map`, `get`, `push`, and `to_string` across a dozen container types makes ambiguity the common case rather than the corner. Every language with this shape puts a bound on the parameter — Rust trait bounds, Haskell classes, Swift protocols, Go interfaces. Inferred constraints answer this for *operators*, because the registry can be consulted at the copy site. Nothing answers it for *methods*, and `Hash` and `ToString` in `stdlib/ops/` are precisely the two cases that need it.

This is the finding with no design anywhere.

### 2 · `impl` methods are exempt from the checking `fn` gets

`specialize.ml:176` matches `Fn` and the statement forms that contain one. `Impl_decl` falls into `_ -> ()`, so a generic method is never copied per instantiation and its constraints are never checked. The same body gets two regimes:

```cronyx
fn twice<T>(v: T) -> T { return v + v; }
```
at a type with no `op +` — a clean type error, naming the type.

```cronyx
impl Box<T> { fn twice(self) -> T { return self.v + self.v; } }
```
at the same type — accepted, then `Runtime error: Operator is not defined for record and record.`

The regime that works is not the one a standard library is written in.

### 3 · Structural equality is reference equality, silently

`value.ml:80` compares `Array` and `Record` with OCaml's `==`, while `Variant` at `value.ml:84` compares structurally. So `Some(1) == Some(1)` holds and `[1, 2, 3] == [1, 2, 3]` does not, nor does `Point { x: 1, y: 2 } == Point { x: 1, y: 2 }`.

`registry.ml:88` registers `Equal` as homogeneous over *every* type. The checker therefore promises `==` works everywhere while the runtime quietly means two different things by it. `prelude.ml:61` — `List.contains` — inherits the wrong one and returns `false` for equal records. `stdlib/collections/HashMap.cx:44` is `map.keys[s] == key`, which will never find a non-primitive key.

A user `op ==` does override it. But Rust makes you *derive* `PartialEq`; here the wrong answer is the free one, and `derive` is Step 11.

### 4 · `print` is a hole in the type system, and printing has no seam

`typecheck.ml:446` special-cases `print` **by name**, and `builtins.ml:35` binds it to a bare unconstrained type variable. Three consequences:

- `var p = print; var n: int = p;` is accepted.
- The name-keyed check ignores scope, so `fn print(x: int) -> int` does not shadow it. What comes out is `Verify error: A call should be int but is annotated unit` — an internal invariant failure surfaced as a user diagnostic.
- `print` and `str` both render through `Value.string_of_value` (`value.ml:60`), so a type can never control its own output. `stdlib/ops/ToString.cx` exists and is wired to nothing.

Every language of this shape routes printing through one overridable interface: `Display`, `Show`, `Stringer`, `__str__`. Separately, the variadic list is compiler-private, so the standard library can never add a variadic function of its own.

### 5 · Nothing cross-checks the six tables

`builtins.ml:15` declares `Array__len`. `builtins.ml:113` implements it. A string literal is the only link between them, and a typo in either produces a runtime `Undefined variable` after a clean type check.

`__array_get`, `__array_set`, and `__array_of` appear in `Builtins.values` and in the registry but in neither `methods` nor `functions` — they are never type checked at all. `Resolve` trusts the type it derived.

OCaml's `external` and Rust's `#[lang_item]` bind name, signature, and implementation at a single declaration site for exactly this reason.

### 6 · The prelude has no identity

**Spans.** `main.ml:97` is `Prelude.program () @ program`. Prelude spans are prelude-relative and carry no filename, so anything originating there reports as though it were the user's file. `l[7]` on a `List` prints `[13:16] Index 3 is out of bounds for length 3` — wrong index, wrong length, wrong file.

**Namespace.** Prelude declarations land in the same flat global scope as the program's. `fn Array__len(v: int)` collides head-on with the mangled builtin. `Ast.method_name` is `%s__%s` (`ast.ml:440`) and `Specialize` appends `__%d` (`specialize.ml:113`): three name spaces, one identifier space, no reserved prefix.

**Encapsulation.** `List.items` and `List.count` are public. `l.count = 99` makes `l.len()` return `99`. Every standard library type's representation is its public API permanently. Rust has `pub`; OCaml has `.mli`.

### 7 · Errors have no representation, so the prelude fakes them

`prelude.ml:19` signals an out-of-range index by doing `return self.items[self.count]` to provoke an array panic. There is no `panic`, no `Option`, no `Result` — and the effect machinery, the one thing that could express fallibility, is untouched by every builtin. `tests/stdlib/fallible` and `tests/stdlib/error` show the intended answer. Nothing in the builtin layer is written against it.

### 8 · The registry has the right shape and no surface syntax

`builtins.ml:48` registers `List` as a container and as indexable — in OCaml, for a type declared in Cronyx. `op` declarations reach the operator table; nothing reaches `containers`, `indexed`, or `constructors`. So `[…]` and `[i]` remain privileges of blessed types, where `Index`/`IndexMut`, `FromIterator`, and `__getitem__` are ordinary declarations.

[TODO](TODO.md) already records that indexing keys on the container and takes the element to be its sole type argument, which means `HashMap` cannot be indexed at all under the current entry shape.

### 9 · Duplicated tables

`registry.ml:70` says which `(op, ty, ty)` triples exist. `interp.ml:10` re-implements the same table in OCaml with its own fallthrough, and nothing enforces the correspondence. [Elaboration](Elaboration.md) says builtin arithmetic is not a special case in the compiler; that is true of the checker and false of the interpreter, where `Registry.Primitive` means "a hand-written arm exists somewhere else."

Adding `%` touches `token.ml`, `scanner.ml`, `ast.ml`, `parser.ml`, `registry.ml`, and `interp.ml`.

### 10 · Two smaller ones

**Global mutable compiler state.** `Types.default_container` is a `string ref` mutated by `Builtins.register`; `Types.extra_admits` is a function ref; `ctx_types`, `ctx_traits`, `ctx_methods`, and `ctx_type_params` are module-level tables, as is `Resolve.declared_rows`. Correct for one program per process. Step 10 compiles several units, and Step 11 has compilation calling itself.

**No unwrapping layer.** Roughly two fifths of `Builtins.values` is `| _ -> Value.fail span "Cannot apply %s to these arguments."`, branches unreachable if the checker is right. `as_bool` lives in `interp.ml:5`, `elements` in `builtins.ml:68`, and every other builtin re-derives the rest inline. A set of `as_str` / `as_int` / `arg2` combinators collapses most of the file. This is the finding that decides whether adding a hundred builtins is mechanical or miserable.

## What this means for the standard library

`stdlib/` is 2085 lines across 33 files, written against the Rust bootstrap. Against `bootstrap2` today:

| Needs | Status |
|---|---|
| `import` | Step 10 — blocks all 23 `tests/stdlib` fixtures |
| `struct`, `enum`, `trait … impl … for` | syntax removed; ~33 files to rewrite, already counted in the plan |
| `%`, string indexing, `ord`, file I/O | Unplanned; `HashMap` needs the first three |
| `Hash`, `ToString` as constraints | no mechanism — finding 1 |
| Trailing lambdas | Unplanned |

Modules are correctly identified as the largest single unlock. But findings 1, 2, and 3 are what make the work *after* modules unpleasant rather than merely long: they let bad programs through and fail at run time with messages pointing at the wrong line of the wrong file. Writing `stdlib/collections` on top of them means debugging a hash map that cannot find its keys, at a prelude line number that reads as a user line number.

Suggested order, smallest first:

1. **One builtin table** — name, signature, implementation — with the three views derived from it and a startup assertion that the sets agree.
2. **Structural equality for aggregates, or no default `==` at all.** Reference equality as the free default is the worst of the three available options.
3. **Run `Specialize.collect` over `Impl_decl` methods**, so a generic method is checked the way a generic function already is.
4. **Give the prelude a filename and file-relative spans**, before it grows.
5. **Decide the method-constraint story.** It is the one finding here with no design, and the standard library is what turns it from a corner case into the common path.

## Plan to remediate

Ordered by how quietly a finding fails, then by cost. Each step names what it closes and the fixture that would prove it, so the suite records the fix rather than a commit message.

Steps 1 to 4 are the ones that should land before more standard library is written on top of them. The rest can follow modules.

### 1 · Stop `Specialize` deleting what it did not rewrite

Closes half of finding 1. `specialize.ml:194` drops a generic template unconditionally, including when a call site was left alone because its type never resolved. A deleted function reported as an undefined variable is the worst possible diagnostic for it. Dropping only templates whose every call site was rewritten, and reporting the ones that were not, turns a miscompile into an error at the call site that could not be resolved.

Cheap, and independent of the design question in step 4.

| Fixture | `comptime/errors/unresolved_method` — `fn describe(x) { return x.len(); }`, rejected where the receiver is decided |

### 2 · Run `Specialize` over `impl` methods

Closes finding 2. `specialize.ml:176` matches `Fn` and the statement forms containing one; `Impl_decl` falls into the catch-all. `type_directed` already handles `Impl_decl`, so the walker was written for this and the registration was not.

Until this lands the prelude is written in the regime that is not checked, which is the same regime the collections library will be written in.

| Fixture | `comptime/errors/impl_constraint` — a generic `impl` method using `+` at a type with no `op +`, rejected at the call site the way `fn` already is |

### 3 · Structural equality for aggregates

Closes finding 3. `value.ml:80` compares arrays and records with `==` while variants compare structurally, so which meaning `==` has depends on which constructor the value happens to be. The checker promises one operator over every type; the runtime provides two.

Identity still matters — `var ys = xs` aliases, and mutating through one is visible through the other — but that is what identity is for, not what `==` should mean. A user's `op ==` continues to override.

| Fixture | `core/operators/structural_equality` — arrays, records and nested combinations; and `List.contains` finding a record |

### 4 · Decide the method-constraint story

Closes the rest of finding 1, and it is the one finding with no design anywhere. `typecheck.ml:508` picks a receiver when exactly one type owns a method of that name — a heuristic that is already false for `len` and gets worse with every container added.

Two shapes are available. Infer the requirement, the way operator constraints are inferred: record *has a method named m with this signature* against the type variable and discharge it when the copy is made. Or require it to be written, which is what every language of this shape does and which [Comptime Params](Comptime%20Params.md) explicitly rules out for operators.

The operator half works because the registry can be consulted at the copy site. A method table keyed by `(type, name)` can be consulted the same way, so the inferred shape is reachable — the work is carrying the requirement through generalization and instantiation, which `quantified_rows` and `quantified_fields` already demonstrate.

`Hash` and `ToString` are the two cases the standard library needs first.

| Fixture | `core/traits/inferred_method_constraint` — the same method name on several types, dispatched correctly through a generic function |

### 5 · One builtin table

Closes finding 5 and the compiler half of finding 9. A record carrying name, signature and implementation, with `Builtins.types` / `methods` / `functions` / `values` derived from it, and a startup assertion that every name the registry emits exists. `__array_get` and its siblings are type checked for the first time.

This is what makes adding a builtin one edit instead of three that must agree by convention.

| Fixture | none — an assertion at startup, and the existing suite proves nothing regressed |

### 6 · An unwrapping layer

Closes the second half of finding 10. `as_int`, `as_str`, `arg1`, `arg2` combinators collapse the two fifths of `builtins.ml` that is unreachable argument-shape failure. Worth doing before the table above rather than after, since it decides what an entry looks like.

### 7 · Give the prelude a filename

Closes finding 6. Spans need a source identity before there are two sources; today a prelude error reports at a prelude line as though it were the user's file. A reserved prefix for compiler-generated names — `Ast.method_name`, `Specialize`'s copies — closes the collision half.

Do this with modules, which is when a third source appears anyway.

### 8 · Printing through a declared interface

Closes finding 4. `print` special-cased by name in the checker is a hole — it ignores scope, so a user's own `print` produces a `Verify` failure rather than a shadowing error — and `Value.string_of_value` gives a type no way to control its own rendering. `stdlib/ops/ToString.cx` is the intended answer and is wired to nothing.

Needs step 4 first: `ToString` as a constraint is exactly the mechanism that does not exist yet.

### 9 · Surface syntax for the registry

Closes finding 8. `op` declarations reach the operator table; nothing reaches `containers`, `indexed`, or `constructors`, so `[…]` and `[i]` are privileges of types blessed in OCaml. [Elaboration](Elaboration.md) already gives indexing the form it should take — `op [](r: Ring<T>, i: int)` — and [Collection Literals](Collection%20Literals.md) leaves the literal case as the `of` convention, which is a naming rule rather than a table entry and should be reconsidered.

### 10 · Fallibility, and per-unit compiler state

Finding 7 needs `Option`, `Result` or effects before the prelude can stop provoking array panics to signal a bad index; the effect machinery is the one mechanism that could already express it and no builtin uses it.

The global tables in finding 10 are correct for one program per process and wrong for Step 10, which compiles several units, and Step 11, where compilation calls itself. Fix them when the second unit exists, not before.
