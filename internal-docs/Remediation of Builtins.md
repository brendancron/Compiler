# Remediation of builtins

What is wrong with how `bootstrap2` supplies what a program can use without declaring it, and the order to fix it in.

Ordered by how quietly a defect fails, then by dependency, then by cost. Everything above the line lets a wrong program through; everything below it produces a diagnostic or an inconvenience. Dependency is a real term and not a hedge: step 4 adds `same` as a builtin, which is step 3's table, so 3 precedes 4 despite costing more.

Two items are not on this list. Method constraints and fallibility are design questions rather than scheduled work; they are stated as [open questions](#open-questions) below, with the options and the criterion for choosing, and deliberately not decided here.

Sizes are lines of OCaml, which measures typing and not risk. Step 3 is the largest and the safest — mostly text moved under an assertion. Step 4 is the smallest that can break a passing fixture, and does.

## Silent — a wrong program is accepted

| # | Step | Size |
|---|---|---|
| 1 | Stop `Specialize` deleting what it did not rewrite | ~15 lines |
| 2 | Register `Impl_decl` in `Specialize.collect` | ~10 lines |
| 3 | One builtin table | ~150 lines, mostly moved |
| 4 | Structural equality, plus identity and cycles | ~40 lines, and a fixture reversed |

### 1 · Stop `Specialize` deleting what it did not rewrite

`specialize.ml:194` drops a generic template unconditionally, including when a call site was left alone because its receiver never resolved. The function disappears and the failure reads `Undefined variable`. Dropping only templates whose every call site was rewritten, and reporting the rest, turns a miscompile into an error at the call site that could not be resolved.

Independent of which option the [method-constraint question](#method-constraints) takes, so it should not wait on that being settled.

"Every call site was rewritten" is vacuously true of a template with no call sites, so an uncalled generic is still dropped. That is correct while a program is one unit — the body names types nothing can select on, so dropping dead generic code is the only thing to do with it, and it is what makes an uncalled `fn unused<T>(v: T) -> T { return v + v; }` compile clean today.

It is not correct once a generic can be exported, and the repair does not belong to this step. `Specialize` runs on `typed_stmt`, before `Resolve` and CPS, so an importing unit can only specialize a template it receives as pre-`Resolve` typed IR. Retaining the template in the exporting unit does not supply that — it leaves an unspecializable body to reach `Resolve`, which fails at `resolve.ml:96` whenever a receiver is a bare type parameter. What crosses a module boundary is a question about what a compiled unit contains, and every language that monomorphizes answers it there: Rust ships generic MIR in rlib metadata, C++ puts templates in headers, OCaml declines to monomorphize at all.

**Constraint on Step 10, recorded here because this is where it was found.** Either a compiled unit carries the typed, pre-`Resolve` body of every exported generic, or specialization is whole-program and no unit drops a template before every unit's call sites are known. Choosing neither reintroduces the silent deletion this step exists to remove, one milestone out.

| Fixture | `comptime/errors/unresolved_method` — `fn describe(x) { return x.len(); }` rejected where the receiver is undecided |

### 2 · Register `Impl_decl` in `Specialize.collect`

`specialize.ml:176` matches `Fn` and the statement forms containing one; `Impl_decl` falls into the catch-all, so a generic method is never copied per instantiation and its constraints are never checked. A generic `fn` using `+` at a type with no `op +` is a clean type error naming the type; the identical body inside an `impl` is accepted and dies as `Operator is not defined for record and record`.

`type_directed` already handles `Impl_decl` — the walker was written for this and the registration was not.

Until it lands, the prelude and everything built on it are written in the regime that is not checked.

| Fixture | `comptime/errors/impl_constraint` — a generic `impl` method using `+` at a type with no `op +`, rejected the way the same body in a `fn` already is |

### 3 · One builtin table

`builtins.ml` declares `Array__len` in one list and implements it in another, with a string literal the only link. A typo in either type checks clean and dies at run time as `Undefined variable`. `__array_get`, `__array_set` and `__array_of` appear in the values list and in the registry but in neither `methods` nor `functions`, so they are type checked nowhere and `Resolve` trusts the type it derived.

A record carrying name, signature and implementation, with `types` / `methods` / `functions` / `values` derived from it, and a startup assertion that every name the registry emits exists in it. OCaml's `external` and Rust's `#[lang_item]` bind the three at one site for this reason.

This sits in the silent group because a mistyped name type checks clean and dies at run time — the same failure mode as the two above, not a smaller one.

An unwrapping layer — `as_int`, `as_str`, `arg2` — is part of this step rather than a later one, because it decides what a table entry looks like. Roughly two fifths of `builtins.ml` is `| _ -> Value.fail span "Cannot apply %s to these arguments."`, branches unreachable if the checker is right.

It does not touch the interpreter's duplicated operator table, which is [accepted debt](#accepted-and-stated-as-such): those arms implement entries whose emission is `Primitive`, which are not named functions and so never enter this table.

| Fixture | `builtins/errors/missing_implementation` — a registry entry naming a function with no implementation, rejected at startup rather than at the call. A green suite today is evidence the invariant was never enforced, not that the change is safe, so this step owes a case that trips its assertion |

### 4 · Structural equality, plus identity and cycles

`value.ml:80` compares arrays and records with OCaml's `==` and variants structurally, so which question `==` asks depends on which constructor the value happens to be. `Some(1) == Some(1)` holds; `Point { x: 1 } == Point { x: 1 }` does not. The defect is the incoherence rather than the default: identity for aggregates is a defensible position and `tests/core/arrays/identity` states it deliberately, but `Variant` does not follow it.

`prelude.ml` — `List.contains` — inherits whichever one its element happens to be, and `stdlib/collections/HashMap.cx:44` is `map.keys[s] == key`, which will never find a record key.

Two things have to be answered before changing it.

**Identity comparison must survive.** `var ys = xs` shares storage, and asking whether two names refer to the same object is a real question the language would otherwise lose. It becomes a builtin — `same(a, b)` — not an operator, matching `ptr::eq` rather than OCaml's `==`/`=` pair, because two operators one character apart is the trap OCaml is known for.

**Cycles must terminate.** Structural comparison of a cyclic value does not, and no cycle is constructible in the language today — mutable record fields exist, but nothing can hold a forward reference to a value under construction. That changes with graphs and parent pointers, which is the standard library this plan exists to enable. The definition is therefore coinductive: a pair of physical addresses already being compared is treated as equal, which terminates on cycles and agrees with the inductive definition on acyclic values.

**It reverses a passing fixture, and the plan has to say so.** `tests/core/arrays/identity` is in the passing list. It pins `print([1] == [1])` to `false` and its comment says arrays have identity deliberately. This step makes that line print `true`, so the step is not a bug fix — it is overturning a decision that has a test behind it. Sequence: rewrite the fixture to assert aliasing through `xs[1] = 77` (which is the part that is genuinely about identity) and `same(xs, ys)`, then change `values_equal`. Reversing the expectation without rewriting the fixture would look like a regression to anyone bisecting.

`tests/operators/operator_eq` stays green either way — a declared `op ==` takes precedence over both defaults.

| Fixture | `core/operators/structural_equality` — arrays, records, nesting, `List.contains` finding a record, and `same` distinguishing two equal values. Plus `core/arrays/identity` rewritten, not merely re-expected |

## Loud — a diagnostic, or a cost

| # | Step | Size |
|---|---|---|
| 5 | Resolve `print` through the environment | ~20 lines |
| 6 | Give the prelude a filename and file-relative spans | ~40 lines, with modules |
| 7 | Printing through a declared interface | ~80 lines, after method constraints |
| 8 | Surface syntax for the registry | ~120 lines |
| 9 | Per-unit compiler state | ~100 lines, with modules |

### 5 · Resolve `print` through the environment

`typecheck.ml:446` tests `List.mem name Builtins.variadic` against the syntactic name, ignoring scope, and `builtins.ml:35` binds `print` to an unconstrained type variable. `var p = print; var n: int = p;` is accepted — a wrong program let through — and a user's own `fn print` produces `Verify error: A call should be int but is annotated unit` instead of a shadowing diagnostic.

Resolving the name through the environment rather than by string fixes both, needs nothing decided, and is smaller than any step in the silent group. It is listed here rather than above only because the accepted-program half is a value of function type flowing into an `int`, which no realistic program writes by accident; the shadowing half is a bad diagnostic, not a bad program.

The variadic list is also compiler-private, so the standard library can never add a variadic function of its own. That is a separate hole and is not closed here.

| Fixture | `types/errors/print_is_not_a_value`, and a program declaring its own `print` |

### 6 · Prelude identity

Spans need a source identity before there are two sources; a prelude error reports at a prelude line as though it were the user's file. `l[7]` on a `List` prints `[13:16] Index 3 is out of bounds for length 3` — wrong index, wrong length, wrong file.

A reserved prefix for compiler-generated names — `Ast.method_name`'s `%s__%s`, `Specialize`'s `__%d` — closes the collision half: `fn Array__len(v: int)` written by a user collides head-on with the mangled builtin.

Do it with modules, when a third source appears anyway.

### 7 · Printing through a declared interface

`print` and `str` both render through `Value.string_of_value`, so a type can never control its own output. `stdlib/ops/ToString.cx` is the intended answer, wired to nothing. Every language of this shape routes printing through one overridable interface — `Display`, `Show`, `Stringer`, `__str__`.

Blocked on the [method-constraint question](#method-constraints): `ToString` as a constraint is exactly the mechanism that does not exist.

### 8 · Surface syntax for the registry

`op` declarations reach the operator table; nothing reaches `containers`, `indexed`, or `constructors`, so `[…]` and `[i]` stay privileges of types blessed in OCaml — including `List`, which is otherwise ordinary Cronyx. [Elaboration](Elaboration.md) already gives indexing its form; [Collection Literals](Collection%20Literals.md) leaves the literal case as the `of` convention, which is a naming rule rather than a table entry and should be reconsidered when this is done.

### 9 · Per-unit compiler state

`Types.default_container`, `Types.extra_admits`, `ctx_types`, `ctx_traits`, `ctx_methods`, `ctx_type_params` and `Resolve.declared_rows` are module-level and correct for one program per process. Step 10 compiles several units and Step 11 has compilation calling itself. Fix it when the second unit exists.

## Accepted, and stated as such

**Encapsulation is deferred to modules.** `List.items` and `List.count` are public, so `l.count = 99` makes `l.len()` return `99`, and every prelude type's representation is its public API. Visibility is a module-system feature everywhere it exists — `pub`, `.mli` — and inventing a second mechanism before modules would be work thrown away. It is a real hole until then.

**The interpreter's duplicate operator table is debt, in full.** `registry.ml:70` says which `(op, ty, ty)` triples exist; `interp.ml:10` re-implements the same table as hand-written arms with its own fallthrough, and nothing enforces the correspondence. So `Registry.Primitive` means "an arm exists elsewhere", and adding `%` touches `token.ml`, `scanner.ml`, `ast.ml`, `parser.ml`, `registry.ml` and `interp.ml`. [Elaboration](Elaboration.md) says builtin arithmetic is not a special case in the compiler; that is true of the checker and false of the interpreter. Routing primitive arithmetic through the builtin table would fix it and would put a closure call on every integer addition in a tree-walking interpreter that is already slow. Not paid until there is a reason. The LLVM backend is that reason: it needs a third implementation of the same table, and three is where the convention stops holding.

## Open questions

Neither is scheduled work, and neither is decided here. Each states the options, what turns on them, and what would settle them. They are the two things a standard library cannot be written around.

### Method constraints

`typecheck.ml:508` picks a receiver when exactly one type owns a method of that name. That is already false for `len`, and every container added makes it worse. Nothing expresses "T has a method `m`."

Three shapes, in descending order of fit with what is already here:

**Infer it, as operator constraints are inferred.** A method call on a type variable records a requirement — *has `m` with this signature* — carried through generalization and discharged when the copy is made. The operator half works because `Registry.find` can be consulted at the copy site; `ctx_methods` is keyed `(type, name)` and can be consulted the same way, so the mechanism is reachable. The work is carrying the requirement through instantiation, which `quantified_rows` and `quantified_fields` already show the shape of. Fits [Comptime Params](Comptime%20Params.md), which rules out written bounds for operators on the grounds that they name a predicate over a table as though it were declarable.

**Write it** — `fn f<T: ToString>(x: T)`. What every language of this shape does, and what `stdlib/ops/` was written against. Contradicts the inference decision already taken for operators, which would leave the language inferring one kind of constraint and requiring the other.

**Structural bound** — the receiver's type is a row demanding the method, which is machinery that already exists for record fields. Uniform with the field case, and makes method presence a structural rather than nominal property, which cuts against `impl` being nominal.

What turns on it: steps 1 and 7, `Hash` and `ToString`, and most generic code in `stdlib/collections`. What would settle it: writing `HashMap.get` under each of the three and seeing which reads as Cronyx. That is a page of code, not a research question.

### Fallibility

`prelude.ml:19` signals a bad index by provoking an array panic, which reports the wrong index at the wrong line. There is no `panic`, no `Option`, no `Result`, and the effect machinery — rows, `run`, `handle` — is untouched by every builtin.

Three shapes:

**Effects.** `tests/stdlib/fallible` and `tests/stdlib/error` were written this way, `raise` and `throw` as operations with handlers. It is the mechanism the language already has and the only one that costs nothing at a call site that cannot fail. [TODO](TODO.md) records that a call unifies rather than subsumes rows, so a pure function called from a fallible one is annotated fallible and then handed evidence it has no parameters for — that bug is directly in the path of making every indexing operation effectful.

**`Option` / `Result` in the prelude.** Sums and `match` exist, so this works today. It puts a `match` at every index, which is why no language with indexing does it for indexing.

**A panic that is a real construct** rather than an out-of-range read that happens to fail. Cheapest, honest about the current semantics, and not composable — a library cannot recover.

What turns on it: every prelude and library function that can fail, which is most of them, and the diagnostic quality of the ones already written. What would settle it: deciding whether indexing out of range is recoverable. If it is not, the third option is enough and the first two are for library code that genuinely returns absence. The row-unification bug is a prerequisite either way.
