# Implementation plan

The remaining language work for `bootstrap2`, and the fixtures each step should light up.

Steps 0 through 7b — the registry and `Resolve`, arrays, tuples, records, sums and `match`, reify, comptime params, operator declarations, methods — have landed. What they claimed is no longer written down here: the suite is the record, and a prediction kept beside its outcome is a second source that drifts. `git log` has the order they arrived in.

The compiler's own hygiene runs beside this as a second stream, in [Remediation of Builtins](Remediation%20of%20Builtins.md). Its steps are numbered separately and referred to below as *remediation N*. Where the two meet is called out per step, because three of the five steps here depend on that stream rather than on each other.

That stream is not only hygiene, and the numbering here does not cover what it has produced. `char` and `byte`, string escapes, character-indexed strings and string immutability are all language features, and none of them has a step number — they arrived because moving a builtin into the prelude required them. Expect more of that: a migration finds the gap, and the gap is a feature. [Data Structures](Data%20Structures.md) is where the resulting language is described; this document is only the queue.

The suite is 258 fixtures — 181 run, 60 error, 2 runtime, 15 expected-failing — plus a round-trip check over every run fixture, for 439 assertions in total.

## Where to pick up

Steps 0 through 13 have landed and **nothing is queued behind a step**. What is left is fifteen fixtures in two categories, and three of them want a decision rather than work.

| | |
|---|---|
| **Waiting on a decision** | `builtins/conversions` needs [fallibility](Remediation%20of%20Builtins.md#fallibility) settled for `to_int` — the discussion favoured `<Fallible> int` over `Option<int>`, and now that [step 13](#13--parameterized-traits--done) exists it could be `impl TryFrom<string> for int` instead of a function. `builtins/readfile` and `writefile` wait on what IO becomes once `print` is an effect. |
| **Waiting on a feature** | GADTs (`types/gadt`), `async` (`effects/async`), and tuple destructuring in a binding (`core/for_tuple`, which also wants `HashMap.entries()`). |
| **Parked** | `compile/*`. Native compilation is out of scope for this bootstrap. |

**Deliberately not built:** string interpolation, and a formatter. `--dump-code` prints the tree as Cronyx and its round-trip check guards mechanical rewrites, but neither is a formatter — comments are discarded by the scanner, so a real one needs trivia the tree does not carry.

**Two limits worth knowing before extending anything.** Two impls of one trait for one type at different arguments collide, because a method registers under `(type, method name)`. And the prelude is order-sensitive: a call above a declaration is monomorphic, so an entry that uses a helper must come after it.

## Order

**Steps 8 through 12 have landed.** What each one turned out to be is below; three of them cost less than planned and two cost more, and in every case the difference is recorded where the step is.

Nothing is queued behind a numbered step any more. What remains is four `stdlib/` fixtures, none of them read yet, and four standing categories that no step will discharge: fixtures predating a syntax decision, ones deferred on purpose, the LLVM backend, and features with no design.

Two additions came out of the stdlib port rather than the plan: [function values](#not-covered-by-the-design) and [uniform function call syntax](#uniform-function-call-syntax). Both were `Unplanned` and both were unavoidable — `stdlib/list` is written entirely in terms of the first, and the second is what makes it readable.

## What each step is claiming

`bootstrap2/test/test_bootstrap2.ml` carries four lists. `cases` pairs a `.cx` with expected stdout, `error_cases` with a `.err` of diagnostics, and `runtime_cases` with a `.rt` for a program that must be accepted and then fail while running — `error_cases` stops at `Resolve` and never reaches the interpreter, so that third kind had nowhere to live until step 14 needed it.

`expected_failing` is the fourth: every fixture under `tests/` that none of the others claims, tagged with what it is waiting on. The suite asserts they still fail, so a fixture that starts working is reported rather than sitting unnoticed.

The tags a step can discharge are `Step "..."`; the rest will not be fixed by finishing one.

| Tag | Count | Meaning |
|---|---|---|
| `Parked` | 9 | `compile/*`. Native compilation is out of scope for this bootstrap — not waiting on anything, not being worked on |
| `Unplanned` | 6 | no design anywhere yet |



**No fixture is waiting on a step.** Steps 8 through 13 are done, and everything they were tagged with is discharged, retired, or turned out to be waiting on something else. Two categories are left.

`Parked` is native compilation, deliberately not being worked on.

`Unplanned` is six fixtures, and they split two ways. **Three want a decision, not code**: `builtins/conversions` needs [fallibility](Remediation%20of%20Builtins.md#fallibility) settled for `to_int`, and `builtins/readfile` and `writefile` need to know what IO is once `print` becomes an effect. **Three want a feature**: GADTs, `async`, and tuple destructuring in a binding.

**Five fixtures were retired rather than fixed**, each describing something the language decided against: an array-of-records sample with no expected output, `free(p)` with no memory model to want it, two trailing lambdas written `{ x, y -> … }` where a trailing block takes one parameter called `it`, and a record literal that named a type nothing declared — which is what `records/nominal` now covers, with `new` and a declaration.

**Step 8 never had a tag**: no fixture under `tests/` was waiting on `Set` or `Map`, so finishing it discharged nothing here — its fixtures were written with the feature, which is what that step's entry now records.

## 8 · List, Set and Map — done

All three are prelude Cronyx. `Set<T>` is twenty-eight lines over a `List<T>`; `Map<K, V>` is forty over a `List<(K, V)>`, and is the first two-parameter container the language has had. Neither needed a compiler change beyond remediation 4, which made a container's element come from its declared entry rather than from an assumed arity.

`Set` declares a literal and no indexing, so `seen[0]` is an error — the three bracket entries are independent and a type takes only the ones that mean something for it.

Element equality was the one thing shipped wrong here — `Set<Point>` kept duplicates and a `Map<Point, _>` treated equal keys as distinct — and remediation 16 fixed it: `==` is structural, and `same(a, b)` is the identity question when that is the one being asked.

**Lookup is still a linear scan.** Hashing needs [method constraints](Remediation%20of%20Builtins.md#method-constraints) decided, and so does a `get` returning an `Option` — [Data Structures](Data%20Structures.md) still shows one, and `Map` supplies `contains` and `get_or` instead so that fallibility stays undecided.

| Fixtures | |
|---|---|
| Passing | `core/collections/set`, `core/collections/map`, `core/collections/user_container` |

## 9 · Iteration — done

`for (x in xs)` desugars rather than resolves, which is the one thing the plan had wrong. The expansion is uniform, so the type-dependence is handled by the machinery underneath it:

```
for (x in xs) BODY
  ⟶  { var seq = xs; var i = 0;
       while (i < seq.len()) { var x = seq[i]; BODY; i = i + 1; } }
```

`len` and `[]` are written as ordinary source, so the checker resolves them for whatever the sequence turns out to be — the array intrinsics, the string ones, or `List`'s method and `op []`. The sequence is bound once, so an iterable that is an expression is evaluated once. `in` is not a keyword; the two `for` forms are told apart by lookahead.

A `Set` cannot be iterated, because it declares a literal and no indexing — `Cannot index Set<int>.` That is the honest limit of resolving the known shapes directly, and it is where an iterable trait would go. [Method constraints](Remediation%20of%20Builtins.md#method-constraints) are decided, so that is available whenever a second shape needs it.

| Fixtures | |
|---|---|
| Passing | `core/lists/list`, `core/control/for_in` |
| Still blocked | `effects/logic/simple_guard`, `effects/logic/multi_guard` — need `resume` inside a loop, which is a CPS matter |

## 10 · Modules — done

`import`, and a module as a compile-time value. [Modules](Modules.md) has the design and the built state; the short version is that the loader hands the pipeline one program with each unit's declarations renamed, which is what makes the circular-import fixture work rather than diagnose.

Functions, types, traits and sum types all cross a boundary, including `impl` blocks written for an imported type and `match` patterns naming an imported sum. A type is named across a unit the way a value is — `geom.Point` in an annotation, `new geom.Shape::Circle(3)` in an expression.

Two things the plan expected turned out otherwise. **Remediation 19 is not needed**: module-level compiler state is correct for one program and wrong only for separate compilation. And **the phase-separation rule is discharged rather than implemented** — an imported unit contributes only declarations, so a meta block cannot see an imported runtime value because there is none to see.

**The prelude is still order-sensitive**, and modules did not fix it: a call to a generic `impl`'s method from anything inferred before that impl pins its type variable. That is now a property of one string in `bin/main.ml` rather than of the loader, and it stays until the prelude becomes a unit.

### The `stdlib/` fixtures were never waiting on modules

They import fine. What they are waiting on is their own source, which is written against the Rust bootstrap — `struct`, `[int]`, `enum`, `fn f(a): T`, `__eq_int` — and, under that, on features the language has not got.

Porting `ToString`, `StringBuilder`, `String` and `Hash` turned up five gaps, each small enough to close on the spot:

- **`%` and `%=`.** Modular indexing is how a hash table probes, so neither table could be written without it. It discharged `core/math/modulus`, which had been sitting under `Unplanned`.
- **A keyword after `.`.** `HashSet.new()` could not parse. Nothing but a member may stand there, so a keyword is an ordinary word in that position, and `fn new` is allowed to match — the one keyword accepted as a declaration name, because it is the constructor convention.
- **An order on `char` and `byte`.** `is_digit` is `c >= '0' and c <= '9'`, which needs one. A scalar value orders by code point, an octet by its byte.
- **`ord`.** A `char` is a code point now, so this is the number it already is. `core/builtins/ord` was tagged for wanting exactly this.
- **`starts_with`, `ends_with`, `index_of` and `replace`** on `string`, in the prelude over `len` and `[]`, which is where this plan already said they belonged. That discharged `core/strings/string_starts_ends`.

**A hash table is not iterable by `for`**, which desugars to `len` and `[]` — and `m[0]` meaning *a key* would be nonsense next to `m[key]` meaning a lookup. `HashMap.keys()` and `HashSet.items()` return a `List` instead, and both walk the slot table rather than the backing array, which is what keeps a removed key from reappearing behind a tombstone.

**Trait bounds were already built.** `fn probe<K: Hash>(…)` parses, builds a `Types.Bound` kind, gives `key.hash()` a result type through the trait's signature, and rejects a call passing a type with no `impl` — *Expected Hash, got bool*. A bound is read as a comptime parameter whose annotation names a declared trait, which is why nothing needed adding for it.

What was missing was one line in `Specialize`. A `Call` was type-directed only if its *callee* was generic, while a `Method_call` also looked at its receiver — so a generic function called with a generic argument, which is every use of a bounded helper from inside a generic method, was never copied per concrete type. The call then named a function specialization had deleted. Both tables work with that fixed.

**A bound on an impl is still dropped.** `impl Box<K: Hash>` parses, but `Impl_decl` carries its parameters as bare strings, so the bound goes nowhere and a method calling `self.held.hash()` fails. Both tables are written around it — the hashing lives in bounded free functions and the methods delegate — which is not a workaround so much as where that code belongs, but the gap is real and carrying `comptime_param` on `Impl_decl` would close it.

**Function values are built**, which `List` is written entirely in terms of. A function with no name is `(a, b) => body`, where the body is an expression or a block — `fn` stays for declarations. A block after a call is sugar for a one-parameter function value whose parameter is `it`, so `List.map(nums) { it * 2 }` reads as it did before.

Three things fell out of building it:

- **An expression can hold statements**, which nothing could before. Every stage's `expr` and `stmt` are now one recursive group; `expr` could not name `stmt` at all, since they were declared separately. Two passes needed the same treatment, and `CPS` needed a knot tied through a `ref` because its expression and statement converters sit in groups with other definitions between them.
- **`=>` became one token.** It had been scanned as `=` then `>` and reassembled in match arms, which cannot survive a parser that has to look for it past a closing paren. `(a, b)` reads exactly like a group or a tuple until then.
- **A trailing block is tried as an expression first.** `{ it * 2 }` is a value and `{ var t = it; return t; }` is a body, and telling them apart takes parsing one and backtracking.

**A function value cannot perform a delimited effect.** Its body converts like any other, but carrying evidence out of a value is not something anything does yet, and `CPS` says so rather than emitting something wrong.

**`final ctl` is built**, following Koka: a handler that cannot resume, whose operation's result is therefore fresh at every call site — and which needs no continuation, so it costs an unwind rather than a CPS conversion. That takes failing, the most common effect a program has, off the expensive path entirely and lets it be thrown from inside a loop or a function value. [Effects](effects.md#what-each-translation-costs) has the table.

**A loop that suspends is converted rather than rejected**, into a recursive continuation. **A function value carries evidence** the way a declaration does, so an effect may be performed inside a lambda. And **a `run` block now delimits what it should** — the body's completion returns to whoever entered it instead of continuing the program, so a multi-shot handler no longer re-runs everything after the block. That discharged both `effects/logic` fixtures, which were the last tagged for step 9. It is what lets `return throw(…)` check at `int` in one function and `string` in another, and it is why Cronyx needs no bottom type — see [effects](effects.md#operation-kinds).

Building it uncovered a `CPS` bug that predates it: `return` and *what runs next* were one continuation, so a `return` inside a branch was left untouched and its value never reached the caller. Both are recorded with the effects design; `effects/nested_return` pins it.

**Generic effects are built**, and `stdlib/fallible` passed without its module being touched — `effect Fallible<T>` was already written in the form that now parses. See [effects](effects.md#surface-syntax) for the rule. Two places had to agree: the declaration binds one variable per parameter, and a handler's arms re-derive their types from that declaration, so the same bindings are restored around them.

`Regex`, `Toml` and the automata are 1,300 lines against the old slice and struct syntax, and none of them has been read.

| Fixtures | |
|---|---|
| Passing | `core/modules/*`, and `stdlib/` — `math`, `error`, `tostring`, `stringbuilder`, `string`, `hashset`, `hashmap`, `iterable`, `list`, `fallible` |
| Not yet read | `stdlib/regex`, `stdlib/toml`, `stdlib/automata/dfa`, `stdlib/automata/nfa` |
| Still blocked | 13 under `stdlib/`, every one on its own source rather than on the loader |

## 11 · Metaprocessing — done

`meta` blocks, `meta` over a bare statement, `gen`, and `meta fn` all work, and metaprocessing is recursive compilation literally: `lib/metaprocess.ml` runs the same nine passes on a block's statements that the rest of the program goes through. [Metaprocessing](Metaprocessing.md) has the mechanism — the capture table, what substitutes, and the splicing rules that the fixtures forced.

**A meta function's parameters take values**, because a meta function is an ordinary function reduced away before the program runs. That decision moved `derive/basic` out of this step: `derive(Dog)` needs `Dog` to *be* a value, which is step 12's work, and the fixture also declares its `meta fn` inside a `trait` and reads `T.name`. It is a step-12 fixture with a rewrite attached.

It also closed off `print` becoming a meta function — `print(a, b)` with a runtime `a` has no value to reduce — so remediation 8's remaining half is a choice among rest parameters, a top type, or a one-argument `print`, and not this.

Reify became real here, as step 5 predicted, though narrowly: only a value with a literal form is written out, and a name position substitutes only for a string.

| Fixtures | |
|---|---|
| Passing | `meta/execution/*`, `meta/codegen/*`, `meta/functions/*`, `meta/params/func` |
| Needing change | `meta/derive/basic` — and `meta/functions/fib`, which uses `meta fn` for a computation that should be a staged call |

## 12 · `typeof` yields a `Type` — done

`typeof(x)` was a string; it is a `Type` now, with `.name` and `.shape`, and a `Type` cannot reach runtime — `print(typeof(x))` is an error naming what to ask for instead. [Architecture](architecture.md#reflect-answers-a-types-questions) records how `Reflect` answers and why the answers are one level deep.

**A type is not uniformly a thing with fields**, which is what enums forced. `.shape` is a sum — `Scalar`, `Product` of fields, `Sum` of variants, or `Other` — and a deriver matches it, because comparing fields and matching variants are different jobs.

**What a field reports is its name, not its type.** That is a real limit, and a deliberate one: it keeps every answer a literal this pass can build, and a deriver mostly needs names anyway, since the code it generates says `self.x == other.x` and lets the checker resolve the rest. Conditioning on a field's type is what will need the lazy handle, and a self-referential type is why it cannot be eager.

**A deriver works on top of this**, which is what the step was for — see `code` in [Metaprocessing](Metaprocessing.md#code-builds-syntax-gen-emits-it). `meta/code/derive_eq` reads a `TypeShape`, folds one comparison per field, and generates a working `eq`.

**`typeof` takes a type name as well as a value.** A bare name is a value's type if one answers to it, and the declared type otherwise, so `typeof(Dog)` reflects a type nothing holds and `typeof(int)` a primitive. A value of that name wins, which is ordinary shadowing.

**The type a generated `impl` is written for comes from the shape, not from `Type.name`.** The plan had `Type.name` becoming a `Name`; that is wrong, because `typeof(f).name` is `(int) -> int` and no identifier could be. `Product` and `Sum` carry a `Name` because a declaration exists to name; the other shapes do not. So `derive_eq` derives for any product, and `Type.name` stays a string, which also spares every fixture that prints one.

**Deriving is built on top of it**, which is what the step was for. `derive Eq, Show for Dog;` runs one registered deriver per trait, each generating an `impl` from the type's shape — see [Deriving](Metaprocessing.md#deriving). Passing a whole `Type` to a meta function was never needed: a deriver takes the shape, and the name of the type comes out of it.

| Fixtures | |
|---|---|
| Passing | `reflection/shape_*`, `reflection/typeof_type_name`, `reflection/errors/*`, `meta/code/*`, and the seven `typeof_*` that were passing, rewritten to `.name` |
| Rewritten | `meta/derive/basic`, which was written against a deriver declared inside the trait — the design settled on a `for` clause instead |

## One spelling per operator

`&&` and `||` are the logical operators; the `and` and `or` keywords are gone, and both are ordinary identifiers now. They were two spellings of one operator, which is two things to keep in step and nothing gained — the stdlib was written in symbols and the prelude in words, which is how it stayed unnoticed.

The scanner emits `AMP_AMP` and `PIPE_PIPE`, the parser takes each at the level `and`/`or` had, and both build the same `And`/`Or` node, so short-circuiting and precedence come from one implementation. `core/operators/precedence` is what checks that: comparisons bind tighter, `!` binds tightest, arithmetic precedes comparison.

## `defer`

Zig's, not Go's: **block-scoped**, so an inner block runs its own when it ends rather than waiting for the function. Reverse order, and it runs however the block is left — falling off the end, `return`, or an aborting handler unwinding through it.

The statement runs when the block is left, so it reads what a variable holds *then*, not what it held where the `defer` was written. Inside a loop body that means once per iteration, after the increment.

**It is the interpreter's, not a desugaring.** A block collects what it defers as it walks, and runs them on the way out — including out of an exception, which is what makes `return` and an effect unwind behave the same without either being special-cased. Desugaring would have had to find every exit itself.

**A deferred statement runs after an aborting handler's arm, not before.** The arm runs where the operation was performed; only then does the stack unwind, and the deferred statement is on the way out. `core/defer/defer_effect` pins the order.

## 13 · Parameterized traits — done

`tests/core/traits/try_from` was written before any of it, and failed on the first line of the feature until each piece landed:

```cronyx
trait TryFrom<S> {
    fn from(source: S) -> <Fallible> Self;
}

impl TryFrom<string> for int { … }
impl TryFrom<string> for bool { … }

fn do_work<S, T: TryFrom<S>>(source: S) -> <Fallible> T {
    return T.from(source);
}
```

**Why this shape rather than `To<T>`.** Put the trait on the source and dispatch on the result, and every call needs the result type inferred — which is why Rust's `parse::<i32>()` has a turbofish. Put it on the target and the bound does the work: `T` is named, so the impl is chosen the way `<K: Hash>` already chooses one.

**What it needs**, and none of it is dynamic dispatch:

- **Trait type parameters.** `Trait_decl` carries a name and its method signatures; it needs a parameter list, and `Impl_decl` needs arguments — `impl TryFrom<string> for int`, where the trait today is a bare `string option`.
- **Receiverless methods, and `Self`.** Every trait method takes `self`, so there is neither a way to write `fn from(source: S) -> Self` nor a way to call it. This is the piece that unblocks anything at all here.
- **Parameterized bounds.** `Types.Bound of string list` becomes a list of trait names with arguments, and `ctx_impls`, keyed `(type, trait)`, gains them too.
- **Calling through the parameter**, `T.from(source)`.

**Overloading a trait by its argument** — `impl TryFrom<string> for int` beside `impl TryFrom<float> for int` — is the shape [slicing](#slicing) already needed, where an `[]` entry went from one per type to one per index type.

**Dispatch stays static.** `Specialize` copies `do_work` per concrete `(S, T)`, so `T.from(source)` becomes a direct call to that type's entry. No vtables, no dictionaries.

**One thing to expect.** `var n: int = do_work("42");` takes `S` from the argument and `T` from the annotation, so a call with neither is ambiguous. Comptime arguments are already writable, so `do_work<string, int>("42")` is the escape hatch — the same one Rust needed and for the same reason.

**Associated functions came out general rather than trait-only.** `Counter.zero()` on an inherent `impl` works the same way, which makes a constructor a language feature rather than a module-path coincidence — `Dfa.new(…)` had been the latter. `core/traits/associated` covers it, and `traits/errors/no_self` changed meaning: a method without `self` is no longer an error, so it now asserts the error for calling one on a value.

**What made it look impossible for a while was older than the feature.** `Monomorphize.is_value` recognised a bound only when written as a bare name, so `T: TryFrom<S>` was read as a *value* parameter, the function became a template, and the template was deleted — leaving *Undefined variable* at the call with no error anywhere saying why.

**One limit stands.** Two impls of one trait for one type at different arguments — `TryFrom<string> for int` beside `TryFrom<float> for int` — collide, because a method registers under `(type, method name)`. That is overloading by trait argument, the same shape [slicing](#slicing) solved for `[]`, and it is not built here.

## Variadic parameters

`fn max(first: int, rest: ...int)`, filled by the call site — see [the type system](type-system.md#settled). `print` is *not* variadic: it takes one argument, and several are written as one string until interpolation exists.

## Slicing

`a[i:j]` is indexing by a `Range`, so it reuses `op []` and any type can support it — see [Elaboration](Elaboration.md#slicing-is-indexing-by-a-range). Registry entries are now keyed by what indexes them rather than one per type, which is what makes `[]` properly overloadable.

Negative bounds are resolved inside the entry, where the length is known, so they are the prelude's policy rather than a language rule. `core/slices/*` and `core/strings/string_slice` are the fixtures.

## Uniform function call syntax

`x.f(y)` is `f(x, y)` where the receiver's type declares no method `f`, across a module boundary as well as within one — see [the type system](type-system.md#settled) for the rule and for why a method call carries two names. `core/ufcs/*` are the fixtures, and `stdlib/list` is written through it:

```cronyx
nums.map() { it * 2 }.filter() { it > 4 }.fold(0, (a, b) => a + b)
```

## Not covered by the design

Everything `tests/` describes is now either built, retired, or parked. `core/slices` is [slicing](#slicing) and `core/embed` is [embedding](Modules.md#embed-reads-a-file-where-the-program-is-put-together); both are built.

The `Unplanned` tag is 13 fixtures. It has lost seven: `core/math/modulus` to `%` and `core/strings/string_starts_ends` to the prelude during the stdlib port, `core/builtins/ord` once a `char` became a code point, three `core/functions/trailing_*` to function values, and `effects/logic/*` to the effects work.

Two of the trailing fixtures stay, retagged `Rewrite`: they are written `{ x, y -> x + y }`, an explicit-parameter trailing form the design did not take. **A trailing block has one parameter, called `it`**, so a lambda wanting two is written as an ordinary argument — `combine(3, 4, (x, y) => x + y)`. Worth revisiting only if that reads badly in practice.

`effects/generic/generic` was blocked on rows carrying type arguments, which they now do — see [effects](effects.md#surface-syntax). `Yield<int>` and `Yield<string>` are different entries, so one program may hold both, and a handler discharges the instantiation its arms turn out to take.

`core/builtins/conversions` wants `to_int("7")`, which needs [fallibility](Remediation%20of%20Builtins.md#fallibility) decided. The rest — the file builtins, `async`, tuple destructuring in a binding — have no design anywhere.
