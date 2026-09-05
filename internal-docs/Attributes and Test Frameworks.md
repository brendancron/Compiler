# Attributes and test frameworks: implementation plan

[Attributes.md](Attributes.md) is what an attribute is today and [Testing.md](Testing.md) is how `cx test` runs. This is the order the two grow into one mechanism, and what "done" means at each step.

One rule shapes the ordering: **`cx test` is the first user of a facility that should not be its own.** Finding every declaration marked `@test` and generating a table from it is the same problem as a routing table, a serialization registry, a benchmark harness, and a CLI dispatcher. Today it is compiler code, so each of those would be another compiler patch. Every step below either fixes something already wrong or moves the mechanism toward being general, and the proof of the last one is that `@test` leaves the compiler.

A step is done when its criteria hold *and* a fixture covers each one — `tests/` for the language, `cx/test/packages/` for the tool.

## 0. Test code stops shipping

`@test` marks an ordinary function, and nothing removes it. A plain `cx run` on the package `cx new` writes carries the test into the program:

```
$ cx run --dump-code src/main.cx
-- code --
fn greeting(): string { … }
print(greeting());
@test
fn greets() { assert((greeting() == "Hello, World!"), "the greeting changed"); }
```

The artifact a consumer links against carries it too. This is a defect rather than a design question, and it comes first because everything after it links against artifacts: a `tests/` target that consumed an artifact full of test code would be building on the bug.

D is the precedent — `unittest` blocks compile only under `-unittest`.

**Done when**

- `cx run` and `cx build` produce a program with no `@test` function in it, checked through `--dump-code`.
- `cx test` still sees every one.
- A `.cxa` written by `cx build` contains no test declaration, so a dependency's tests cannot be reached from a consumer.

## 1. A test's name is its path

Discovery knows which unit a test came from — declarations are mangled under their package — and the runner throws it away, matching and printing only the leaf. So the namespace is flat: two units with an `adds` are indistinguishable in the report, and there is nothing structural for a filter to select.

Keep the path. A test is `math.adds`, and the filter matches against that:

```
cx test math.        # the unit
cx test adds         # substring, as today
```

This is how `cargo test parser::` works, and it is worth being clear that Rust has no grouping *feature* — it has paths in names and a substring filter over them. Selection comes free from a hierarchy that already exists.

A filter ending in `.` should match a segment prefix and anything else a substring, or `cx test parser` selects `superparser.x` — the papercut `cargo test` lives with.

**Done when**

- The report names every test by its qualified path.
- Two tests with the same leaf name in different units both run and are distinguishable in the output.
- A segment-prefix filter selects a unit; a substring filter behaves as it does today.

## 2. An attribute is a value of a declared type

An attribute is a name and a list of arguments, each a `string`, `int`, `float` or `bool` written out. Nothing declares `test`, so `@tset` is inert rather than an error, and `@test("recovery", 3)` is a well-formed list of variants that some walker will misread later. A deriver destructures by position, against a convention no type records.

An attribute becomes a value instead:

```cronyx
@Test { group: "parser.recovery" }
fn recovers_unclosed_brace() { … }

@Test
fn adds() { … }
```

An unknown attribute fails to resolve, arguments are checked against a declared shape, and a deriver matches a type rather than a list. Java, C# and Kotlin all settled here; Rust did not, and `#[foo(bar = 1)]` is an inert token tree that means whatever the macro reading it decides.

Grouping then needs no syntax of its own — it is a field, and it composes with step 1 by extending the path. Groups within groups are segments in one string, so there is still exactly one selection mechanism.

**The boundary is reifiability, not literalness.** [Reify.md](Reify.md) is the constraint: an attribute value is written back into source, and `cx build` marshals it into a `.cxa` that another compiler reads. Records, enum variants, arrays and nestings of those qualify. A closure, an effect, or a reference does not, and the rule has to be stated rather than discovered by whoever first writes one.

Two costs. A bare `@Test` needs a unit struct or variant to name. And `@Foo { … }` before a declaration carries the record-literal-versus-block ambiguity that bites Rust in `if` conditions; attribute position makes it tractable, but it is parser work rather than a free win. It is also a breaking change to every `@test` and `@derive` already written.

**Done when**

- An attribute naming no declared type is a diagnostic with a span.
- An attribute whose fields do not match its type is a diagnostic with a span.
- A deriver reads an attribute as a typed value.
- An attribute value that cannot be reified is rejected where it is written, naming why.

## 3. Reflection reaches declarations

`Discover.carrying` is a syntax walk because reflection cannot do this: attributes belong to a declaration, `typeof` takes a value, and a function value's type — `(int, int) -> int` — names no declaration to look one up under. However much is added to `TypeShape`, it cannot close that gap.

So reflect *declarations*: a meta-time handle carrying the qualified path, the attribute values, the signature, and the function itself as a callable. A closure could never be reified, but a reference to a declaration is a name, and a name writes back into source cleanly.

The question this step has to answer rather than discover: **when does the query run?** A meta function enumerating declarations while another meta function is still generating them sees a program that is not finished. Either enumeration is a fixpoint, or it happens at a stated point after generation with generation forbidden to depend on it.

**Done when**

- A meta function enumerates every declaration carrying a given attribute type, in a defined order.
- It can call one, and the generated code survives `--dump-code` as plain Cronyx.
- The ordering question has an answer in [Metaprocessing.md](Metaprocessing.md), and a fixture covers a `gen` that produces a declaration the query then finds.

## 4. `@Test` leaves the compiler

The runner becomes what a user could have written: a meta function that asks for every declaration carrying `Test`, wraps each in its own `run { … } handle Assertion { final ctl failed(msg) { … } }`, and emits the table. `cx test` builds the package, runs that, and reports.

Nothing about the isolation changes — `final ctl` is still what makes a failure abandon one test and leave the next standing, and that argument belongs in [Testing.md](Testing.md). What changes is that a routing table or a benchmark harness no longer needs a compiler patch to exist.

**Done when**

- `Discover` and the runner's wrapper synthesis are gone from `cx`, and the prelude declares `Test`.
- The suite's output is byte-identical to what it was before the move.
- A second attribute — a benchmark, a route — is collected the same way in a fixture, without touching the compiler.

## Not in this plan

**`tests/` as a target.** Inline `@Test` reaches package internals, which is right for a unit test and wrong for checking the surface a consumer sees. `tests/*.cx` would compile as consumers, reaching the package by name through its artifact the way a downstream package does — so the export boundary is the one `Loader` already enforces rather than a new visibility concept. It needs step 0 first, or it links against artifacts carrying tests, and it needs `[dev-dependencies]`, which [Package Manager Plan.md](Package%20Manager%20Plan.md) parks until a feature calls for it. This is that feature. Two questions to settle then: whether each file is its own program, as a Rust integration test is its own crate, and whether `cx publish` ships the directory at all.

**Tags.** A path cannot express `slow`, which exists in every group, and forcing it into one wrecks the hierarchy. That wants distinct syntax — a second attribute, not a segment — and it can wait for something that needs it. Rust fills the whole of this slot with `#[ignore]`.

**Multiple filters and negation.** `cx test parser lexer` and `cx test '!slow'` need no language surface and no plan; fold them in whenever the filter is being touched.
