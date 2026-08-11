# Deferred

Decisions taken deliberately later, with the reason.

**Map literal sugar.** `["a": 1, "b": 2]` instead of `[("a", 1), ("b", 2)]`. Surface only — it would desugar to the same array of pairs, so nothing about the type rules changes. Worth doing once maps are common enough that the parens are annoying.

**Overloaded string literals.** A `string` literal is a `string`. Making it target-typed, so `var p: Path = "/tmp/x";` builds a `Path`, would attach a constraint to nearly every literal in a program for a feature used in a handful of places.

**Effect questions.** Whether `handle` can alias an effect, syntax for writing a row *variable* in an annotation, and whether effect declarations nest. None block the current design.

**Constructing types programmatically.** `Type` is opaque: values come from `typeof` or a type expression, never from `new Type { … }`. A constructed one would correspond to no real type and have no type expression to reify back to. Allowing it means types built at compile time — which is what metaprogramming that *creates* types rather than inspecting them would need — and a rule for what such a value can be used for.

**Defaults on comptime parameters.** Every one is supplied at the call site today. Adding a default is additive, so nothing breaks when it arrives.

**Declarations below a meta block.** A meta block sees definitions above it only. The dependency graph makes the other order possible; nothing needs it yet.

**Negative indexing and range slicing.** `nums[-1]` counts from the end and `nums[1:3]` takes a subrange, both exercised by `tests/core/slices`. Neither is in the design. Negative indexing is an entry in the indexing table; a range needs a range type or a second index form, and a decision about whether the result shares storage with its source or copies.

**`defer`.** `tests/core/defer` runs statements on scope exit, in reverse order, including on the return path. Interacts with effects: a deferred statement that performs one, or that runs while a continuation is being abandoned by an aborting handler, has no defined behaviour yet.

**`embed`.** `tests/core/embed` exists and has not been discussed. Reading a file at compile time is a metaprocessing capability, so it belongs with the sandbox question about what the evaluator permits.

**Name resolution rules.** `tests/core/resolution` pins behaviour that no document describes — shadowing, ordering, and what a name means when several things could provide it.
