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

**Effect rows at call sites are equated, not subsumed.** A call unifies the callee's row with the caller's, so a pure function called from an effectful one comes out annotated with the caller's effects — and the CPS pass, reading that annotation, passes it evidence it has no parameters for. `fn twice(n) { return n * 2; }` called from a function that performs `log` fails at run time with an arity error. Koka's rule is containment, not equality. Fixing it means a subrow constraint, or taking a function's row from its declaration rather than from a call site's inference variable. [Resolve] already does the latter for the calls it synthesizes, since it emits the declaration and the call together.

**Verify does not check effect rows.** It compares each node against its children, and a row belongs to a function reached through a name. The bug above is exactly the kind it was built to catch and cannot.

**A deferred method call's result type rests on its uses.** When a receiver is a type parameter, the call is checked for the method's existence and its result is a fresh variable, pinned by whatever the caller does with it. `"x " + item.summarize()` pins it to string; a result nobody constrains is not checked against the impl until the copy is made, and not at all if no copy is. Specialization could re-check each copy against the declared method type.

**Name resolution rules.** `tests/core/resolution` pins behaviour that no document describes — shadowing, ordering, and what a name means when several things could provide it.
