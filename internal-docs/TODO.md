# Deferred

Decisions taken deliberately later, with the reason.

**Map literal sugar.** `["a": 1, "b": 2]` instead of `[("a", 1), ("b", 2)]`. Surface only — it would desugar to the same array of pairs, so nothing about the type rules changes. Worth doing once maps are common enough that the parens are annoying.

**Overloaded string literals.** A `string` literal is a `string`. Making it target-typed, so `var p: Path = "/tmp/x";` builds a `Path`, would attach a constraint to nearly every literal in a program for a feature used in a handful of places.

**Effect questions.** Whether `handle` can alias an effect, syntax for writing a row *variable* in an annotation, and whether effect declarations nest. None block the current design.

**Constructing types programmatically.** `Type` is opaque: values come from `typeof` or a type expression, never from `new Type { … }`. A constructed one would correspond to no real type and have no type expression to reify back to. Allowing it means types built at compile time — which is what metaprogramming that *creates* types rather than inspecting them would need — and a rule for what such a value can be used for.

**Defaults on comptime parameters.** Every one is supplied at the call site today. Adding a default is additive, so nothing breaks when it arrives.

**Declarations below a meta block.** A meta block sees definitions above it only. The dependency graph makes the other order possible; nothing needs it yet.

**Negative indexing and range slicing.** `nums[-1]` counts from the end and `nums[1:3]` takes a subrange, both exercised by `tests/core/slices`. Neither is in the design. Negative indexing is an entry in the indexing table; a range needs a range type or a second index form, and a decision about whether the result shares storage with its source or copies.

**Verify does not check effect rows.** It compares each node against its children, and a row belongs to a function reached through a name. The call-site row bug — a callee annotated with effects it does not perform, given evidence it has no parameter for — was exactly the kind it was built to catch and could not see; it was found by running a program, and fixed as step 11 of [Remediation of Builtins](Remediation%20of%20Builtins.md).

**A deferred method call's result type rests on its uses.** When a receiver is a type parameter, the call is checked for the method's existence and its result is a fresh variable, pinned by whatever the caller does with it. `"x " + item.summarize()` pins it to string; a result nobody constrains is not checked against the impl until the copy is made, and not at all if no copy is. Specialization could re-check each copy against the declared method type.

**Indexing keys on the container, not the key type.** An entry says a type is indexable and its element is that type's sole argument, which is right for `Array` and `List` and wrong for `Map<K, V>` — the element there is `V` and the key is `K`, not `int`. A map wants entries carrying both, which is also what `op [](r: Ring<T>, i: int)` needs to express a user's index type.

**Name resolution rules.** `tests/core/resolution` pins behaviour that no document describes — shadowing, ordering, and what a name means when several things could provide it.
