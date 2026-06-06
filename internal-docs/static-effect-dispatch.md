# Static Effect Dispatch (Koka-style)

Status: design intent, multi-phase work, not started.

## Goal

Replace the current dynamic-dispatch model for effect handlers with
whole-program-ish static dispatch. Every effect operation call site
resolves to a direct LLVM call into a specialized handler — no dispatch
tables, no closure environments threaded at runtime, no indirect calls.

This is the Koka model. The win is (a) performance — effect ops become
as cheap as regular function calls, (b) reasoning clarity — you can
inspect the IR and see exactly which handler runs, (c) simpler runtime
— ctl_handlers / fn_handlers stacks go away.

The cost is (a) compile-time work — substantial. (b) constraints on
what programs are expressible — `handle some_var` either disappears or
needs special handling. (c) months of refactoring across the CPS
transform, monomorphization, and codegen.

## Current model (for reference)

1. Parser produces `RuntimeStmt::WithFn` / `WithCtl` for handler ops.
2. `cps_transform` rewrites every function that performs a `ctl` op to
   take an explicit continuation parameter.
3. `handler_transform` wraps `WithFn` ops as lambda + closure.
4. Codegen maintains `with_fn_active: HashMap<String, FunctionValue>`
   and `with_ctl_active: HashMap<String, FunctionValue>` populated at
   statement encounter; effect op call sites emit indirect dispatch
   through these tables.
5. Interpreter maintains parallel `ctl_handlers` / `fn_handlers` stacks
   with captured envs.

## Target model

1. **Effect rows.** Every function carries a statically inferred row
   of effects it can perform. `effect_inference.rs` already starts this
   — it needs to be made total and load-bearing.
2. **Handler propagation.** At every call site of a function with a
   non-empty row, the caller's handler context is propagated.
3. **Specialization (handler-aware monomorphization).** A function
   `fn greet(name)` with effect `Log` becomes a family
   `greet__handle_print_log`, `greet__handle_prefix_log`, etc. — one
   per distinct handler context observed at call sites.
4. **Direct dispatch.** Every effect op call site resolves at compile
   time to a direct call into the matching specialization of the
   handler body. No table lookup.
5. **CPS becomes inlined.** Continuations are still threaded as
   explicit arguments, but the receiver knows statically which body to
   run. Koka calls this "tail-resumptive" optimization for the common
   single-resume case.

## Constraints

- Handlers are declared with the `handler` keyword (distinct from
  `class`). A handler is **never** a value — it can only be referenced
  by name at a `handle Name(args)` install site. Handler-as-value
  (`handle some_var`) is no longer supported in any mode. This was a
  deliberate language design call to enable full static dispatch
  without runtime fallback.
- `handler Name(params)? : Effect { ops }` carries optional params
  evaluated lexically at install time. Op bodies see them as captured
  variables.
- `class` returns to meaning "nominal product type, instantiable as a
  value." The `class : Effect` form is removed.
- Multi-resume requires more compile-time machinery: each resume call
  site becomes a tail call into the saved continuation, where the
  continuation itself is a specialized function. Doable but adds to
  scope.
- Existing effect tests have to migrate as a class — there's no way to
  partially convert without breaking the dispatch model. PRs must
  preserve test invariants at every commit point.

## Phases

### Phase 1 — Effect row foundation

Make effect inference total: every function's effect row is recorded
and queryable. Strengthen the row type so it carries which specific
handler installation site each effect originates from (not just "this
function uses Log" but "this function uses the Log installed at
stmt_id N"). Confirm the existing `effect_inference::infer_and_check`
catches every effect use.

This phase makes no behavioral changes. It instruments the foundation.

**Estimated effort:** 1-2 weeks.

### Phase 2 — Static dispatch when resolvable

At every effect op call site, do a backward flow analysis: can we
statically identify the unique handler installation site that will
service this op when the program runs?

If yes, emit a direct call into that handler's function.
If no, fall back to the existing dispatch table.

This is incremental — the dispatch table stays, but it becomes
unused for the easy cases. All existing tests should still pass
because the fallback handles whatever the analysis can't prove.

**Estimated effort:** 2-3 weeks.

### Phase 3 — Handler-aware specialization

Extend `monomorphize.rs` to specialize over handler contexts. The
existing pass groups call sites by concrete argument types; extend it
to also group by handler installation context.

For each `greet(name)` call observed at call site with handler context
X, emit `greet__X` if not already present. Rewrite the call site to
target the specialized name. Recursively specialize the body's effect
op calls under context X.

**Estimated effort:** 2-3 weeks.

### Phase 4 — Drop the dispatch table

After Phase 3, most programs will have no remaining dynamic call sites.
Add a verification pass: assert `with_fn_active`/`with_ctl_active` are
empty for the program. If so, strip the table emission entirely.

Programs that still need the table (handler-as-value cases, or
analysis-incompleteness fallbacks) keep it as documented opt-in.

**Estimated effort:** 1 week.

### Phase 5 — CPS inlining

Once specialization is in place, the CPS transform's job changes.
Instead of emitting CPS calls that route through a dispatch table,
emit CPS calls where the continuation receiver is statically known.

For tail-resumptive handlers (the common single-resume case),
optimize the CPS further: inline the handler body at the op call
site rather than emitting a separate function.

**Estimated effort:** 2-4 weeks. This phase is optional polish; the
program is already correct after Phase 4.

### Phase 6 — Migration

Move all existing effect tests to the new model. Mostly mechanical:
the tests should be already passing if phases 1-4 preserved
invariants. This phase confirms it.

**Estimated effort:** 1-2 weeks of bug-finding and stabilization.

## Total

9-16 weeks of focused work. The phases are independently shippable —
each ends with a working compiler and unchanged test counts. Roll
back at any phase if priorities change.

## Decision points along the way

- **End of Phase 1:** Confirm effect inference is solid. If holes
  remain (e.g., effects through closures, recursive functions), they
  bite hard in later phases. Maybe pause here for cleanup.
- **End of Phase 2:** Measure how many call sites the analysis catches
  vs falls back. If <80%, the analysis needs strengthening before
  Phase 3 specialization pays off.
- **End of Phase 3:** Decide whether to drop the dispatch table
  (Phase 4) or keep it permanently as fallback. Some languages
  (Eff, Effekt) explicitly keep dynamic dispatch; you can too if
  static + dynamic coexist cleanly.
- **End of Phase 5:** Re-evaluate whether tail-resumptive inlining
  was worth the complexity. If not, revert and document the model
  as "specialized but boxed continuations."

## Out of scope for this work

- Handler-as-value (`handle some_var`): either deprecated or kept as
  interpreter-only. Decide explicitly before Phase 2.
- Performance benchmarking: Phase-by-phase the IR will improve;
  measure once at the end vs. before.
- Documentation update: User-facing docs change after Phase 4. Don't
  pre-write them.

## Open design questions

- **Recursive functions with effects.** `fn iterate(f)` where `f` may
  do effects: the effect row of `iterate` depends on the row of `f`.
  Standard effect-system territory; you'll need to copy a known
  solution (Koka's effect polymorphism via row variables, or
  Effekt's first-class capability variables).
- **Closures with effects.** A lambda created in one handler context,
  called in another. The lambda's effect row is fixed at creation;
  the handler context is fixed at call site. Probably the lambda must
  carry its handler context (small closure record), but the *dispatch*
  is still static given that record.
- **Multi-resume.** Specializing over the continuation's later
  behavior is non-trivial. Punt for Phase 5.
