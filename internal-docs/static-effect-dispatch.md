# Static Effect Dispatch via Metaprocessing Reduction

Status: design intent, not started. Handler keyword work (separate
`handler` from `class`) is complete and a prerequisite.

## Goal

Treat handler installation as a meta-time reduction inside
`meta_processor`. Every `run { body } handle Name(args)` reduces by
looking up the named `HandlerDef`, substituting params with args, and
inlining op bodies at the matching call sites inside the run block.

The RuntimeAst that emerges from meta_processor has **no handler
concepts** — no `HandlerDef`, `WithFn`, `WithCtl`, and no
`with_fn_active` / `with_ctl_active` dispatch tables downstream.
`Resume` stays because it's a continuation primitive (not a handler
primitive); `cps_transform` continues to lower it to a tail call into
the captured continuation, same as today. The change is in which
*function* the resume is reached from, not in resume itself.

This is the Koka compilation strategy expressed in your existing
pipeline. The win is (a) performance — effect ops become as cheap as
regular function calls, (b) reasoning clarity — inspect the IR and see
exactly what code runs, (c) simpler downstream — cps_transform and
codegen lose a whole category of code paths.

The cost is (a) compile-time work — substantial. (b) constraints on
what programs are expressible — handlers must be statically named at
their install site (already enforced by the `handler` keyword in #11).
(c) extends meta_processor with a new kind of reduction.

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
   — it needs to be made total and load-bearing before any reduction
   can run.
2. **Handler propagation at meta time.** When meta_processor encounters
   a `RunWith { handler_name, args, body }`, it looks up the named
   HandlerDef in scope, binds params → args at the install site, and
   walks `body` looking for effect-op calls handled by this handler.
3. **Specialization.** For every function transitively called from
   `body` that performs a handled op, meta_processor emits a
   specialized copy `fn_name__handle_Name` where each op call is
   replaced by the inlined op body (closed over the install-site
   bindings). Call sites in body are rewritten to the specialized
   names.
4. **Inlined op bodies.** Inside specialized functions, every handled
   op call becomes a direct sequence: bind op args, run op body,
   reach `resume` → tail-call the saved continuation.
5. **CPS still threads continuations.** `cps_transform` runs on the
   already-reduced AST. It only worries about resumption flow, not
   handler routing. No `with_fn_active` / `with_ctl_active` tables in
   codegen.

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

## TDD strategy

- **Layer 1 — behavioral preservation.** `tests/effects/inline/*.cx`
  programs covering specialization edge cases. All pass on the
  interpreter today; serve as a regression net through the rewrite.
- **Layer 2 — pass-output assertions.**
  `bootstrap/tests/handler_reduction_tests.rs`: parse + stage + meta-
  process a small program, assert the resulting RuntimeAst is
  handler-free, specialized fn names exist, call sites point at
  specialized targets.
- **Layer 3 — negative tests.** Programs where an op call site is
  statically unresolvable should fail with a clear error from effect
  inference.

## Chunks

Each chunk ends with a working compiler. Each fits one focused
session (1–3 hours). Sequential dependencies — earlier chunks unblock
later ones. Acceptance criterion is stated per chunk.

### Test scaffolding

**Chunk 1: Layer 1 test files.** Write 6 `.cx`/`.txt` pairs under
`tests/effects/inline/` covering: `simple_op`, `transitive`,
`recursive`, `two_handlers`, `lambda_with_effect`, `nested_handlers`.
Register in `script_integration.rs`. Done when all 6 new tests pass
on the interpreter.

### Effect inference foundation

**Chunk 2: Effect inference audit.** Read `effect_inference.rs`.
Write `bootstrap/tests/effect_inference_completeness.rs` asserting
every function in a sample program has a recorded effect row. Identify
gaps; do not fix yet. Done when the test exists and its failure
message lists every gap.

**Chunk 3: Effect inference — direct calls.** Fix gaps for direct
function calls (`fn f() { g(); }` — f's row includes g's row). Done
when completeness test passes for programs without function values
or recursion.

**Chunk 4: Effect inference — recursion + lambdas.** Recursive
functions and lambda-with-effects. Function values last. Done when
completeness test passes for all Layer 1 test programs.

### Reduction MVP

**Chunk 5: No-op reducer framework.** Add
`bootstrap/src/semantics/meta/handler_reduce.rs` exposing
`reduce_handlers(ast: RuntimeAst) -> RuntimeAst`. Wire into
`meta_processor` after the current evaluation step; returns the AST
unchanged. Write `bootstrap/tests/handler_reduction_tests.rs` with
one test asserting "no-op pass-through." Done when existing suites
still pass and the new test passes.

**Chunk 6: Inline single direct fn-op call.**
`run { op(args); } handle Name { fn op(p) { body } }` reduces to
`run { body[args/p] }`. Direct call from run body only; fn-shaped
ops only; no ctl, no resume. Done when `simple_op.cx` passes through
the reducer and the reduced AST has no `WithFn`/`HandlerDef` for this
case.

**Chunk 7: Inline ctl op + resume.** Same as Chunk 6 but for `ctl`
ops with `resume`. Inlined body's `resume` becomes a tail call to
the rest of the run body (CPS transform does the actual lowering).
Done when simple `ctl` tests pass; resume still works downstream.

### Specialization

**Chunk 8: Single transitive call.** `run { foo(); } handle Name`
where `foo()` does an op. Emit `foo__handle_Name` with the op
inlined; rewrite the call site. Done when `transitive.cx` passes
with reduced output asserted.

**Chunk 9: Recursion.** `fn f() { op(); f(); }` — the recursive
call inside `f__handle_Name` must point at `f__handle_Name`. Done
when `recursive.cx` passes.

**Chunk 10: Same fn, multiple handlers.** A function called from two
different `handle` contexts gets two specializations. Done when
`two_handlers.cx` passes.

**Chunk 11: Lambdas with effects.** A lambda created inside a
handler block performs an op. The lambda specializes at create-site,
capturing the handler context. Done when `lambda_with_effect.cx`
passes.

**Chunk 12: Nested handlers.** `run { run { ... } handle B } handle A`.
Propagate handler contexts inside-out. Op calls inside the inner
block see B first, then A. Done when `nested_handlers.cx` passes.

### Migration + cleanup

**Chunk 13: Migrate registered effect tests.** Verify every existing
registered effect test (`handler.cx`, `stream.cx`, `async.cx`) still
passes after reduction. Done when `script_integration` is 100% and
`compile_integration` does not regress.

**Chunk 14: Drop `WithFn` from runtime.** Remove
`RuntimeStmt::WithFn` entirely. `handler_transform.rs` shrinks (no
`WithFn → Lambda` conversion). Codegen drops `with_fn_active` table.
Done when the code compiles, tests pass, and the unused fields are
gone.

**Chunk 15: Drop `WithCtl` from runtime.** Same as 14 for `WithCtl`.
Codegen drops `with_ctl_active`. `transform_ctl` shrinks or
disappears. Done when the rewrite is complete; runtime AST is
genuinely handler-free.

**Chunk 16: Drop `HandlerDef` from runtime.** The decl shouldn't
survive into RuntimeAst at all; it's only used during meta-reduction.
Move to MetaStmt-only. Done when `RuntimeStmt::HandlerDef` doesn't
exist.

## Total

~16 chunks at 1–3 hours each. Some may collapse if trivial; some may
split if hairy. Should slot into 12–16 focused sessions.

Each chunk ends with a working compiler, unchanged test counts (or
better), and a single commit. Roll back at any chunk if priorities
change.

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
