# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

`bootstrap2/` (OCaml) is the compiler. `bootstrap/` (Rust) is the one it
replaced — it is kept for reference, is not in CI, and no longer compiles the
current `stdlib/` or `tests/`.

All commands run from `bootstrap2/` unless noted.

```bash
# Build
dune build

# Test — the whole fixture suite
dune test
dune test --force            # again, ignoring dune's cache

# Run a program (paths are relative to the repo root)
dune exec --root . bin/main.exe -- ../tests/core/print/hello.cx
```

**CLI flags**, each printing one stage and then running:
- `--dump-source` — the source as the scanner received it
- `--dump-tokens` — the token stream
- `--dump-ast` — the parsed tree
- `--dump-types` — the tree after checking, every node annotated
- `--dump-code` — the program as Cronyx after metaprocessing, which is how to
  read what a `gen` or a deriver produced

## Working in this repo

`main` is protected: it takes no direct pushes, so every change lands through a
pull request. Branch, push, open the PR, and let the `test` check run.

**Do not watch CI.** Opening the PR is where the work ends. Do not poll
`gh pr checks`, do not start a monitor on the run, and do not report back on
whether it went green — check only when asked to.

**Keep a branch current with `main`.** Merge `main` in before opening a PR and
again whenever `main` moves — a branch cut from a commit that has since been
superseded reviews against the wrong thing, and its checks pass against a tree
nobody will merge. Two PRs open at once is the case that bites: the second one
is stale the moment the first lands.

## Code style

Applies to `bootstrap/` (Rust), `bootstrap2/` (OCaml), and the `.cx` fixtures in `tests/`. A fixture is read alongside its `.txt`, which already says what the program produces, so a header explaining what it demonstrates is the same noise as anywhere else.

**A comment is the exception.** Start from the assumption that it should not exist and make it earn its place. Two kinds do:

1. **A trap** — a subtle invariant, an ordering that has to hold, why the obvious alternative fails. The test is whether a competent reader would lose an hour without it.
2. **A label** over a long list, like `(* One or two character tokens. *)` in `token.ml`.

Everything else is noise, including all of these:

- what a function does — the name and signature say it
- what a pass consumes and produces — the types say it
- why a design was chosen — that belongs in `internal-docs/`
- a restatement of the line below, however rephrased
- narrating a branch of a `match` that already reads clearly

A file may carry a one- or two-line header when its purpose is not obvious from its name. Most files do not need one.

Rewriting a comment to be more insightful is usually the wrong fix. Deleting it is the right one.

**Never describe development in code.** No stages, milestones, plan phases, what a rewrite replaced, or what is coming next. Write for someone reading the file in three years with no memory of how it was built — "Stage 1 → stage 2" means nothing to them, and the types (`desugared_expr` → `typed_expr`) already say what a pass consumes and produces. That material belongs in conversation or `internal-docs/`, never in a source file.

**Explain why, not how.** The code shows how. A comment is for the reasoning a reader cannot recover from it.

```ocaml
(* Bad — development framing, and the types already say this. *)
(* Stage 1 → stage 2: Hindley-Milner inference over the desugared tree.
   Three passes, in this order: hoist, infer, resolve. *)

(* Good — a trap that costs an hour to rediscover. *)
(* Drop the monomorphic binding [hoist] installed: leaving it in place makes the
   function's own variables count as free in the enclosing scope, so nothing is
   ever quantified. *)
```

## Architecture

Cronyx is a statically-typed, metaprogramming-first language.
[internal-docs/Architecture.md](internal-docs/Architecture.md) is the authority
on the pipeline and carries a heading per pass; the order itself lives in
`bootstrap2/lib/compile.ml` and nowhere else.

```
Scanner → Parser → Loader → Metaprocess → Desugar → Value monomorphize
  → Typecheck → Type monomorphize → Resolve → Reflect → CPS → Verify → Interp
```

### Key distinctions

**One AST, several stages of it.** `Ast` is parameterized by its annotation and
by what a statement holds, so `desugared_stmt`, `typed_stmt`, `resolved_stmt`
and `cps_stmt` are the same tree at different points. A construct that has been
lowered is gone from the type, which is what stops a later pass from meeting it.

**Two monomorphizers.** `Value_mono` substitutes comptime *value* parameters and
runs before checking, because a value can decide a type. `Type_mono` copies
generic bodies per concrete type and runs after, because inference is what says
which types those are.

**Metaprocessing is the pipeline calling itself.** A `meta` block is compiled by
the passes above and run by the interpreter, which is why `Compile` holds
everything from `Desugar` on and `Pipeline` holds the rest.

**Dispatch is static.** Operators are traits, `Resolve` turns every impl into
plain functions, and there are no trait objects — see
[internal-docs/Elaboration.md](internal-docs/Elaboration.md).

**CPS is selective.** Only functions performing control effects are rewritten,
and each effect gets evidence passing or full continuations depending on whether
its handlers resume in tail position.

### Test fixtures

`tests/` (repo root, not `bootstrap2/test/`) holds `.cx` sources paired with
what they must produce: a `.txt` of expected stdout, a `.err` of expected
diagnostics, or a `.rt` for one that runs and then fails.

Every fixture must be named by a list in `bootstrap2/test/test_bootstrap2.ml`,
and a fixture no list names is a test failure of its own. A feature that does
not work yet goes in `expected_failing` with the work it waits on — the suite
asserts it still fails, and says so the moment it starts passing. Write the
fixture before the feature.
