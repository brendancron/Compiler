# Algebraic effects — bootstrap2

Status: **rows implemented; CPS partial.**

| Piece | State |
|-------|-------|
| Effect rows, inference, discharge | done (`lib/types.ml`, `lib/typecheck.ml`) |
| `fn` operations | run, via evidence passing (`lib/cps.ml`) |
| Tail-resumptive `ctl` | run, compiled as `fn` (bind-inversion) |
| Aborting `ctl` | rejected — needs continuations |
| Multi-shot `resume` | rejected — needs continuations |
| Effectful function as a value | rejected — needs row monomorphization |

Two pieces of work that meet in the middle: effect rows in the type system, and
a selective CPS pass that consumes what the checker inferred.

## Reference architecture

**Koka**, per Leijen's row-polymorphic effect types. Where this document is
silent or a detail turns out to be underspecified, do what Koka does rather than
inventing something.

Cronyx's surface syntax is already Koka-shaped — its `fn` and `ctl` operations
are Koka's `fun` and `ctl`, the tail-resumptive and general cases — so the
semantics carry over with little translation.

## Surface syntax

Fixed by the fixtures in `tests/effects/`:

```cronyx
effect exception {
    ctl throw(msg: string): unit;
}

fn divide(n, d) {
    if (d == 0) { throw("division by zero"); }
    return n / d;
}

run {
    print(divide(5, 0));
    print("unreachable");
} handle exception {
    ctl throw(msg) { print(msg); }
}
```

One `run` may carry several `handle` clauses (`tests/effects/multi_handle`), and
`run` blocks nest (`tests/effects/delim`).

## Operation kinds

| | `fn op` | `ctl op` |
|---|---|---|
| After the arm returns | resumes automatically with that value | does **not** resume |
| Explicit `resume` | not used | resumes; may be called more than once |
| Falling off the arm | resumes | aborts the enclosing `run` block |

`log` and `ask` show `fn` ops. `exception` shows abort — `throw`'s handler prints
and `"unreachable"` never runs. `flip` shows multi-shot resumption:

```cronyx
ctl flip() { resume false; resume true; }
```

producing four lines of output from two `flip` calls.

## Why CPS and not OCaml 5 handlers

`bootstrap2` runs on OCaml 5.5, whose handlers would otherwise map onto this
almost directly. They cannot be used: **OCaml's continuations are one-shot** and
`flip` resumes the same one twice. CPS continuations are ordinary closures, and
multi-shot for free.

Independently, CPS is the representation a compiler backend needs, so the pass
is not throwaway the way an interpreter-only mechanism would be.

## Decisions

| Question | Decision |
|----------|----------|
| Row elements | Effect labels, not operations |
| Duplicate labels | Allowed — Koka's scoped rows |
| Subtyping | None. Row polymorphism with unification |
| Inferred rows | Open (fresh tail variable) |
| Written rows | Closed |
| Unconstrained row at `resolve` | Defaults to empty |
| `run` | Statement |
| `resume` | Statement; checked against the operation's return type |
| Handler arms | Checked outside their own handler |
| `typeof` | Prints the row |

### Rows

```ocaml
type row =
  { labels : string list          (* effect names; duplicates significant *)
  ; tail : row_var ref option     (* None = closed, Some = open *)
  }
```

Effect labels rather than operation names, because `handle exception` discharges
the whole effect — there is no syntax for discharging one operation and leaving
a sibling outstanding. This requires a `handle` clause to implement **every**
operation its effect declares; make that a checked error.

Duplicate labels are what make nested handlers for the same effect work:
`handle exception` peels *one* occurrence, so two nested `exception` handlers are
distinguished rather than the inner one silently discharging both. Order is
irrelevant between distinct labels; duplicates are preserved.

This diverges from the Rust bootstrap, which rows over operation names
(`type_checker.rs` inserts the callee into the row).

### Unification, not subsumption

Koka's design is row polymorphism *instead of* subtyping, and duplicate labels
are precisely what let unification alone suffice. Unifying `{exn | r₁}` against
`{r₂}` instantiates `r₂ := {exn | r₃}` rather than failing, which is what lets a
pure function be passed where an effectful one is expected — the ergonomics of
subsumption without a subtyping relation or coercion insertion.

Row unification is by rewriting: to unify `{l | r₁}` with `r₂`, rewrite `r₂` into
the form `{l | r₂'}` and continue with `r₁` and `r₂'`. It fails when `r₂` is
closed and has no `l`.

If this proves too strict in practice, add widening at call sites later. Going
the other direction — removing subtyping once inference depends on it — is much
harder.

### Open, closed, and defaulting

Inferred function types get an open row. A written annotation is closed, so
`fn f(): unit` is a contract that `f` is pure; without that rule there is no way
to say so.

An unconstrained row variable **defaults to the empty row** at `resolve` time,
exactly parallel to unconstrained numeric variables defaulting to `int`. Without
this, `typeof(double)` would print `(int) -> int <e5>` and break
`tests/reflection/typeof_fn`, which both bootstraps currently agree on. With it,
pure functions print no row and effectful ones print `<exn>`.

### `resume` and answer types

The whole difficulty in typing `resume` is the answer type — what the `run`
block ultimately produces. Because `run` is a statement, that answer type is
always `unit`, so there is nothing to thread and no delimited-continuation
typing to implement. `resume e` simply checks `e` against the operation's
declared return type: `ctl flip(): bool` gives `resume false;`, and
`ctl throw(...): unit` gives bare `resume;`.

Two errors worth checking: `resume` outside a `ctl` arm, and `resume` inside a
`fn` arm, which auto-resumes and must not also resume explicitly.

When `run` becomes an expression — likely, for `var x = run { ... }` — that is
the moment to introduce answer types. The row machinery will already exist.

### Handler arms

Checked like function bodies, with their rows unioned into the enclosing `run`'s
row *after* that run's discharge. A `ctl throw` arm that itself calls `throw`
therefore propagates to an enclosing handler rather than catching itself, which
is both the standard rule and the one that avoids a handler looping into itself.

## Pipeline placement

```
Typecheck (rows)  →  Reflect  →  CPS  →  Interp
```

The checker's rows *are* the CPS marking. The Rust bootstrap runs a separate
`mark_cps` analysis to recover the same information.

Marking should key on **`ctl` reachability, not on a non-empty row**. `fn`
operations are tail-resumptive: they resume exactly once with the arm's value, so
they are a dynamic call against a handler stack and need no continuation. This is
Koka's bind-inversion, and it means `log`- and `ask`-style effects cost nothing at
runtime.

Following the discipline the other passes use, the effect constructs (`effect`
declarations, `run`/`handle`, `resume`) live in a fragment present in the parsed,
desugared, typed, and reflected trees and **absent** from the post-CPS tree — so
the interpreter needs no effect support at all, only closures and calls, which it
already has.

## Open questions

- **Does `handle` bind the effect by name only, or can it be aliased?** Nested
  handlers of the same effect are distinguished positionally today.
- **Row syntax in annotations.** `fn f(): unit <log>` covers the closed case.
  Writing a row *variable* needs syntax and can wait until a signature must be
  row-polymorphic explicitly.
- **Do effect declarations nest or scope?** Currently top-level only.
