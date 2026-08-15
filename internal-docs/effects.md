# Algebraic effects — bootstrap2

Status: **implemented.**

| Piece | State |
|-------|-------|
| Effect rows, inference, discharge | `lib/types.ml`, `lib/typecheck.ml` |
| `fn` operations | evidence passing (`lib/cps.ml`) |
| Tail-resumptive `ctl` | compiled as `fn` — bind-inversion |
| Aborting `ctl`, multi-shot `resume` | continuation passing |
| Effect inside a loop | rejected — needs a loop continuation |
| Effect inside `&&`/`\|\|` | rejected — hoisting would break short-circuiting |
| Effectful function as a value | rejected — needs row monomorphization |

Running: `log`, `ask`, `multi_handle`, `exception`, `delim`, `flip`, `recover`.

Two pieces of work that meet in the middle: effect rows in the type system, and a selective CPS pass that consumes what the checker inferred.

## Planned: `print` is an effect

`print` and `clock` are the two entries in `builtins.ml` that exist because they do I/O. The plan for `print` is an effect declared in the prelude with a handler installed at the program's root:

```cronyx
effect out { fn print(s: string): unit; }
```

Output then goes wherever the innermost handler sends it — a file, a buffer, a test harness — without a writer threaded through every call and without `System.out` at each use. It is the canonical use of the feature.

Three consequences, recorded so they are not rediscovered:

- **An operation has a declared signature**, so `print` takes one string. That removes the variadic question rather than answering it — see step 8 of [Remediation of Builtins](Remediation%20of%20Builtins.md) — and makes string interpolation the ergonomic companion.
- **Printing becomes visible in a row.** Any function that prints carries `<out>`, so the program root must be handled implicitly or every program starts with a `run`.
- **It depends on containment**, which landed as step 11 of the same document. Under row equality a function that printed could not call anything.

## Reference architecture

**Koka**, per Leijen's row-polymorphic effect types. Where this document is silent or a detail turns out to be underspecified, do what Koka does rather than inventing something.

Cronyx's surface syntax is already Koka-shaped — its `fn` and `ctl` operations are Koka's `fun` and `ctl`, the tail-resumptive and general cases — so the semantics carry over with little translation.

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

One `run` may carry several `handle` clauses (`tests/effects/multi_handle`), and `run` blocks nest (`tests/effects/delim`).

**An effect may take type parameters**, which its operations share:

```cronyx
effect Store<T> {
    fn put(v: T);
    fn take(): T;
}
```

A parameter is one type variable for the whole declaration, and a call site is what settles it. An operation could leave a return unannotated and get the same variable; naming it is what lets two operations agree on it, and what says so to a reader.

**`final ctl` is what makes a non-returning operation usable anywhere.** Its handler cannot resume, so no value is ever handed back — and that is exactly the condition under which the operation's result may be read differently at every call site:

```cronyx
effect Error {
    final ctl throw(msg: string);
}

fn as_int(s: string) -> int    { … return throw("not an int"); }
fn as_word(n: int) -> string   { … return throw("not zero"); }
```

Both `return throw(…)` check, at `int` and at `string`, with no type parameter to settle. Declaring `throw` with a parameter instead ties every call site to one type and rejects the second — the two are not interchangeable, and this is the one that works.

Quantifying the result of an ordinary `ctl` would be unsound: the handler would owe a value of a type it never agreed to. `final ctl` is the promise that no such value is ever demanded, and `resume` inside one is an error rather than a wish.

This is Koka's design. It also means Cronyx needs no bottom type: `never` would say the same thing about the value where `final ctl` says it about the handler, and only one of the two can be checked.

**A row entry carries what the effect was instantiated at.** `Yield<int>` and `Yield<string>` are different entries, so one program may hold both, and which handler catches a `yield` follows from its instantiation rather than from its name alone. That is Koka's design: the entry is a label with arguments, not a label.

Three things follow. An operation's scheme quantifies its effect's parameters, so each call site instantiates. Finding a label in a row unifies the arguments rather than comparing them, which is what settles those variables. And a handler discharges *one* instantiation, chosen by unifying against what its arms turn out to take — `handle Yield { ctl yield(x) { print(x); … } }` handles `Yield<int>` or `Yield<string>` depending on the block it is on. `effects/generic/generic` uses both, and `effects/generic/nested` puts one inside the other.

**A row holds at most one instantiation of an effect.** A second is unified with the first, so a single function performing `Store<int>` and `Store<string>` is an error rather than a row with two `Store` entries. Nested `run` blocks are unaffected, since each has its own row.

## Operation kinds

| | `fn op` | `ctl op` | `final ctl op` |
|---|---|---|---|
| After the arm returns | resumes automatically with that value | does **not** resume | does **not** resume |
| Explicit `resume` | not used | resumes; may be called more than once | rejected |
| Falling off the arm | resumes | aborts the enclosing `run` block | aborts the enclosing `run` block |
| Result type | one per program | one per program | **fresh at every call site** |

`log` and `ask` show `fn` ops. `exception` shows abort — `throw`'s handler prints and `"unreachable"` never runs. `flip` shows multi-shot resumption:

```cronyx
ctl flip() { resume false; resume true; }
```

producing four lines of output from two `flip` calls.

## Why CPS and not OCaml 5 handlers

`bootstrap2` runs on OCaml 5.5, whose handlers would otherwise map onto this almost directly. They cannot be used: **OCaml's continuations are one-shot** and `flip` resumes the same one twice. CPS continuations are ordinary closures, and multi-shot for free.

Independently, CPS is the representation a compiler backend needs, so the pass is not throwaway the way an interpreter-only mechanism would be.

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

Effect labels rather than operation names, because `handle exception` discharges the whole effect — there is no syntax for discharging one operation and leaving a sibling outstanding. This requires a `handle` clause to implement **every** operation its effect declares; make that a checked error.

Duplicate labels are what make nested handlers for the same effect work: `handle exception` peels *one* occurrence, so two nested `exception` handlers are distinguished rather than the inner one silently discharging both. Order is irrelevant between distinct labels; duplicates are preserved.

This diverges from the Rust bootstrap, which rows over operation names (`type_checker.rs` inserts the callee into the row).

### Unification, not subsumption

Koka's design is row polymorphism *instead of* subtyping, and duplicate labels are precisely what let unification alone suffice. Unifying `{exn | r₁}` against `{r₂}` instantiates `r₂ := {exn | r₃}` rather than failing, which is what lets a pure function be passed where an effectful one is expected — the ergonomics of subsumption without a subtyping relation or coercion insertion.

Row unification is by rewriting: to unify `{l | r₁}` with `r₂`, rewrite `r₂` into the form `{l | r₂'}` and continue with `r₁` and `r₂'`. It fails when `r₂` is closed and has no `l`.

If this proves too strict in practice, add widening at call sites later. Going the other direction — removing subtyping once inference depends on it — is much harder.

**Call sites diverged from this, and the reason is the CPS pass rather than the type system.** A call requires the callee's row to be *contained* in the caller's rather than unified with it. Unification is enough for Koka because evidence arity is decided from a function's definition; here the CPS pass reads the annotation the call site inferred, so a row that subsumed correctly still produced a callee annotated with effects it does not perform, and an evidence argument it has no parameter for. Containment leaves the callee's annotation alone. Unification survives in one place — a forward or recursive reference whose row is not yet known — and the rest of the row machinery, including rewriting and duplicate labels, is unchanged.

### Open, closed, and defaulting

A written row precedes the return type — `fn f(): <log> unit` — because type arguments made the trailing position ambiguous: `Array<int>` and `unit <log>` are the same shape, and nothing distinguishes a type argument list from a row. Koka writes them the same way. Printed types follow, so `typeof` reads `(string) -> <logger> unit`.

Inferred function types get an open row. A written annotation is closed, so `fn f(): <> unit` is a contract that `f` is pure; without that rule there is no way to say so.

An unconstrained row variable **defaults to the empty row** at `resolve` time, exactly parallel to unconstrained numeric variables defaulting to `int`. Without this, `typeof(double)` would print `(int) -> <e5> int`. With it, pure functions print no row at all and effectful ones print theirs.

### `resume` and answer types

The whole difficulty in typing `resume` is the answer type — what the `run` block ultimately produces. Because `run` is a statement, that answer type is always `unit`, so there is nothing to thread and no delimited-continuation typing to implement. `resume e` simply checks `e` against the operation's declared return type: `ctl flip(): bool` gives `resume false;`, and `ctl throw(...): unit` gives bare `resume;`.

Two errors worth checking: `resume` outside a `ctl` arm, and `resume` inside a `fn` arm, which auto-resumes and must not also resume explicitly.

When `run` becomes an expression — likely, for `var x = run { ... }` — that is the moment to introduce answer types. The row machinery will already exist.

### Handler arms

Checked like function bodies, with their rows unioned into the enclosing `run`'s row *after* that run's discharge. A `ctl throw` arm that itself calls `throw` therefore propagates to an enclosing handler rather than catching itself, which is both the standard rule and the one that avoids a handler looping into itself.

## A `return` inside a branch

`CPS` carries two continuations, not one. `return` calls the function's; the statement after this one runs under whatever the enclosing construct supplies — a join for a branch, a fresh continuation for a block. They coincide at the top of a body and part company as soon as anything nests.

They used to be one, and a `return` inside an `if` was left as an ordinary return by the evidence-only translation, so its value never reached the continuation and the call produced nothing at all:

```cronyx
fn pick(n: int) -> int {
    if (n == 0) { return 42; }   // silently lost
    return ask();
}
```

Only a program whose handler was not tail-resumptive could reach it, which is why no fixture had — `effects/recover` is this shape with a `resume`, and passes. `effects/nested_return` is the one that would have caught it. A statement holding a `return` is now converted whether or not anything in it suspends, which is what makes the two continuations differ in the first place.

## What each translation costs

Two translations, chosen per effect. **Evidence passing** hands each operation's handler down as an extra parameter; the call is direct and the shape of the surrounding code is untouched. **CPS** rewrites control flow so a continuation can be captured. Only the second is expensive, and only the second restricts where an effect may appear.

An effect takes the second only when some handler for it actually resumes out of tail position. So:

| Handler | Translation |
|---|---|
| all `fn` operations | evidence only |
| `ctl` resuming in tail position | evidence only — bind inversion rewrites it to a return |
| `final ctl` | evidence only, plus an unwind |
| `ctl` otherwise | CPS |

**`final ctl` earns its place here.** It never resumes, so it needs no continuation — only a way out, which is an unwind: `Abort` in the converted tree, an exception in the interpreter, and a jump wherever this is eventually compiled. Costing nothing on the path that does not throw is the usual property of exceptions, and it means the most common effect in a real program — failing — keeps its function's control flow exactly as written.

That is also why a `final ctl` reaches places a resuming handler cannot: inside a loop, inside a function value. Nothing was rewritten, so there is nothing that had to know the shape. `effects/final/in_loop` covers both.

## A loop that suspends

A `while` whose body suspends becomes a recursive continuation: the body's *what runs next* is the loop itself.

```
fn after(x) { … the rest … }
fn again(x) { if (cond) { … body, ending in again() … } else { after() } }
again()
```

Resuming carries on with the next iteration, and resuming twice runs it twice, which is what makes a multi-shot handler work inside a loop. `effects/logic/simple_guard` is the fixture.

## What a `run` block delimits

The body ends the delimited computation rather than continuing the program. Completing it returns to whoever entered it — the `resume` that re-entered it, or the block itself — and what follows the block runs once, after the handler is done.

That is what makes a multi-shot handler behave. Resuming twice runs the body twice and the sequel once:

```cronyx
run { print("saw", choose([1, 2])); } handle logic { … resume x … }
print("done");
```

The body's terminal continuation used to be the rest of the program, so `done` printed once per resumption plus once more. `effects/flip` is multi-shot and did not catch it, because nothing follows its `run` block; `effects/multishot_sequel` does.

Statements after a block stay where they were written rather than being wrapped in a continuation, which is what lets a `return` among them return from the function it is in.

**A `return` out of the block itself is rejected.** Once an operation suspends, the rest of the body is a continuation function, and returning from *that* is not returning from the enclosing function. Travelling back out through the handler that resumed into it is what a continuation is for, and the enclosing function has none — handling the effect discharged its row, so nothing converted it. Saying so beats the alternative, which was answering with whatever the code after the block returned.

## Pipeline placement

CPS runs late, after everything that could introduce a call — see [architecture](architecture.md) for the full order.

The checker's rows *are* the CPS marking. The Rust bootstrap runs a separate `mark_cps` analysis to recover the same information.

It has to run after `Resolve` in particular, because an operator can be an ordinary function and an ordinary function can perform effects. `a + b` where `op +` performs `logger` is only visibly a call once elaboration has made it one.

Marking should key on **`ctl` reachability, not on a non-empty row**. `fn` operations are tail-resumptive: they resume exactly once with the arm's value, so they are a plain call to the evidence passed in and need no continuation. This is Koka's bind-inversion, and it means `log`- and `ask`-style effects cost nothing at runtime.

Following the discipline the other passes use, the effect constructs (`effect` declarations, `run`/`handle`, `resume`) live in a fragment present in the parsed, desugared, typed, and reflected trees and **absent** from the post-CPS tree — so the interpreter needs no effect support at all, only closures and calls, which it already has.

## Deferred

Handler aliasing, syntax for writing a row variable in an annotation, and whether effect declarations nest — see [TODO](TODO.md). None of them block what is implemented.

Explicit comptime arguments will hit the same ambiguity in expression position, where `pair<int>(1, 2)` and `a < b` cannot be told apart and `a<b` is idiomatic. Nothing decided here helps there; it needs a turbofish-style marker or inference-only type arguments.
