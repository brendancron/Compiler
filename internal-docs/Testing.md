# Testing

`cx test` runs the `@test` functions in a package. It is unrelated to `tests/`, which is the compiler's own golden-file suite and belongs to `dune test`; the two share a name and nothing else.

```cronyx
fn add(a: int, b: int): int { return a + b; }

@test
fn adds() {
    assert(add(1, 2) == 3, "1 + 2 is 3");
}
```

```
ok   adds
FAIL reports_its_output
  1 + 2 is not 4
  | printed by a failing test

2/3 passed
```

## A failure is an effect, so isolation is free

The prelude declares one effect and one function:

```cronyx
effect Assertion {
    final ctl failed(msg: string);
}

fn assert(cond: bool, msg: string) {
    if (!cond) { failed(msg); }
}
```

`final ctl` is what makes this work. Its handler cannot resume, so no value is ever owed and the call is usable wherever it stands — which is why `assert` needs no bottom type and why a failure leaves the block it is in. Each test is wrapped in its own `run`:

```cronyx
run {
    the_test();
} handle Assertion {
    final ctl failed(msg) { … }
}
```

so a failure abandons that test and the next one still runs.

This is the part worth keeping in mind when comparing to other runners. `cargo test` catches a panic with `catch_unwind`, which is wrong under `panic=abort`; `go test` uses `runtime.Goexit`, which silently fails if a test fails from a goroutine it did not start; `cargo nextest` gives up on both and forks a process per test. Cronyx gets the same isolation from the effect system, statically — a test's row says it may fail.

## Discovery is a walk, not reflection

`Discover.carrying "test"` walks the metaprocessed program for `` `Attributed `` wrapping a `` `Fn ``.

It has to be a walk. Attributes belong to a *declaration*, and `typeof` takes a *value* — a function value's type is `(int, int) -> int`, which names no declaration to look an attribute up under. So reflection cannot reach a function's attributes however much is added to `TypeShape`, and the walk runs on surface syntax because `Desugar` is where the wrapper is unwound.

A test takes no parameters, which is checked rather than assumed: it is called by name and there is nowhere for an argument to come from.

## Output is captured by the host, not by the program

`print` is a builtin already parameterized by where it writes — `Builtins.env ~out` — so `cx test` passes a buffer instead of `print_string`. The runner marks the stream with lines beginning `\x1e`, and whatever a test printed lands between the line that opened it and the line that closed it. A passing test's output is dropped; a failing test's is replayed under it.

That is why making `print` an algebraic effect is *not* a prerequisite for any of this, though it remains worth doing for its own reasons — see Open.

## Settled

**The runner is synthesized, not written.** `cx test` appends one `run` block per test to the linked program and compiles the result. There is no test harness written in Cronyx to keep in step with the tool, and nothing is generated on disk.

**`cx test` exits 1 when anything failed**, and prints `no tests` rather than succeeding silently on a package with none.

## Open

**`assert` takes its message.** `assert(x == 1, "x is 1")` is what is expressible today. pytest's rewritten asserts — reporting the subexpression values of a bare `assert x == 1` — cannot be had by making `assert` a `meta fn`: a meta function's arguments must be known at compile time, and `x` is a runtime value, so the call would fail with `Undefined variable 'x'`. Getting it needs the compiler to reify the argument's *source* into the message, which `Source.expr` can already print. That is one pass away and it is the single largest improvement available here.

**`print` as an effect.** Then a test could handle its own output rather than the host capturing it, and the row would say which functions print. The cost is not small: 332 fixtures call `print` and 50 files write explicit effect rows, so every one of those signatures changes, and an implicit top-level handler has to be designed for programs that do not write one. Worth doing, worth designing first, and not required by anything above.

**No property testing, no snapshots, no doctests.** In that order of value. `stdlib/` has documentation and nothing checks its examples, which is the cheapest of the three to fix and the one that stops documentation rotting.
