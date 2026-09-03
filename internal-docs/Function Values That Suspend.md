# A function value that suspends

A closure whose row carries a delimited effect — one whose handlers do not all resume in tail position — is refused:

```
A function value cannot perform an effect whose handler resumes yet.
```

Calling such a value may not return normally, which is what the name is for. `Cps` already calls the transform *conversion* and already has `suspends` as a predicate; "delimited closure" names the table (`info.delimited`) rather than the thing.

This is what `effects/async/async` waits on. Nothing else does.

## What already works, and why the rest does not

A delimited *named* function is converted at the `Fn` case of `stmt`: it gains its evidence parameters and a trailing continuation, and its body goes through `cps` rather than `sequence_body`. A lambda gets only the first half — evidence parameters and a `sequence_body` — and the delimited case raises `unsupported` instead.

The asymmetry survives because a delimited named function is never used as a value. The `Var` case refuses to let one escape, and a direct call to one takes the operation path, so nothing ever reads its type. That is why `widen` can add evidence parameters and stay silent about the continuation: no one was looking.

A closure has no name. Its type is the only description of its arity, and it travels. So the type has to become honest before a lambda can be converted at all, and that ordering is what the steps below follow.

## Widen first, and stop there

`widen` should give a delimited function type its continuation alongside its evidence, where the continuation takes the function's result and answers with nothing:

```
Fn (params, ret, row)  ->  Fn (params @ evidence @ [ Fn ([ ret ], Unit, []) ], ret, row)
```

This changes the type of every delimited named function, whose declarations already carry the parameter, so it is the one step that can break what passes today. Run the suite on it alone. `effects/delim`, `effects/flip`, `effects/recover` and `effects/stream` are the shape that would notice. Everything below assumes this held.

## Then the lambda, with a continuation of its own

The delimited lambda case becomes what the `Fn` case does — evidence, a trailing continuation, and `cps` over the body.

**Give each conversion a fresh continuation name.** `continuation` is one generated constant, so nesting one conversion inside another makes the inner binding shadow the outer. A closure written inside a `ctl` arm is exactly that: it has a continuation of its own, and the `resume` it captures is the arm's. With a shared name the capture binds to the wrong one, and the result is not an error but the wrong program. `fresh "k"`, threaded through `cps`, costs nothing here and cannot be retrofitted cheaply once anything depends on the constant.

A call through a function value then has to pass the continuation. `suspends` already answers for the callee's row, but the `Call` case appends evidence arguments alone.

## What falls out

The `Var` case refuses a named effectful function because "a bare reference would escape with the wrong arity". Once `widen` describes the arity, that stops being true, and passing a named function as a value can be allowed in the same change rather than tracked as its own gap.

## Fixtures

In order, and each written before the step it belongs to:

- a suspending closure called immediately
- one stored in a `List` and called afterwards, which is what the scheduler does
- one capturing `resume` while typed with a row — `effects/deferred` with its list typed `<E>` rather than pure, which is the only difference between the case that works and the case that does not
- nested suspending lambdas, for the shadowing above
- a named effectful function passed as a value
- `effects/async/async`, which flips from waiting to passing

## Where this could go wrong

The shared continuation name is the risk that produces a wrong answer rather than a diagnostic, which is why it is settled in the same step that introduces the need for it.

Scope restoration is probably a separate matter — `effects/deferred` shows a resumption outliving its arm already works — but a closure invoked long after its arm is the case that would meet it. A fixture failing in a way that reads as scopes rather than arity means `async` is behind [the continuation rework](Algebraic%20Effects.md) after all, and not behind this.
