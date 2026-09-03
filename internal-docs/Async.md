# Async

The effect has one operation:

```cronyx
effect async {
    ctl suspend<T>(setup: ((T) -> unit) -> unit): T;
}
```

`suspend` hands `setup` a callback and parks. Whoever calls that callback supplies
the value `suspend` returns. Promises, `await`, yielding and `interleaved` are all
ordinary code over it — `tests/effects/async/promise` and
`tests/effects/async/interleaved`.

## Why one operation

Koka's `std/async` declares four — `do-await`, `no-await`, `async-iox` and
`cancel` — and three of them exist to talk to the host event loop: register a
callback with libuv, run an I/O action at the outer level, cancel an outstanding
request. `do-await` is the only one that is about suspension itself. Cronyx has
no event loop, so the other three have nothing to bridge to and `suspend` is
`do-await` with the platform removed.

## A promise is data

```cronyx
type PromiseState<T> {
    Resolved(T),
    Awaiting(List<(T) -> unit>)
}
```

`await` and `resolve` are ordinary generic functions over that, not operations.
They are polymorphic because any function may be; nothing about waiting on a
value needs the handler's help once `suspend` exists.

The two states are exclusive, which is the point of the variant: a resolved
promise cannot still be holding listeners nobody will call.

## Concurrency reinterprets suspension

`interleaved` takes the actions, runs each under a handler that catches
`suspend` and queues the resumption, and drives the queue until every strand has
resolved its slot. There is no `spawn`. A task is created only by handing it to
`interleaved`, which does not return until all of them are finished, so nothing
outlives the call that started it and a strand's failure has somewhere to go.

This is Koka's shape. The alternative — an operation that starts a task and
returns a promise — is the `go` statement, and its cost is a function that may
leave work running after it returns with nothing in its type to say so.

## An operation binds its own type parameters

`suspend`'s `T` belongs to the operation, not to `async`. It cannot belong to the
effect: a row holds at most one instantiation, so `async<int>` and `async<string>`
in one program would unify into an error rather than describing two suspensions.

Quantifying an operation's result is otherwise unsound — the handler would owe a
value of a type it never agreed to, which is what `final ctl` exists to promise
it never does. `suspend` is safe because `T` is settled by an *argument*: the
handler is given a `(T) -> unit` and can only produce a `T` by being handed one.

A handler is installed once and serves every instantiation, so an arm is checked
under variables it may not settle. Pinning one is rejected —
`tests/effects/errors/arm_settles_op_param`.
