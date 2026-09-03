# Continuations and tasks

A continuation carries its handler. A task needs one. They are not the same kind of thing, and a scheduler that treats them as one will not compile.

## Why `resume` contributes nothing to a row

A closure that captures `resume` is pure:

```cronyx
() => { resume; }        // () -> unit
() => { work(); }        // () -> <deferred> unit, where work performs `deferred`
```

This looks like an oversight and is not. Resuming re-enters a computation the handler still delimits, so nothing about invoking it demands a handler from whoever holds it — the handler travels along. `work` has no such luggage: it performs the effect and its caller has to be somewhere the effect is handled.

The handler travelling is observable. A continuation resumed after its `run` block has exited still finds its own handler for a *second* suspension:

```cronyx
run {
    todo();
    print("middle");
    todo();              // caught by the same handler, after the run exited
} handle deferred { ctl todo() { pending.push(() => { resume; }); } }
pending[0]();            // prints "middle", queues the second suspension
```

So the row is telling the truth in both directions. A continuation asks nothing of its caller; a task asks for a handler.

## What that means for a scheduler

The queue holds continuations, not tasks. Every entry is something already under a handler that merely has not finished, which is exactly `() -> unit`, and no function value ever carries an effect row:

```cronyx
fn start(name: string) {
    run { counter(name); } handle sched {
        ctl tick() { pending.push(() => { resume; }); }
    }
}
```

`start` takes the *parameters* of the work rather than the work itself, so the effectful thing is called by name inside a handler and never becomes a value. Round-robin then falls out of draining the queue in order, and `effects/async/async` is this and nothing more.

The shape that does not work is the one that puts tasks and continuations in the same queue. Their types differ, unifying them forces the row onto the continuations too, and the result is rejected — correctly, and for a reason that reads as an arbitrary limitation only until the two are told apart.

## What is still not expressible

Spawning an arbitrary task. `start` above is fixed to `counter`; handing it a computation chosen by the caller means passing a value whose row contains an effect needing a continuation, which `Cps` refuses at the lambda case with *"a function value cannot perform an effect whose handler resumes yet"*.

Nothing in the suite waits on this. It is worth writing down only because it looks like the same problem as the scheduler and is not.

Supporting it means making a closure carry what a converted named function already carries. A delimited named function gains its evidence and a trailing continuation, and its body goes through `cps` rather than `sequence_body`; a lambda gets only the evidence half. The asymmetry survives because such a named function is never used as a value — the `Var` case forbids it and direct calls take the operation path — so nothing ever reads its type, which is why `widen` can add evidence parameters and stay silent about the continuation.

A closure has no name and travels, so its type is the only record of its arity. `widen` would have to describe the continuation before a lambda could be converted at all, and that changes the type of every delimited named function, so it wants the suite run on it alone before anything is built on top.

One trap if that is ever attempted: `continuation` is a single generated constant, so a conversion nested inside another shadows it. A closure written inside a `ctl` arm has a continuation of its own while the `resume` it captures belongs to the arm. With one name the capture binds to the wrong one, and the result is a wrong program rather than an error.

## A message that says the wrong thing

The refusal reads *"an effect whose handler resumes"*, but a plain `ctl` arm that never resumes delimits its effect just as much — only `final ctl` and `fn` arms need no continuation. A handler that only aborts triggers this message while doing the opposite of what it describes.
