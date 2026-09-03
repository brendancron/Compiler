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

## Spawning a task the caller chose

`start` above takes the parameters of the work rather than the work itself, which is enough for a fixed set of tasks and was for a long time all that could be written. A task chosen by the caller is a value whose row carries an effect needing a continuation, and `Cps` now compiles one:

```cronyx
fn spawn(task: () -> <sched> unit) {
    run { task(); } handle sched {
        ctl tick() { queue.push { resume; }; }
    }
}
```

A lambda whose row needs a continuation is converted the way a named function of the same shape always was — evidence parameters, a trailing continuation, and a body through `cps` rather than `sequence_body`. What made that possible was `widen` describing the continuation parameter: a converted named function has always taken one without its type saying so, which survived only because such a function could never be a value and nothing read its type. A closure has no name and travels, so its type is the only record of its arity.

The continuation is described as `Unit` rather than as the function it is, because that is what every site passing one annotates it with. The arity is what a caller reads and the arity is honest; the element type is a separate untruth, older than this, and worth fixing on its own rather than halfway here.

Each converted body binds its continuation under a fresh name. The shared one is still what an arm binds, which is what `resume` reaches for, so a closure written inside an arm keeps resuming the arm rather than itself — `effects/fn_values/closure_in_arm` is that shape and would go wrong silently under one name.

A suspending call is emitted by name, so a callee that is not one — an element of a list, a field — is bound to a temporary first.

`effects/fn_values` holds the shapes: a named effectful function as a value, a closure that suspends, one reached through an index, a closure built inside an arm, and `spawn` itself.
