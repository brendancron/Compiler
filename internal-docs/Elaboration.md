# Elaboration

`Resolve` runs after type checking and turns every construct whose meaning depends on a type — operators, compound assignment, indexing, literals — into a concrete call or a primitive, chosen once at compile time. It is the last stage of the [elaboration](architecture.md) cluster, which is where its name comes from.

That is what lets a user-defined type give meaning to `+`, `==`, `[]`, and the rest without `int + int` costing anything more than it does today.

## Declaring an operator

An operator is declared like a function, named by the symbol it defines:

```cronyx
type Vec2 {
    x: int,
    y: int
}

op +(a: Vec2, b: Vec2) -> Vec2 {
    return Vec2 { x: a.x + b.x, y: a.y + b.y };
}

var v = Vec2 { x: 1, y: 2 } + Vec2 { x: 3, y: 4 };

print(v.x);      // 4
print(v.y);      // 6
```

Both operands are matched, not just the left one, so the two sides may differ:

```cronyx
op *(n: int, v: Vec2) -> Vec2 {
    return Vec2 { x: n * v.x, y: n * v.y };
}

var doubled = 2 * v;

print(doubled.x);   // 8
```

That also means commutativity is not free. `v * 2` is a separate declaration unless a rule derives it:

```cronyx
op *(v: Vec2, n: int) -> Vec2 {
    return n * v;
}
```

Comparison operators are entries like any other, so their result type is whatever the entry says:

```cronyx
op ==(a: Vec2, b: Vec2) -> bool {
    return a.x == b.x and a.y == b.y;
}

print(v == v);      // true
```

## Assignment and increment

`x += v` and `x++` are entries of their own, because a type may want to update in place rather than build a new value and rebind:

```cronyx
op +=<T>(xs: List<T>, v: T) {
    xs.push(v);
}

var xs: List<int> = [1, 2, 3];

xs += 4;         // appends; does not rebuild the list
```

```cronyx
type Counter { count: int }

op ++(c: Counter) {
    c.count = c.count + 1;
}

var c = Counter { count: 0 };

c++;
print(c.count);  // 1
```

When no entry matches, they derive from the plain operator, so a type gets the expected meaning without declaring anything:

```cronyx
var n = 1;

n += 2;          // no entry for (AddAssign, int, int), so: n = n + 2
n++;             // likewise: n = n + 1
```

The derivation needs `+` to exist. A type with neither is an error naming both: there is no `+=` for it and nothing to derive one from.

Because these are chosen by type, they cannot be rewritten in `Desugar` — `x += v` reaches the checker intact, and `Resolve` decides whether it becomes a call to an in-place entry or an assignment built from `+`.

Note what in-place means for a type with identity:

```cronyx
var xs: List<int> = [1, 2, 3];
var ys = xs;

ys += 4;
print(xs.len());   // 4 — same list
```

## Indexing

Reading and writing an index are separate entries, since a type may support one and not the other:

```cronyx
type Ring<T> {
    items: Array<T>,
    head: int
}

op [](r: Ring<T>, i: int) -> T {
    return r.items[(r.head + i) % r.items.len()];
}

op []=(r: Ring<T>, i: int, v: T) {
    r.items[(r.head + i) % r.items.len()] = v;
}

print(ring[0]);
ring[0] = 5;
```

`xs[i] += 1` composes the two with the operator between them, unless an in-place entry claims the whole form.

## Literals

A literal has no type of its own — it proposes candidates and the expected type picks one. That is the same table, keyed in the opposite direction: operators resolve forwards from operand types to a result, literals resolve backwards from a target type to a constructor.

```cronyx
var a: Array<int>       = [1, 2, 3];
var l: List<int>        = [1, 2, 3];
var s: Set<int>         = [1, 2, 3];
```

See [Collection Literals](Collection%20Literals.md) for how that one works in detail. Numeric literals already behave this way — `1` is an `int` or a `float` depending on context, defaulting to `int`.

String literals stay simple: a string literal is a `string`. Making them target-typed would attach a constraint to nearly every literal in a program, and it is deferred rather than decided — see [TODO](TODO.md).

## What it costs

Both of these lower during compilation, so nothing is looked up while the program runs:

```cronyx
var a = 1 + 2;                       // primitive add
var b = Vec2 { x: 1, y: 2 } + v;     // direct call to the `+` above
```

The first emits the same node the evaluator has always handled. The second emits an ordinary call — the cost of the function you wrote, and nothing more.

## The table

Internally the declarations above become entries. The checker consults them for the result type, and the lowering pass for the emission:

```
(Add, int,    int)     → int      primitive add
(Add, float,  float)   → float    primitive add
(Add, string, string)  → string   primitive concat
(Add, Vec2,   Vec2)    → Vec2     call the declared `+`
(Mul, int,    Vec2)    → Vec2     call the declared `*`
```

Builtin arithmetic is not a special case in the compiler. It is an entry whose emission happens to be a primitive rather than a call, which is why it stays fast without the operator path knowing anything about it.

Two consumers, one table — that is the reason for a table rather than two matches that have to agree.

## Inference

When operand types are not yet known, the checker attaches a constraint instead of resolving, exactly as it does today:

```cronyx
fn double(x) {
    return x + x;
}

print(typeof(double));   // (int) -> int
```

`x` is constrained to a type that has a `+`, nothing narrows it further, and an unresolved numeric variable defaults to `int`.

The same constraint is what a generic function carries. A body using `+` on a type parameter requires an entry for it, that requirement travels with the signature, and each call site discharges it against the table — inferred throughout, never written. See [Comptime Params](Comptime%20Params.md). Annotating changes the answer without changing the body:

```cronyx
fn double_vec(x: Vec2) -> Vec2 {
    return x + x;
}
```

## Polymorphism forces monomorphization

```cronyx
fn sum3(a, b, c) {
    return a + b + c;
}

print(sum3(1, 2, 3).x);            // int addition
print(sum3(v, v, v).x);            // Vec2 addition
```

One body, two answers. Either the function is specialized per instantiation — so each copy has concrete operand types and resolves statically — or the body dispatches at runtime, and the zero-cost property is lost precisely where generic code needs it.

So: resolve statically wherever types are concrete, and monomorphize generic functions so that they are. Monomorphization is already owed for `Generic` and for effect-polymorphic functions; this makes it load-bearing for a third feature rather than adding a new obligation.

Without it, zero-cost operators exist only in monomorphic code. That is the ceiling, and it is worth stating rather than discovering.

## Rules the table needs

- **Precedence** when two entries match the same operands.
- **A designated default**, so an unresolved numeric literal has an answer.
- **Which operators are open.** `and` and `or` short-circuit, so overloading them would be overloading control flow — they stay closed:

  ```cronyx
  op and(a: Vec2, b: Vec2) -> Vec2 { ... }   // rejected
  ```

- **Registration completes before type checking**, since the checker consults the table.

## What this replaces

The previous approach dispatched at runtime: a `HashMap` keyed by `(trait, type_name)`, consulted on every evaluation of an operator whose left operand was a struct, and reached only after falling through a match on the builtin cases. Selection here happens once, at compile time, and the builtin cases are entries rather than a fallthrough.

## Two lookups, one registry

Operators and literals are keyed in opposite directions — an operator lookup takes operand types and yields a result, a literal lookup takes a target type and yields a constructor — so they are two maps rather than one with a discriminant and unused fields.

They are built and passed together as a single registry, so registration and injection stay one thing even though the lookups are two.

## Settled

**`v * 2` is not derived from `2 * v`.** Both are written, or neither works. A derivation rule would silently invent an entry the author did not write, and commutativity is not a property the compiler can assume of a user's operator.
