# Data structures

The datatypes the language has, and what each looks like in use.

## Array

`Array<T>` — length fixed at creation. Elements can be reassigned; the array cannot grow.

```cronyx
var xs = [1, 2, 3];

xs[0] = 5;

print(xs[0]);            // 5
print(xs.len());         // 3
print(typeof(xs));       // Array<int>
```

An array of a given size:

```cronyx
var zeros = new Array<int>(16);

print(zeros.len());      // 16
print(zeros[0]);         // 0
```

## List

`List<T>` — grows, backed by an array.

```cronyx
var xs: List<int> = [1, 2, 3];
xs.push(4);

print(xs[0]);            // 1
print(xs.len());         // 4
print(typeof(xs));       // List<int>
```

An empty collection is written `[]`, and states its type where there is nothing to infer one from:

```cronyx
var results: List<int> = [];

results.push(3);
```

## Collection literals

`[1, 2, 3]` does not commit to a container. An annotation, a parameter type, or a method call decides which one it builds, and with nothing to narrow it the literal is an array.

```cronyx
var a: Array<int> = [1, 2, 3];
var l: List<int>  = [1, 2, 3];
var s: Set<int>   = [1, 2, 3];
var d             = [1, 2, 3];   // Array<int>
```

See [Collection Literals](Collection%20Literals.md) for the rules.

## Tuple

`(A, B, ...)` — fixed length, positional, mixed types.

```cronyx
var pair = (1, "hello");
var nested = ((1, 2), "abc");

print(typeof(pair));     // (int, string)
print(typeof(nested));   // ((int, int), string)
```
## Type declarations

One keyword declares both products and sums. The body says which: entries written `name: T` are fields, bare entries are variants. A declaration may not mix the two.

A product is a named record — nominal, so two declarations with identical fields are still different types.

```cronyx
type Point {
    x: int,
    y: int, // trailing comma is optional
}

var p = new Point { x: 1, y: 2 };

print(p.x);              // 1
print(typeof(p));        // Point
```

A sum lists its variants. They carry nothing, a tuple of values, or fields.

```cronyx
type Color { Blue, Red, Green }

type Shape {
    Circle(int),
    Rect { w: int, h: int },
    Empty
}

var s = new Shape::Rect { w: 3, h: 4 };

match s {
    Shape::Circle(r)     => { print(r); }
    Shape::Rect { w, h } => { print(w * h); }   // 12
    Shape::Empty         => { print("empty"); }
}
```

Both take type parameters.

```cronyx
type Option<T> { Some(T), None }

type Pair<A, B> {
    first: A,
    second: B
}

var found = new Option::Some(3);
var both = new Pair { first: 1, second: "one" };
```

## Map

`Map<K, V>` — keys must be hashable and comparable.

```cronyx
var ages: Map<string, int> = [("alice", 30), ("bob", 25)];

ages.insert("carol", 41);

print(ages.get("alice"));   // Some(30)
print(ages.len());          // 3
```

A map's element type is the pair `(K, V)`, so an array of tuples is already the right shape and no separate literal form is needed.

## Set

`Set<T>` — same constraints as map keys. A collection literal reads as a set when the annotation says so, dropping duplicates.

```cronyx
var seen: Set<int> = [1, 2, 2, 3];

seen.insert(4);

print(seen.contains(2));    // true
print(seen.len());          // 4
```
