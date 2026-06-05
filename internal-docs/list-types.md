# List, Slice, and the Iterable Trait

Status: design intent, not yet implemented end-to-end.

## Goal

Move the user-facing list type out of the interpreter and into the stdlib,
keeping only the minimum buffer primitive in the compiler. Same direction
as Rust's `Vec` / `Iterator`: the language provides one growable heap-shared
buffer, and the stdlib builds everything user-facing on top.

## Today

Compiler-coupled:

- `Value::List(Rc<RefCell<Vec<Value>>>)` — runtime representation. Created
  by the `[1, 2, 3]` literal, mutated by built-in `.push` / `.pop` /
  `.remove` methods hard-coded in the interpreter.
- `Type::Slice(Box<Type>)` — static type spelled `[T]`. Despite the name,
  no slice semantics exist: there's no view/window relationship to a parent
  buffer. Every operation returns a fresh `Value::List`.
- Iteration: `for x in xs` walks `Value::List` directly in the interpreter.
- Codegen: emits a `{ i64 len; i64 cap; ptr data }` struct for `[T]`, with
  push/pop intrinsics that realloc.

Result: "list" and "slice" name the same thing, badly. There are no real
slices (views) in the language.

## Target

### Compiler-given primitives (minimal)

- One opaque growable buffer (`__vec` or similar; not user-visible). The
  current `Value::List` storage, stripped of methods.
- An `Iter` trait shape recognized by `for-in` lowering.
- Indexing and slicing syntax (`xs[i]`, `xs[a:b]`) desugars to method
  calls — `xs.index(i)`, `xs.slice(a, b)` — that resolve via the impl
  registry. No special-case interpretation.

### Stdlib `List` class

```
class List<T>(buf: __vec) : Iter<T>, Add, Index {
    fn push(x: T) { ... }
    fn len(): Int { ... }
    fn map<U>(f: fn(T): U): List<U> { ... }
    fn filter(f: fn(T): Bool): List<T> { ... }
    ctl next(): Option<T> { ... }   // for Iter
    fn add(other): List<T> { ... }  // for Add (used by `+=`)
    fn index(i: Int): T { ... }
}
```

- `[1, 2, 3]` becomes a parser-level desugar to `List(__vec::from([1, 2, 3]))`,
  or simpler: `List([1, 2, 3])` where `[..]` produces the primitive buffer.
- `xs += y` is the existing `xs = xs + y` desugar; the trait makes it work.
- `for x in xs` desugars to `xs.iter()` + `.next()` loop, type-checked
  through the `Iter` trait bound.

### Stdlib `Slice` class

A second, distinct type. Now the name means something.

```
class Slice<T>(buf: __vec, start: Int, end: Int) : Iter<T>, Index {
    fn len(): Int { return end - start; }
    fn index(i: Int): T { ... }
    ctl next(): Option<T> { ... }
}
```

- `xs[a:b]` desugars to `xs.slice(a, b)`. List returns `Slice<T>` that
  aliases the parent buffer — true slice semantics.
- No `.push` — slices have fixed length.
- Reads through the same buffer the parent List owns. Mutating the parent
  is visible to the slice.

### Iter trait

```
effect Iter<T> {
    ctl next(): Option<T>;
}
```

(Or expressed as a plain trait with `fn next(): Option<T>` — handler-class
semantics make either workable.)

`for x in xs { body }` lowers to:

```
run {
    while (true) {
        match xs.next() {
            Option::Some(x) => { body },
            Option::None => break,
        }
    }
} handle xs;
```

(Sketch; real lowering depends on whether iteration is effect-based or
trait-based.)

## Migration order

1. **Iter trait + `for-in` desugar** — landmark change. Until this exists,
   the interpreter can't drop its hard-coded list iteration.
2. **Move list method dispatch to impls** — `.push`, `.len`, etc. become
   user-visible impl methods on a stdlib class. Interpreter intrinsics
   become a private `__vec` primitive that the class wraps.
3. **`[…]` literal desugars to `List(…)` ctor** — parser-level. The
   primitive `[…]` still exists internally for the wrapped buffer.
4. **Add `Slice<T>` class** — distinct from List, with view semantics.
   Switch `xs[a:b]` parsing to it.
5. **Codegen** — last. Stays interpreter-only for stdlib types until
   codegen learns class types end-to-end.

## Smallest step in the right direction

Done (this session):

- Compiler: `dispatch_binop` consults `op_dispatch` for primitive values
  too — `Value::List` resolves under type name `"list"`, not only
  `Value::Class`.
- Stdlib: `impl Add for list` defines append semantics, so `queue += thunk`
  works as push.

Each subsequent migration step uses the same pattern: move dispatch from
intrinsic table to user-visible impl, one operator/method at a time.

## Open design questions

- **Generic primitives**: `List<T>` wants `T` parameters. The current type
  system has `Type::App` for generic instantiation; codegen monomorphizes
  some calls but not container types. Real container generics need full
  monomorphization of the wrapping class.
- **Slice ownership**: a `Slice<T>` borrows the parent's buffer. If the
  parent is dropped while the slice lives, the slice reads freed memory.
  Either Rc<RefCell> the underlying buffer (current shape, fine), or
  introduce lifetimes (large language addition; probably skip).
- **`+=` as push vs concat**: today `xs + ys` with two lists would also
  resolve through `impl Add`. A single impl can't mean both push (`xs +
  thunk`) and concat (`xs + other_list`). Either:
  - `Add` is concat, `Push` is a separate trait, and `+=` resolves
    against either based on operand types (real ad-hoc overloading);
  - `+=` always means push of the RHS as a single element, and concat
    uses an explicit `.concat()` method.
- **Iter as effect vs trait**: effects already support `ctl next()` shape
  cleanly. Trait-based requires async-stream-like ownership of an
  internal cursor. Effect-based is simpler given the current design.
