# Type system and type checking — bootstrap2

Status: **implemented.** `lib/types.ml` and `lib/typecheck.ml` sit between
`Desugar` and `Interp`; every program now runs through the checker.

This records the design of the OCaml bootstrap's static type system, what was
carried over from the Rust bootstrap's checker
(`bootstrap/src/semantics/types/`), what was done differently, and what is still
missing.

## Context

`bootstrap2` today is a tree-walking interpreter over a dynamically typed
language: scanner → parser → desugar → interp. Values are `Num | Str | Bool |
Unit | Closure | Native`, and no stage consults a type.

Types are needed downstream, not merely for error reporting. That decides
several things below — most importantly that the checker must *produce* an
annotated tree rather than just validate and discard. Known consumers:

- **codegen** — LLVM needs `i64` vs `double` vs pointer.
- **operator overloading** — static types are what let `` `Binop `` lower to a
  direct call instead of a runtime dispatch table. See "Related work" below.
- **monomorphization or method dispatch**, if the language grows them.

## Prerequisite: there is no type syntax yet

`` `Fn of string * string list * 's list `` and `` `Var_decl of string * 'e option ``
carry no annotations — parameters are bare strings. Before any checking:

```cronyx
fn fib(n: num) -> num { ... }
var x: num = 1;          // optional annotation, see open questions
```

Touches `token.ml` (a `Colon` and an `Arrow`), `scanner.ml`, `parser.ml`, and
widens those two AST constructors. Pure plumbing, but it gates everything else,
and how much of it exists determines how much inference is needed.

## Decisions

| Question | Decision |
|----------|----------|
| Numerics | Split `int` / `float`. No implicit widening. |
| Annotations | Optional everywhere; full inference. |
| Numeric ambiguity | Constrained type variables, defaulting to `int`. |
| Polymorphism | Full let-polymorphism, with the value restriction. |
| First cut | Typed AST end-to-end, interpreter runs on it. |
| Desugar spans | Fixed before the checker is written. |

## Representation: two type languages, not one

```ocaml
(* Inference time: mutable and incomplete. *)
type infer_ty =
  | IInt | IFloat | IStr | IBool | IUnit
  | IFn of infer_ty list * infer_ty
  | IVar of tv ref
and tv = Unbound of int * kind | Link of infer_ty
and kind = Any | Numeric

(* After inference: fully resolved. No unification variable can appear. *)
type ty =
  | Int | Float | Str | Bool | Unit
  | Fn of ty list * ty
  | Generic of int
```

`resolve : infer_ty -> ty` collapses `Link` chains, defaults unconstrained
numeric variables to `Int`, and maps anything still genuinely unconstrained to
`Generic`.

The point of two representations is that an *unresolved* type variable is
unrepresentable downstream: nothing consuming `ty` has to ask "what if this is
still being inferred". This is the same discipline that keeps `` `Compound `` and
`` `For `` out of the interpreter via `desugared_stmt`, and it is the structural
fix relative to the Rust bootstrap, where unresolved variables leak into the
type map and later stages compensate.

`Generic` is the concession full let-polymorphism forces. `fn id(x) { return x; }`
is `'a -> 'a` and its body genuinely has no concrete type, so `ty` needs a way to
say "quantified" as distinct from "not yet known". A `Generic` reaching codegen
means that call site was never monomorphized — a marker for work to do, not an
unresolved variable.

The surface spelling is Cronyx's: `int`, `float`, `string`, `bool`, `unit`. The
OCaml constructor is `Str`, but no user ever writes `str`.

Cost: one conversion function and some duplication between the two types.

## Annotation: parameterize `node`, don't build a parallel AST

```ocaml
type ('a, 'ann) node = { it : 'a; span : span; ann : 'ann }

type desugared_expr = (desugared_expr_kind, unit) node
and  desugared_expr_kind =
  [ lit | desugared_expr vars | desugared_expr ops | desugared_expr logic ]

type typed_expr = (typed_expr_kind, ty) node
and  typed_expr_kind =
  [ lit | typed_expr vars | typed_expr ops | typed_expr logic ]
```

The fragments in `ast.ml` are already parameterized over their child type, so
the typed AST costs about six lines instead of a re-declaration of every
constructor — the same payoff that made `desugared_expr` cheap. `at` gains
`ann = ()`; untyped stages are otherwise unchanged.

A side table keyed by node id (what the Rust bootstrap uses) is the wrong
choice here specifically because `bootstrap2` has no node ids. Adopting one
would mean inventing an `IdProvider` solely to serve the type checker.

Annotate expressions. Statements are all `Unit` except `` `Return ``, which is
checked against the enclosing signature — so inference threads a "current return
type" downward, the way the evaluator threads `Return_value`.

## Pass structure

```
desugared_stmt list
  → hoist      bind every top-level fn signature
  → infer      walk, unify, annotate with infer_ty   (mutates tv refs)
  → resolve    infer_ty → ty everywhere; error on unbound
  → typed_stmt list
```

Two traversals after the hoist, because Hindley–Milner cannot finish a node's
type when it first visits it: in `fn f(x) { return x + 1; }`, `x` is only pinned
at the `+`. Fusing infer and resolve means threading a substitution by hand;
the separate resolve pass is far easier to get right.

The hoist pass also produces the function signature table
(`(string, ty) Hashtbl.t`) that codegen wants for emitting declarations before
any body is compiled.

## Numeric constraints and defaulting

Two numeric types plus annotation-free inference makes `fn double(x) { return x + x; }`
ambiguous — nothing pins `x` to `int` or `float`. The resolution is a single
built-in constraint rather than a general type-class mechanism:

- A fresh variable is `Unbound (id, Any)`.
- Arithmetic operators unify their operands with each other and mark the
  resulting variable `Numeric`.
- Unifying a `Numeric` variable with `Int` or `Float` succeeds; with `Str`,
  `Bool`, `Unit`, or a function type it fails with "operator `+` expects a
  numeric type".
- Unifying two variables takes the stronger constraint.
- At `resolve` time, a still-unbound `Numeric` variable **defaults to `Int`**.
  A still-unbound `Any` variable is an error (except where generalized).

There is no implicit widening: `1 + 2.5` is a type error, and mixing requires an
explicit conversion. Subtyping interacts badly with unification, and the error
messages it produces are markedly worse.

## Polymorphism and the value restriction

Generalization happens at both `fn` and `var`, so `fn id(x) { return x; }` is
`'a -> 'a` and usable at several types.

Because `var` bindings are reassignable, naive generalization is unsound:

```cronyx
var f = fn(x) { return x + 1; };   // generalized to 'a -> 'a ?
f = fn(x) { return x; };           // each use instantiates fresh
f("hi");                           // passes the checker, misbehaves at runtime
```

A `var` is therefore generalized only when **both** hold:

1. its initializer is a syntactic value (literal, function literal, variable) —
   the standard value restriction, and
2. the binding is never reassigned in its scope — a cheap pre-scan, and
   `desugar.ml` has already rewritten `+=` into an ordinary assignment by this
   point, so compound assignment is covered for free.

Everything else stays monomorphic.

Consequence to plan for: inferred polymorphism means LLVM codegen will need
**monomorphization**, since no single machine function implements `'a -> 'a`.
The Rust bootstrap punted on this and stored one call-site signature per
function (`runtime_type_checker.rs:118-126`), warning when a polymorphic
function is used at two concrete types. That shortcut is exactly what to avoid
repeating.

## Lessons from the Rust bootstrap

`bootstrap/src/semantics/types/` is textbook Algorithm W with an **explicit
substitution map**: `Type::Var(TypeVar { id })` is immutable and all state lives
in `TypeSubst { map: HashMap<TypeVar, Type> }` (`type_subst.rs:5`), threaded as
`&mut` through every inference call. `unify` (`type_subst.rs:60`) applies the
substitution to both sides, matches structurally, and occurs-checks via
`contains`. `TypeEnv` is a scope stack of `HashMap<String, TypeScheme>` plus the
fresh-variable counter. Results land in side tables keyed by node id.

### Carry over

- **Hoist function signatures before checking bodies** — `hoist_fn_types`
  (`runtime_type_checker.rs:108`). Make it the only mode, not a phase-2 addition.
- **A final resolve pass** — phase 2 substitutes over the whole map at the end
  (`runtime_type_checker.rs:113-116`) so callers see concrete types. Build this
  in from the start.
- **The scope-stack environment**, which already mirrors `interp.ml`'s `env`.

### Change

- **Mutable unification variables instead of a threaded substitution.**
  `IVar of tv ref` with path compression lets `unify` mutate in place: no
  substitution parameter on every function, no `apply` at every node, no
  repeated chain-walking (today `unify` applies the substitution to *both*
  arguments on every recursive call). It also removes an entire bug class —
  phase 1's `TypeTable` records `ty.apply(subst)` at visit time
  (`type_checker.rs:492`) and is never re-substituted afterward, so it holds
  whatever was known then. Benign only because `debug_sink.rs:72` is its sole
  reader.
- **One checker, not two.** `type_checker.rs` and `runtime_type_checker.rs` are
  ~1,680 lines of near-duplicate inference, and the duplication exists only
  because `MetaAst` and `RuntimeAst` are distinct types. `bootstrap2` has a
  single post-desugar AST.
- **No permissive fallbacks.** `env.lookup(name).unwrap_or_else(|| Type::Var(env.fresh()))`
  (`type_checker.rs:252-259`) and the struct escape hatches (`:271-280`) each
  inject an unresolved variable that something downstream must then compensate
  for. An unbound variable should be a hard error.
- **Keep `TypeScheme`, but add the value restriction.** `generalize`/`instantiate`
  (`type_utils.rs:73-100`) port over almost directly, and `TypeEnv::lookup`
  instantiating on every lookup is the right ergonomic. What is missing there is
  any value restriction — acceptable in the Rust bootstrap only because nothing
  yet exercises the unsound case.

### Latent issues worth not reproducing

- **Effect rows are carried but never unified.** `Type::Func` holds an
  `EffectRow`, but `unify`'s function case destructures with `..`
  (`type_subst.rs:84-87`) and unifies only parameters and return type, while
  `apply` clones the row through unchanged. Effects are really computed by a
  separate `collect_body_effects` walk. That is a defensible design, but storing
  the row inside the type implies a checking discipline that does not exist. If
  `bootstrap2` grows effects, decide explicitly whether they participate in
  unification or are a separate analysis with their own representation.
- **Call-site signatures standing in for monomorphization.**
  `runtime_type_checker.rs:118-126` stores one concrete signature per `FnDecl`
  for codegen and, when a polymorphic function is called at two different
  concrete types, warns and keeps the first. Either monomorphize properly or do
  not generalize; the middle ground produced this wart.

## What shipped

| Step | Where |
|------|-------|
| Desugar span fidelity | `lib/desugar.ml` |
| `int` / `float` split | `lib/token.ml`, `lib/scanner.ml`, `lib/interp.ml` |
| Optional type syntax | `lib/token.ml`, `lib/scanner.ml`, `lib/parser.ml` |
| `('a, 'ann) node` | `lib/ast.ml` and everything downstream |
| Types and unification | `lib/types.ml` |
| Inference | `lib/typecheck.ml` |
| `--dump-types` | `bin/main.ml`, `lib/printer.ml` |
| Fixtures | `tests/types/inference/`, `tests/types/errors/` |

Notes on the parts that differ from the plan above:

- **Errors accumulate per top-level statement.** A statement that fails is
  dropped from the checked tree and checking continues; the tree is only
  returned when there are no errors at all, so a partial tree never escapes.
- **`unify` takes expected first, actual second**, and the message reads
  "Expected X, got Y" straight off that. Call sites that had it backwards
  produced inverted messages (`if (1)` reporting "Expected int, got bool"),
  which is worth watching for when adding rules.
- **Generalization removes the function's own binding first.** `hoist` binds a
  function monomorphically so recursion works; leaving that binding in place
  while computing the environment's free variables makes every one of the
  function's own variables look free, and nothing is ever quantified. This cost
  an hour and is invisible until you test a function used at two types.

## Known gaps

- **`print` is variadic**, which no HM type describes. Calls to it are checked
  structurally (arguments are inferred, the call is `unit`); referring to
  `print` as a value yields an unconstrained type rather than a real signature.
  A proper fix needs either varargs in the type language or a `print` that takes
  one argument.
- **No exhaustiveness check on `return`.** A function returning on only one path
  infers from the `return` it can see, while the evaluator yields `unit` when
  control falls off the end. The types are a promise the runtime does not keep.
- **`Generic` has no consumer yet.** Nothing monomorphizes, so a polymorphic
  function is fine for the interpreter and will not be for codegen.
- **Uses before a function's declaration are monomorphic.** Generalization
  happens when the declaration statement is reached, so a call earlier in the
  same block sees the hoisted monomorphic type.

## Open questions

- **Do comparison operators have a fixed `bool` return**, or may user code
  define otherwise? Currently fixed, and `<` is numeric-only while `+` also
  accepts strings.
- **Should `var x;` with no initializer really be `unit`?** That is what the
  evaluator does, so the checker agrees with it, but "declare now, assign later"
  is a reasonable thing to want and is currently a type error.

## Related work

Operator overloading (deferred) depends on this. The sketched design dispatches
on the type tags of *all* operands, with builtin arithmetic living in the same
table as user definitions, and lowers `` `Binop `` to a direct call once static
types exist. Until then the same table would be consulted at runtime. The
design does not change when types arrive — only when it runs.
