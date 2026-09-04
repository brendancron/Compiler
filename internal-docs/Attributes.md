# Attributes

Data written on a field or a variant, read by a deriver, and gone before the program runs.

```cronyx
type Person {
    @rename("full name")
    name: string,

    @skip
    password: string,

    age: int,
}
```

## Why the word is not "annotation"

`Ast.node` is `('a, 'ann)`, and `'ann` is the per-node type slot that carries `unit` through the front end and `Types.ty` after checking. That name was taken first, so the user-facing feature is an *attribute*.

## Erasure is the whole design

An attribute exists so a deriver can read it. Nothing else in the language looks at one: no pass changes behaviour on an attribute, and there is no way to ask for one at runtime.

That is enforced by the shape of the tree rather than by a rule anyone has to remember. `type_defs` appears in `stmt_kind`, `desugared_stmt_kind`, `typed_stmt_kind`, `resolved_stmt_kind` and `reflected_stmt_kind` — and not in `cps_stmt_kind`. A `Type_decl` is therefore already gone by the time `Interp` runs, so an attribute riding on one cannot reach the program. A pass that tried to read an attribute after CPS would not compile.

This is the same question Java answered with `@Retention` and answered badly: `RUNTIME` retention puts metadata in every artifact, makes reflection the way frameworks work, and defeats static reachability badly enough that ahead-of-time compilers need hand-written configuration to recover it. Go's struct tags make the smaller version of the mistake — a tag is a string nobody validates, so a typo compiles and misbehaves. Rust, D and C++26 keep attributes to compile time, and this follows them.

## Where they live

Attributes are on the AST node, in `Ast.field.f_attrs` and `Ast.variant.v_attrs`, which is what puts them in `Artifact` — an artifact is `Marshal` of `Ast.program`, so a deriver in one package reads attributes written in another with nothing added.

They stay out of `Typecheck.decl`. Unification and both monomorphizers walk that, and none of them has a reason to carry inert data. `Typecheck` copies attributes into `ctx_attrs`, keyed by type name and member label, and `Reflect` is the only reader — the same arrangement `ctx_types` already had for a sum's variants, which `Reflect` reads for the same reason.

```
parser      attaches to the field or variant
typecheck   records in ctx_attrs
reflect     folds into the TypeField / TypeVariant record
cps         drops the declaration, and the attribute with it
```

## Arguments are literals

An argument is a `string`, `int`, `float` or `bool` written out, never an expression. There is nothing to evaluate: the parser builds `Ast.attr_arg` directly, and `Reflect` writes it back as an `AttrArg` variant. A deriver matches on it.

```cronyx
for (a in f.attrs) {
    if (str(a.name) == "rename") {
        for (arg in a.args) {
            match arg {
                AttrArg::Str(v) => { label = v; }
                AttrArg::Int(v) => {}
                AttrArg::Float(v) => {}
                AttrArg::Bool(v) => {}
            }
        }
    }
}
```

## Settled

**Only a member carries one.** `@json("people") type Person` is rejected. Whatever a whole type would say is an argument to the deriver — `derive_json(Person, table: "people")` needs no new syntax and is already more expressive. Locality on a field is the one thing a meta function's parameters cannot reach, and that is the entire case for the feature.

**A named payload's fields do not.** `Circle { r: float }` is a variant's payload, not a member of the type, and reflection reports a variant's arity rather than its fields. Nothing can read an attribute there, so nothing may write one.

## Open

**Nothing rejects an attribute no deriver reads.** `@renmae("x")` compiles and does nothing, which is Go's bug. The fix is Rust's: a deriver declares the attributes it consumes, and an attribute belonging to no declaration is an error. That needs syntax on the deriver — a `uses` clause or similar — and is worth having before attributes are used for anything real.

**A product reports its fields in sorted order, a sum its variants in declaration order.** `Reflect` reads a product's fields off the row, which is sorted, and a sum's variants off `ctx_types`, which is not. That predates attributes, but attributes make it matter: a serializer derived from field order now produces sorted output, which is nobody's intent.
