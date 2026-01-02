### Common Characters
```
⊏ - strictly less general
⊑ - less general
⊐ - strictly more general
⊒ - more general
⊓
⊔
```

Some types are more general than others!
```
Aa. Ab. a -> b
```
is more general than
```
Aa. a -> bool
```
is more general than
```
int -> bool
```

```
Aa. Ab. a -> b ⊑ int -> bool
```

### Syntax for type order
```
o ⊑ o
Aa. a ⊏ int
```

Formal Definition: o1 is more general than o2 if there is a substitution S that maps the for-all quantified variables in o1, and S(o1) = o2

```
Aa. Ab. a -> b
{b |-> bool}
Aa. a -> bool
{a |-> int}
int -> bool
```

## Instantiation
We can instantiate for-all quantifiers to monotypes.

```
Aa. Ab. a -> b
{b |-> List y, a |-> int}
int -> List y
```

If o1 ⊑ o2, an expression of type o2 can be used where one of type o1 is needed.

## Generalization
See [[HM Type System]], [[Free and Bound Type Variables]]

Here is an example of generalizations
```
Ab. a -> b

Aa. Ab. a -> b

FV(Ab. a -> b) = {a} // add this quantifier
```

Adding for-all quantification to a free type variable in a type

IMPORTANT: In HM we can only generalize a type when the type variable is not free in the context.

`generalize(Γ, σ)` returns the most generalized version of the type σ.

Example:
```
Γ = {
	x: b,
	y: List c -> int,
	z: Ad. d
}

σ = Ae. a -> b -> c -> d -> e

generalize(Γ, σ) = ?
```
Remember this adds the free variables in the type that are not in the context.

```
FV(σ) = {a, b, c, d}
FV(Γ) = {b, c}

generalize(Γ, σ) = Aa. Ad. Ae. a -> b -> c -> d -> e
```