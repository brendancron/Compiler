See [[Substitutions in Logic]]

A substitution unifies two values if when applied to both, it results in equal results.

S(a) = S(b)
```
S = {r |-> y, s |-> y, d |-> s}
a = red
b = yes

S(a) = yes
S(b) = yey
```
Does S unify a and b? NO!

These are different so they doo not unify a and b.

What substitutions unify a and b?
```
a = red
b = yes

S = {r |-> y, d |-> s}
S(a) = yes
S(b) = yes
```
Often we would like to find a unifying substitution with the fewest mappings called:

The Most General Unifying Substitution

## Exercises
For this exercise, we can only map symbols to expressions.

```
a = 3
b = y

S = {y |-> 3}
S(a) = S(b) = 3
```

```
a = 3 + (7 * z)
b = y + (x * 2)

s = {y |-> 3, x |-> 7, z |-> 2}
S(a) = S(b) = 3 + (7 * 2)
```

Note for this examples expr can include operators

```
a = 3 + 4
b = y

S = {y |-> 3 + 4}

S(a) = S(b) = 3 + 4
```

```
a = 3 + z
b = y

S = {y |-> 3 + z}
S(a) = S(b) = 3 + z
```

Now lets look at an example where it doesn't work.

```
a = 3 * 7
b = 3 + z

S = ?
```
There is no unifying substitution that unify these 2.

```
a = 1 + z
b = z

S = {z |-> 1 + z}
```
Unless we let z be an infinite substitution, there is no unifying substitution.

## Algorithm Signature
```
S = unify(a, b)

S(a) = S(b)
```
A unifying substitution may not exist at all or may not be finite.

# Unifying Substitutions in Type Systems

```
a = int -> a
b = b -> bool

S = unify(a, b)
S = {a |-> bool, b |-> int}

S(a) = S(b) = int -> bool
```

```
a = List a
b = b -> bool

S = unify(a, b)
```
No unifying substitution exists, this is a type error!

```
a = a -> int
b = a
```
Again we hit infinite types, type error!

In a sense unification can 'merge' types while preserving all type constraints.

