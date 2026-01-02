A substitution is a map from variables to terms.
Mappings are applied simultaneously.

```
S = {h |-> j}
```
```
hello
S(hello) = jello
```

```
S = {
	h |-> l,
	e |-> a,
	l |-> s,
}

S(hello) = lasso
```
Note in this example h does not turn into s transitively since substitutions are applied simultaneously.

However,
```
S(S(hello)) = sasso
```

## Substitutions in Type Systems
Hindley-Milner type inference algorithms use substitutions from type variables to monotypes, applied on types.

```
S = {a |-> int}

S(a -> bool) = int -> bool

```
Here is a substitution from a type variable to a monotype.

```
S = {
	a |-> b
	b |-> int
}

S(a -> b) = b -> int
S(S(a -> b)) int -> int
```

See [[Combining Substitutions]] and [[Unifying Substitutions]]