
Free variables in types.
```
FV(a) = {a} 
FV(C t1, t2 ... tn) = FV(t1) U FV(t2) U ... FV(tn)

FV(Aa. o) = FV(o) - {a}
```

HM Context Synax
```
Γ = (empty)
  | Γ, e: o 
```
What is `FV(Γ)`
```
FV((empty)) = {}
FV(Γ, e: o) = FV(Γ) U FV(o)
```

## Examples
### Example 1: Type FV
What are the free variables in this type?

```
Ab. List (b -> (a -> b))
```

```
FV(a) = {a}
FV(b) = {b}
FV(a -> b) = {a, b}
FV((b -> (a -> b))) = {a, b}
FV(List(b -> (a -> b))) = {a, b}
FV(Ab. List (b -> (a -> b))) = {a}
```

### Example 2: Context FV
What are the free variables in this context?
```
Γ = 
  x : int,
  y : Ab. b -> a
```

```
FV((empty)) = {}
FV(int) = {} // type function application
FV(x: int) = {}
FV(a) = {a}
FV(b) = {b}
FV(b -> a) = {a, b}
FV(Ab. b -> a) = {a}
FV(Γ) = {a}
```