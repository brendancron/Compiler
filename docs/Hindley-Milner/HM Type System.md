
What makes up a type in the HM type system?

## Monotypes
Monotypes are the building blocks of HM. Its important to know that.

Type variables - like expr variables, but store a type instead of a value. Its important that that value is well defined, just unknown, unlike forall modifiers.
a, b, c, etc.

Type function application

List bool - 1 type arg
Int -> Bool - 2 type args
Bool - is actually a type function application - 0 arguments?
a -> a - Can take type variables
List a

The above are monotypes.
```
T = a                 //variable
  | C t1, t2 .. tn    // application
```

## Polytypes
Polytypes allow us to perform parametric polymorphism.

For-all quantifiers
```
Aa. a -> a
```
For any type, it maps back to the type (identity function)

How do these differ?
```
Aa. a -> a
b -> b
```
a can take any value
b can not take any value bc b is defined.

Example:
```
id (odd (id 3))
```
Lets say id is type:
```
Aa. a -> a
```
This works since id can go from int -> int or bool -> bool.
But if id is type: 
```
b -> b
```
The first call turns it into `int -> int` and the second throws a type error since it is already cast to int.

Here is the grammar for polytypes:
```
T = a                 //variable
  | C t1, t2 .. tn    // application

o = T                 // Monotype
  | Aa. o             // quantifier
```
