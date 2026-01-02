```
S1 = {h |-> j}
S2 = {o |-> y}

S2(S1(hello)) = jelly
```
How can we combine/compose these to S3?

```
S1 = {h |-> i}, S2 = {o |-> h}
S3 = {h |-> i, o |-> h}

S2(S1(oh)) = S3(oh) = hi
S1(S2(oh)) = ii
```
Note combining substitutions in different orders affects the outcome.

```
combine(S1, S2)(thing) = S1(S2(thing))
```

What is combine(S1, S2)?
```
S1 = {a |-> y, b |-> d}
S2 = {a |-> b}
```

Here is the table for mappings:

| s   | S2(s) | S1(S2(s)) |
| --- | ----- | --------- |
| a   | b     | d         |
| b   | d     | d         |
So `combine(S1, S2)` is:
```
S3 = {a |-> d, b |-> d}
```
Note y is omitted at the end!

Here is another combining example:
```
S1 = {a |-> y, b |-> d}
S2 = {a |-> b, b |-> a}
```

What is combine(S1, S2)?

| s   | S2(s) | S1(S2(s)) |
| --- | ----- | --------- |
| a   | b     | d         |
| b   | a     | y         |
```
S3 = {a |-> d, b |-> y}
```
