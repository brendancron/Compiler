Zigs symbol discovery along with many other languages follow a 2 phase approach. First a top-level declaration phase occurs and following symbol resolution takes place linking all symbols to their needed code chunks.
The issue with metaprogramming is that symbols may not be statically defined, so symbol discovery really only makes sense to run on code post Metaprocessing when it is lowered to a runtime AST.
So then how should symbol resolution work on meta blocks?

Here is the catch:
Meta blocks only emit ast nodes at their level

During meta lowering, unresolved identifiers produce symbol futures, meta blocks execute when all required futures are satisfied, otherwise they're queued. Meta generated declarations fulfill futures and trigger re-execution. Terminate with error on leftover futures or cycle detection.

Lets say we have the following program:
```cx
meta { foo(); }
meta gen fn foo() { ... }
```
Here we have an issue since the first meta stmt contains a symbol that is only resolved by execution of the 2nd. This is a trivial example, but realistically there is no guarantee that the 2nd produces any nodes. So we need to defer execution until that symbol resolves *somewhere* 

Then we go to the 2nd stmt. Here we do not need to do any symbol resolution for lowering, so we can just execute! The execution runs and we are left with an ast node which needs to be injected into the above ast. That injected node gets symbol registered if needed.

When a symbol is registered, it needs to trigger all meta stmts that were waiting on that symbol, dequeue them, and process them.

Finally after all of that is finished, it moves onto the next stmt for lowering.

GOAL: Make this work exactly like a 2 pass symbol resolver unless the programmer uses a meta stmt.

