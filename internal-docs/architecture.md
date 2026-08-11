# bootstrap2 architecture

```mermaid
flowchart TD
    subgraph elab["Elaboration — run to a fixpoint"]
        direction TB
        check["Typecheck"] --> mono["Monomorphize"] --> resolve["Resolve"]
        mono -. "specialized bodies need checking" .-> check
        resolve -. "a selected impl may be generic" .-> mono
    end

    src["source.cx"] --> scanner["Scanner"]
    scanner -->|"tokens"| parser["Parser"]
    parser -->|"AST"| desugar["Desugar"]
    desugar -->|"desugared AST"| meta["Metaprocess"]
    meta -->|"meta-free AST"| check
    meta -. "compiles and runs each block's dependencies" .-> meta
    resolve -->|"elaborated AST"| reflect["Reflect"]
    reflect -->|"reflected AST"| cps["CPS"]
    cps -->|"CPS AST"| interp["Interp"]
    interp --> out["stdout"]
    interp -. "supplied to metaprocessing as the evaluator" .-> meta
```

Every pass that constructs nodes invents their type annotations rather than getting them from inference — `Resolve` when it turns an operator into a call, `Monomorphize` when it copies a body, `CPS` when it builds continuations. Those annotations are checked, not trusted. The tree is verified after each such pass.

The case that makes it necessary rather than tidy is effect rows. `CPS` decides what to convert by reading the row off a call's annotation, so a synthesized call carrying an empty row is skipped, and the program performs an unhandled effect at runtime with no compiler error anywhere.
