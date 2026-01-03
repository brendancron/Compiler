See [[HM Algorithm W]]
- **Expanded AST**: surface syntax, statements, sugar, names, control flow.
- **HM Core AST**: expression-only, minimal (`Var/Lam/App/Let/Lit`), _only input to Algorithm W_.
- **Typed AST**: same shape as Expanded AST, but every expr annotated with a _final monomorphic type_.
Process
- **Desugar**: `ExpandedAST → HmExpr`  
    This erases statements/sugar (fn decls, blocks, operators, dot, etc.).
- **Infer (Algorithm W)**: `W(Γ, HmExpr) → (Subst, PolyType)`  
    This is _purely functional_, no mutation, no statements.
- **Monomorphize**: apply `Subst` + instantiate `PolyType` → concrete `Type`.
- **Re-type surface AST**: walk `ExpandedAST` again, attaching the inferred concrete types → `TypedAST`.