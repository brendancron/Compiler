# Open questions

Decisions deferred deliberately, with enough context to pick them up cold. A line leaves when it is answered — in the document that owns the subject, not here.

## When a declaration query runs

Owner: [Attributes and Test Frameworks.md](Attributes%20and%20Test%20Frameworks.md)

Metaprocessing generates declarations, so a `gen` can emit a function carrying `@Test`. A meta function that enumerates declarations therefore sees a different program depending on when it runs, and nothing in the source says when that is — it falls out of the metaprocessor's walk order, so adding a file could change which tests exist.

The three answers in the wild: rounds to a fixpoint, as Java's annotation processors do; a hard phase split where generation finishes before any query runs and generated code is invisible to other generators, as C# source generators do; or no answer at all, as Rust proc macros, which is why that ecosystem builds registries at link time instead.

The phase split looks right — every use case so far only reads a finished program and emits a table, never needs its output visible to another generator — but nothing depends on choosing yet, and the first collector that wants to react to another's output is the case that decides it.
