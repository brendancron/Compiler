# Internal documentation

Design notes, plans, and rationale for the Cronyx compilers themselves.

This is separate from `docs/`, which is the Docusaurus site published for language *users*. Nothing here ships. Write for the person implementing the compiler — including future you — and prefer recording *why* a decision was made over restating what the code does.

| Document                                                     | Subject                                                               |
| ------------------------------------------------------------ | --------------------------------------------------------------------- |
| [Architecture.md](Architecture.md)                           | `bootstrap2` pipeline, a heading per pass                             |
| [Type System.md](Type%20System.md)                           | Type system and type checking for `bootstrap2` (OCaml)                |
| [Algebraic Effects.md](Algebraic%20Effects.md)               | Effect rows and the selective CPS pass                                |
| [Function Values That Suspend.md](Function%20Values%20That%20Suspend.md) | Why a closure cannot yet carry a delimited effect, and the plan  |
| [Data Structures.md](Data%20Structures.md)                   | The datatypes and how each is used                                    |
| [Collection Literals.md](Collection%20Literals.md)           | How `[...]` picks a collection type                                   |
| [Elaboration.md](Elaboration.md)                             | Type-directed resolution of operators, indexing, and literals         |
| [Comptime Params.md](Comptime%20Params.md)                   | `<>` parameters, and why they are not just generics                   |
| [Modules.md](Modules.md)                                     | `import`, and why several files become one program                    |
| [Metaprocessing.md](Metaprocessing.md)                       | `meta` and `gen`, and compilation calling itself                      |
| [Reify.md](Reify.md)                                         | Turning a compile-time value back into syntax                         |
| [GADT Refinement.md](GADT%20Refinement.md)                   | Refining a type parameter in a match arm, and the choice of how       |
