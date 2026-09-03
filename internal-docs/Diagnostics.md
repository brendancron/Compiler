# Diagnostics

How an error reaches the user: what a span is, and why the renderer cannot draw
half a frame.

```
× Type error: Expected int, got string.
  ┌─ sub/lib.cx:2:20
  │
2 │     var bad: int = "nope";
  │                    ~~~~~~
  └─
```

## What the Rust compiler got wrong

`bootstrap/src/error.rs` built a `Diagnostic` whose location, source line,
underline column, underline length, label and help were each an `Option`, and
rendered every section under an `if let`. Every combination of those six
compiled, and most of them draw something broken. Two showed up constantly:

- **The source line goes missing.** `source_line` ended in
  `.lines().nth(line - 1).unwrap_or("")`, so a line number the file did not have
  rendered an empty snippet rather than failing.
- **The bottom of the frame goes missing.** The closing `└─` was written inside
  `if let Some(help)`, so a diagnostic with no help text ended mid-frame.

Neither is a typo. Both follow from letting the parts of a frame be individually
absent and having the renderer decide what to skip.

There was a third, upstream of those. Spans lived in a
`HashMap<node_id, (line, col)>` built by the parser, and the staged ASTs remapped
node ids, so the lookup missed for whole classes of error. `enrich_diagnostic`
compensated by *searching the source text* — scanning for the first whole-word
occurrence of a variable's name, or for a line containing `for ((`. A diagnostic
that finds its own location by grepping is pointing at the wrong one as often as
not.

## Spans

`lib/source_map.ml` owns the text and the spans into it. A `Span.t` is a file
plus a byte range, and the file it names is a `File.t` holding the text and a
table of line starts — so resolving a span to a line is a binary search in data
the span already carries, not a lookup in a table that may not have the entry.

`Span.t` is abstract behind `source_map.mli`, and `Span.of_range` is the only way
to build one. That constructor clamps `lo` and `hi` to the file it was given, so
every span in existence points at text that is there. The invariant is what makes
`Span.view` total:

```ocaml
type view =
  | Located of located   (* line, column, the line's text, the underline's bounds *)
  | Nowhere_in_source
```

`view` resolves the span all the way to what a renderer needs, so the clamping
happens once here rather than in each consumer. Line and column are derived on
demand and stored nowhere, which is why a span can widen without a second field
having to agree with it.

**Absence is a constructor, not a missing field.** A node the compiler invented
has `Span.nowhere`, and `Nowhere_in_source` is a case the renderer answers for —
it draws a complete frame that says there is no location. It cannot silently drop
a row from a frame it is halfway through. Since the OCaml build runs with
`-warn-error +a`, a fourth view would break the build at the render site rather
than being skipped there.

Columns are counted in bytes from the start of the line, which is what
`[line:col]` in an `.err` fixture means.

## The frame

`lib/render.ml` draws it, and holds the only knowledge of colour. Two rules:

- **Nothing structural is conditional.** The rows between the rules vary; the
  rules do not. There is no field whose absence removes a line.
- **The gutter is as wide as the line number that sits in it,** so the rules and
  the source line hang off the same column. The Rust version indented the header
  by a fixed two spaces and the source line by the number's own width, which
  agreed only for single-digit line numbers.

Tabs are expanded to eight-column stops in both the source line and the underline
beneath it, and a UTF-8 sequence counts as one column rather than one per byte.
A character the terminal draws double-width — an emoji, most CJK — still counts
as one, so an underline after one sits a column left of where it should. Fixing
that needs a width table, and has not been worth one.

Colour is ANSI when stderr is a terminal and `NO_COLOR` is unset, and the same
code path otherwise with an empty escape for every entry, so the two cannot
drift.

## What is not here

`Diagnostic.error` carries a stage, a span and a message, and nothing else. The
Rust version's `help:` line was often the same generic advice at every site
("check for mismatched brackets, missing semicolons, or typos"), which is why it
is not being ported wholesale. A note field earns its place when there are
diagnostics that genuinely have something to add.

Every span starts life as one token's extent, so an underline covers the token a
pass reported against rather than the whole expression. Joining a node's span to
its children's in the parser would widen them, and needs nothing from this
design that is not already here.

## Tests

The fixture suite compares `[line:col] message` against a `.err` file, which
would not notice a frame missing its source line or its closing rule. The
renderer is pinned separately by `run_rendering` in
`bootstrap2/test/test_bootstrap2.ml`: a span inside a line, a two-digit line
number for the gutter, and a span with no source behind it.
