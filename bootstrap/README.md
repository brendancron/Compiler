# bootstrap — superseded

The first Cronyx compiler, in Rust. `bootstrap2/` (OCaml) replaced it and is
what the tests, the CI job and the release script build.

**It no longer compiles current Cronyx.** `stdlib/` and `tests/` were rewritten
for bootstrap2 and this compiler cannot load them — every one of its own tests
fails, most of them before reading a fixture. Nothing here is expected to pass,
and nothing runs it.

It is kept because it is the only thing that ever emitted native code: `codegen/`
is an LLVM backend behind `--compile --out`, which bootstrap2 has no answer for.
The nine `tests/compile/*` fixtures are parked against that gap.

Read it for the codegen, or for how a pass was done the first time. Do not
expect to run it.
