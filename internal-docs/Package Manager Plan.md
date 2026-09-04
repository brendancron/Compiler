# Package Manager: implementation plan

[Package Manager.md](Package%20Manager.md) is the design. This is the order it gets built in and what "done" means at each step.

One rule shapes the ordering: **the registry is the easy part and it is the part that looks like progress.** A `cx` that builds a two-package tree over `path` dependencies has solved everything hard — the compilation unit, the boundary, the artifact, the cache. Fetching tarballs over HTTP after that is a week. Every milestone below is therefore chosen to force a hard question early and defer an easy one.

A milestone is done when its criteria hold *and* a fixture in `tests/` covers each one. The suite is the completion criterion; prose here is what the fixture is checking.

## 0. The driver exists

`cx/` beside `bootstrap/`, a dune executable linking `bootstrap` as a library. No manifest, no resolution: `cx run <file>` does what `bin/main.exe` does today, through the library rather than the binary.

This milestone exists to prove the seam. `Pipeline.compile` already returns `(Ast.cps_stmt list, Diagnostic.error list) result`, so diagnostics cross as data and `Render` stays presentation — if that turns out to be false, it is much cheaper to learn here than in milestone 3.

**Done when**

- `cx run` and `bin/main.exe` produce identical output for every fixture in `tests/`.
- `cx` never shells out to `cronyxc`, and no diagnostic reaches the user as a captured string.
- The `--dump-*` flags work through `cx`, since they are the only debugging tool the compiler has.

## 1. The manifest

`cronyx.toml` parses into a typed record: `name`, `version`, `cronyx`, and `[dependencies]` restricted to `{ path = … }`. Nothing else in the schema is read yet, and an unknown key is a diagnostic rather than a silent skip — a manifest that quietly ignores what it does not understand is how a typo becomes a support burden.

The TOML parser is a coin flip between vendoring `otoml` and writing one. `bootstrap` currently declares `(depends ocaml)` and nothing else; that property is worth knowing you are spending, and it is not worth an argument.

`cx new <name>` writes the skeleton: manifest, `src/main.cx`, `.gitignore` naming `target/`.

**Done when**

- A manifest missing `name` or `version`, carrying an unknown key, or naming a malformed version fails with a `Diagnostic` carrying a span into the TOML.
- `cx new` produces a package that `cx run` executes.
- Version requirements parse to the table in the design doc, including the `0.y.z` and `0.0.z` cases, which are where hand-rolled SemVer implementations go wrong.

## 2. The package boundary

The loader gains two parameters. Neither changes what it does; both change what it is allowed to reach.

- **A root.** An `import` that resolves outside the package root is an error naming both the import and the root. Inside the root, `import "../other/thing"` stays legal — a package is a set of files under one directory.
- **A dependency map**, `name → directory`, supplied by `cx`. `import "http/client"` consults the map before the filesystem; a name absent from the map is an unresolved-dependency diagnostic, not a file-not-found.

This is the milestone that makes the design real. Until the boundary is enforced, `import "../../../stdlib/…"` keeps working and the package model is advisory.

**Done when**

- A fixture importing above its package root fails with the boundary diagnostic.
- A two-package tree over `path` deps compiles and runs, the dependency reached only by name.
- No fixture in `tests/` reaches outside its own tree; the ones that do today are migrated in this change, not after it.
- `stdlib` resolves from the toolchain root rather than by relative path, so `import "std/…"` works from any directory.

## 3. Separate compilation

The point of the project and the item [Package Manager.md](Package%20Manager.md) calls "a design rather than a task". A dependency stops being source the consumer recompiles and becomes an artifact it loads.

The format is settled — serialize the compiler's own types, tag with the compiler version, key the cache on the tag, rebuild everything when the types change.

**What an artifact holds today** is the package's declarations after loading and metaprocessing, mangled under the package's own name, plus the names each of its units exports. It stops before the checker: a consumer typechecks and monomorphizes the whole graph at once, which is what lets a generic from a dependency be instantiated at a type the dependency never saw. Shipping *typed* IR — so a consumer need not re-check a dependency's bodies — is the next step and is what makes the artifact an interface rather than a shortcut.

Two consequences of stopping there, both worth knowing before they are discovered:

- **A `meta` block sees only its own package.** Each package is metaprocessed alone, so a `meta` block cannot reach a dependency's declarations. Nothing needs it yet.
- **The standard library is embedded, not linked.** `stdlib` is not compiled to an artifact, so each package that imports it carries what it used, and the link keeps the first declaration of each name. Compiling `std` like any other package retires that.

**Done when**

- A dependency compiled once and consumed from cache produces output identical to the same tree compiled from source. *(`cx/test/packages`, every fixture built cold and then again over its own artifacts.)*
- A generic exported by a dependency is instantiated at a type the dependency never saw. *(`generic_dep`.)*
- Two packages declaring the same unit name link without collision. *(`same_unit_name`.)*
- Two dependencies defining an overlapping impl are rejected, with both sites in the diagnostic. *(`overlapping_impls`.)*
- Deleting `target/` changes nothing but the wall clock. *(The suite deletes every `target/` before each fixture.)*

## 4. The cache

Content-addressed, keyed on an input hash covering: the compiler version, every source file, the manifest, the feature set, the profile, every path a `meta` block read through `readfile` or `embed`, and each dependency's artifact hash.

That `meta` clause is why this milestone is not free. `readfile` runs at meta time in `builtins.ml` and `embed` resolves in `loader.ml`; neither records what it touched. Both hooks land in this change rather than after it — a cache whose hash misses a build input is silently wrong, which is the worst failure a build tool has, and the retrofit invalidates every cache in existence.

For the same reason, `meta` gets no clock and no RNG. A nondeterministic `meta` makes the hash a lie that no amount of tracking repairs.

**Done when**

- A second `cx build` with no change does no compiler work.
- Touching a file a `meta` block read invalidates the package that read it and nothing else.
- Changing the compiler version invalidates everything.
- Two builds of the same tree in different directories produce byte-identical artifacts.

## 5. Toolchains and dispatch

`~/.cronyx/toolchains/<version>/`, the forward-only `bin/cx` copy, and the two-phase dispatch: exec on the root manifest's floor, resolve, exec once more if the graph raised it.

`cx toolchain install` and `cx toolchain list` are in this milestone rather than deferred, because the tool that picks a compiler has to be able to install one.

**Done when**

- A package pinned to a version the machine lacks installs it and builds, with no prior step.
- Installing an older toolchain leaves `bin/cx` alone.
- A `cx` too old to satisfy a manifest fails naming the version and where to get it, never with a parse error.
- Dispatch execs at most twice, and the second exec is provably bounded by the lockfile.

## 6. Resolution and the lockfile

PubGrub over a real requirement graph, with `cronyx` as a lower-bound-only node. `cronyx.lock` written deterministically — sorted, stable, no incidental ordering — because the alternative is diff churn in every user's repository forever.

`cx add`, `cx update`, and the `--locked` / `--offline` / `--frozen` triad, which are three points on two axes and not two points on one.

**Done when**

- A conflict renders the requirement chain in the shape the design doc shows.
- A floor no released compiler reaches is reported as an unsatisfiable requirement, in the same shape as any other.
- The same graph resolves identically on two machines, and re-resolving rewrites the lockfile byte-for-byte.
- `--locked` fails rather than writing; `--locked` with no lockfile is an error rather than a generation.
- `cx build` reads no index. It may fetch an artifact the lockfile names, whose checksum is already pinned.

## 7. The registry

Sparse HTTP index, tarball fetch, checksum verification, `cx publish`. Immutability, yank semantics, and the publishing rules: a `path` dep needs a `version`, tarball contents are declared by `include` / `exclude`, a git dependency locks a commit and a tree hash.

**Done when**

- A published package resolves, downloads, verifies, and builds on a machine that has never seen it.
- Re-publishing an existing version fails.
- A yanked version still resolves for a lockfile that already selected it.
- A tarball whose checksum does not match the lockfile is an error before anything is compiled.
- Publishing a package with a bare `path` dep fails.

## What is not in the plan

**`cx test`.** The subcommand currently promises per-test reporting and failure isolation that the language cannot express — there is no test attribute and no assertion mechanism. Either it means "run the golden-file suite the way `dune test` does" and says so plainly, or it waits on a language design. It is not blocked on anything in this plan and it should not block anything either.

**Features.** They arrive with the registry and not before; a feature is not observable in a tree of `path` dependencies. The design doc's §Features still contains a contradiction to resolve first — a feature set cannot both be part of the resolution key and be unioned into a single build — and the correct model is a fixpoint over version resolution and feature unification, because enabling a feature can add an edge that adds a package that requests features.

**Codegen.** `cx build` emits nothing executable and `cx run` interprets. When a backend lands, `cx build` writes `target/<profile>/<name>` and nothing in this plan changes.

**Workspaces, profiles beyond `checks`, `[build-dependencies]`, `[dev-dependencies]`.** Each lands with the feature that needs it.
