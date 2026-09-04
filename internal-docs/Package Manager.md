 # Package Manager

Status: **not built.** No manifest, no resolver, no registry, no `cx` driver. This document is the plan.

The tool ships alongside the compiler and is the boundary where the [Modules](Modules.md) design ends: cycles are legal *inside* a package and rejected *between* them, and an interface is what crosses. Everything else here follows from that one line.

## What the tool is called and what it does

`cx`. One binary, subcommands. The compiler is a library it links against, not a program it shells out to — a `cx build` that fork/execs the compiler pays the OCaml startup cost per invocation and loses structured diagnostics to a pipe.

Six subcommands cover the whole model. Each one is small; extra flags earn their place the same way a comment does.

```
cx new <name>            create a package skeleton
cx build                 compile the current package
cx run [-- args…]        build then execute the entry
cx test [filter]         run the fixture suite
cx add <pkg>[@<req>]     record a dependency, resolve, lock
cx publish               upload to a registry
```

`fmt`, `doc`, `bench`, `check`, `clean`, `update`, `search`, `login`, `yank`, `tree` follow later. None of them change the model.

## The tool lives in the compiler's repo

`cx` is a sibling of `bootstrap/` in this repository, not a separate one. The rule from the section above — the compiler is a library the tool links against — makes the seam a build-graph edge, not a network of pinned repos. Splitting them would mean a version-pinned dependency between two trees the same people own, which is friction with no payoff: every compiler change that touches a public type would need a coordinated two-repo pull request.

```
CronyxLang/
  bootstrap/              the compiler: library + `cronyxc` binary
  cx/                      the package manager, links `bootstrap` as a library
  stdlib/                  shared
  tests/                   shared: `cx test` and `dune test` read the same fixtures
  internal-docs/
```

Three consequences.

- **One release ships one artefact.** The toolchain tarball carries `cronyxc` and `cx` together; a split repo means two release trains that must stay in sync anyway, and every mismatch is a bug report.
- **Fixtures are not duplicated.** `tests/` is the same set of `.cx` files under both `dune test` (the compiler's own suite) and `cx test` (the package tool driving the compiler as a library). A split would need a submodule or a copy, both worse.
- **The Cargo / Rust split is historical.** Cargo predates `rustup` and predates being part of the release train; the seam is one the Rust project has spent effort papering over rather than defending. Go, Deno, Bun, and Zig keep the tool in-tree, and none regret it.

**When to split.** `cx` self-hosts in Cronyx *and* has enough users that its release cadence needs to decouple from the compiler's. Neither is close, and the split at that point is a mechanical `git filter-repo`, not a design change.

## Toolchains: `cx` installs its own compiler

The `cx` on `$PATH` is a **shim**. It reads the package's compiler requirement, resolves it to an installed toolchain, and execs into it. A missing toolchain is downloaded and installed on first use, not a diagnostic asking the user to go get one. This is the Go / Deno / Bun shape rather than the Rust `rustup` split, and the reason is one line: a language young enough not to have channels (stable / beta / nightly) does not need a second tool to manage them.

What this buys, in order of importance:

- **A fresh clone builds.** A repo pinned to `cronyx = "2026.1"` on a machine that has never seen 2026.1 downloads it and runs. There is no "install the right compiler first" step because the tool doing the build is the tool that installs compilers.
- **The shim is stable, the toolchain moves.** Users install `cx` once. Every compiler release lands as a new toolchain the shim can dispatch to, not a replacement for `cx` itself. A user with a two-year-old shim opens a repo pinned to a two-week-old compiler and it works.
- **CI is one line.** `curl https://cronyx.dev/install | sh` gives a machine `cx`, and every subsequent build resolves and downloads its own toolchain. There is no matrix of preinstalled compiler versions to maintain.

```
~/.cronyx/
  bin/cx                              the shim, on $PATH
  toolchains/
    2026.1.4/
      bin/cronyxc                     the compiler binary
      lib/…                           prelude and stdlib for this version
    2026.2.0/
      …
  registry/                           the cache from the previous section
```

**`cronyx = "…"` is the compiler toolchain requirement.** A machine running `cx build` with a compiler that does not satisfy it installs one that does — silently, once, cached. The lockfile pins the exact version resolved.

A separate `edition` field for language dialects (the way Rust distinguishes 2018 / 2021 / 2024 source) is *not* shipped: there is one edition, so the field would always carry the same value. It arrives when a second edition does.

**The shim's job list is short.**

1. Find the package root (walk up looking for `cronyx.toml`).
2. Read `[package].cronyx` and the lockfile.
3. If the pinned toolchain is not installed, fetch it from `toolchains.cronyx.dev/<version>/<target>` and verify its checksum.
4. Exec `~/.cronyx/toolchains/<version>/bin/cronyxc` with the original argv.

Everything else — resolution, caching, publishing — lives in the toolchain, so a shim update is not needed to ship new package-manager features. This split matters: a tool that has to update itself to give users new functionality is a tool users refuse to update.

**Outside a package**, the shim uses the highest installed toolchain, or the one named by `~/.cronyx/config.toml`'s `default = "…"`. `cx toolchain install <version>` and `cx toolchain list` handle the manual side; the automatic side is what carries the weight.

**One caveat.** Compiler releases and shim releases have different cadences. The shim is deliberately tiny (a manifest parser, an HTTP fetch, an `execve`) so that it almost never needs to change; anything that grows the shim's job list is a design smell. If a new toolchain needs a shim capability the installed shim lacks, it fails with a diagnostic naming `cx self-update`, and the user runs one command. This is the only case where the shim's version matters, and keeping it rare is a discipline the shim's scope enforces.

## A package is a directory with a manifest

```
mypkg/
  cronyx.toml              the manifest
  cronyx.lock              the resolved graph, generated
  src/
    main.cx                the entry, for a binary
    lib.cx                 the root module, for a library
  tests/
    …                      .cx / .txt fixtures, the way this repo already runs them
  target/                  build output, in .gitignore
```

`cronyx.toml`, not `cronyx.json`: comments matter in a file humans edit, and JSON has none. Not `Cronyx.toml` capitalised — the file is code, not a title.

```toml
[package]
name    = "mypkg"
version = "0.3.1"
cronyx  = "0.1.1"         # minimum compiler toolchain; `cx` installs it if missing

[dependencies]
http    = "1.4"            # ^1.4 by convention, per SemVer
json    = { version = "0.9", features = ["derive"] }
tui     = { git = "https://…", rev = "abc123" }
local   = { path = "../local" }
```

**This is the MVP surface.** `[dev-dependencies]`, `[build-dependencies]`, `[features]`, `[[bin]]`, `[lib]` and `[workspace]` each land with the feature that needs them — the sections below describe those additions. A key earns its place in `[package]` the way a comment earns its place in code: only when omitting it costs the reader something.

Two things the manifest deliberately will *not* grow, even when the rest does:

- **No `[workspace]` in the leaf.** A workspace is a separate root manifest that lists members. Repeating it in every leaf is where Cargo grew a class of confusing errors.
- **No `[patch]` at the leaf either.** Overrides live in the workspace or user config, never in a manifest that ships to a registry.

## The compilation unit

**A package is a compilation unit.** One `Type_mono` run, one prelude, one global name space per package. This is what makes cycles inside a package legal — the [Modules](Modules.md) design already turns each unit's declarations into `unit__name` and concatenates. A package is the largest tree that goes through the pipeline together.

**A dependency is compiled separately** and consumed as an *interface plus a monomorphizable body*. `Type_mono` runs on typed IR; a compiled dependency must therefore ship the typed body of every exported generic, the way a Rust rlib ships generic MIR. Non-generic bodies can ship as their post-CPS form. Nothing about this decision is free — it is the item [Modules.md](Modules.md) called out as "a design rather than a task", and it is the largest thing this project buys.

**Cycles between packages are rejected at resolution**, before any compilation. The resolver's graph is a DAG or the tool refuses. This is the second half of what `import "../../../stdlib/…"` currently sidesteps: relative paths reach anywhere the filesystem does, and a package boundary is a name the tool understands rather than a path it walks.

**Path imports across package boundaries are forbidden.** Inside a package, `import "../other/thing"` stays legal — a package is a set of files under one root. Reaching *outside* that root goes through `[dependencies]` and a name, so the resolver sees it. This is the one rule that turns Modules' informal boundary into an enforceable one.

## Versions, requirements, and resolution

Semantic versioning, unmodified. `major.minor.patch`, with `-pre` and `+build` suffixes read but not compared beyond pre-release ordering.

A requirement is a *set* of acceptable versions. The manifest DSL is Cargo's, chosen for reader recognition:

| Written | Means |
|---|---|
| `"1.4"` or `"^1.4"` | `>=1.4.0, <2.0.0`, the default |
| `"~1.4"` | `>=1.4.0, <1.5.0` |
| `"=1.4.2"` | exactly that version |
| `">=1, <2"` | a raw range |
| `"1.4"` on a `0.x` package | `>=0.4.x, <0.5.0` — 0.y is treated as major, the SemVer convention |

Resolution is one function: given the manifest's requirements and a registry index, produce a set `{ package → version }` satisfying every requirement, with the constraint that a single graph carries **one version per package name** for each *public* dependency and *at most two majors* for private ones. This is stricter than Cargo (which allows any number of majors) and is what lets a type from one package flow through another without confusion. It costs the ability to have five versions of the same library linked at once; the language does not want that either.

**MVS or PubGrub.** Go's Minimum Version Selection is simpler to explain, produces reproducible builds without a lockfile, and eliminates the class of "my machine picked a different patch" bugs. PubGrub gives better diagnostics ("A wants B ^1, C wants B ^2, so no version of B works"). The initial resolver is PubGrub — the diagnostic quality is worth the implementation cost, and the class of user error the resolver reports on is exactly the class we want to be precise about.

**The lockfile is authoritative for `build`.** `cx build` reads `cronyx.lock` and never contacts a registry. `cx add`, `cx update`, and a lockfile-absent state are the only paths that resolve. This is the invariant users depend on: a green build stays green.

## The lockfile

`cronyx.lock`, generated, checked in for **binaries**, checked in for **libraries too** — the Rust convention of omitting it for libraries saves nothing and makes bug reports irreproducible.

```toml
version = 3

[[package]]
name = "mypkg"
version = "0.3.1"
dependencies = ["http 1.4.7", "json 0.9.2"]

[[package]]
name = "http"
version = "1.4.7"
source = "registry+https://packages.cronyx.dev"
checksum = "sha256:…"
dependencies = ["bytes 0.1.4"]
```

`checksum` covers the source tarball, not the compiled artifact. Binary caches are a separate concern (below) and have their own hash.

## The registry

**One canonical registry** with a public HTTP API, served at `packages.cronyx.dev`. Third-party registries reachable by URL; the manifest names them:

```toml
[registries.internal]
index = "https://packages.corp.internal"

[dependencies]
sekrit = { version = "1", registry = "internal" }
```

The index is a **git repository of JSON files**, one file per package, sharded by name prefix. This is the design crates.io eventually moved away from (to a sparse HTTP protocol) because the git clone grew to gigabytes; a sparse HTTP endpoint over the same files is the design to start with. `cx` fetches only the files it needs.

Publishing is `PUT /api/v1/crates/new` with an API token and a tarball, the tarball's manifest signed by the token's account.

**No auto-yank on vulnerability.** A yanked version still resolves for a lockfile that already selected it — otherwise a security disclosure breaks every downstream build simultaneously, which is not the disclosure's intent. New resolutions skip yanked versions with a diagnostic.

## Caches, directories, offline

```
$XDG_CACHE_HOME/cronyx/
  registry/
    index/                 the sparse-fetched JSON, per-registry
    src/                   unpacked source, one directory per <name>-<version>
    bin/                   compiled artefacts, keyed by content hash
  git/
    checkouts/             git-source deps, per revision
```

`target/` is per-package, per-profile:

```
target/
  debug/                   the default profile
    build/                 build-script outputs
    deps/                  intermediate compiled units
    <name>                 the final binary
  release/
```

**Offline is a first-class mode.** `cx build --offline` refuses network access and errors if the cache lacks something. `cx build --frozen` additionally errors if the lockfile would need updating. Both are what CI wants; both are what a plane wants; the only way to reach that reliably is to make them modes, not the accidental effect of a warm cache.

## Profiles

Two by default: `debug` and `release`. A profile is a set of compiler flags:

```toml
[profile.debug]
opt = 0
checks = "all"             # bounds, overflow, effect assertions
debug-info = "full"

[profile.release]
opt = 3
checks = "safety"          # bounds and overflow stay; assertion-style checks compile out
debug-info = "line-tables"
```

Not four (`dev`, `release`, `test`, `bench`) — `test` and `bench` inherit from `debug` and `release` respectively, and the split only exists to let a user tune them separately, which nobody does. One rule.

## Features

A feature is a **compile-time flag** the manifest names. A dependency's feature set is part of the resolution key: if two paths through the graph ask for different features of `X`, the resolver unions them and compiles `X` once with the union. This is Cargo's model and it is the right one — the alternative (compile `X` twice) loses type identity across the graph.

```toml
[features]
default = ["std"]
std     = []
async   = ["dep:runtime"]  # pulls in an optional dep
serde   = ["dep:serde", "json?/serde"]   # forwards to a dep's own feature
```

Features are **additive**. A feature must not remove behaviour, only add it; this is the invariant that lets union resolution be correct. A feature that turned an API off would silently break every consumer that expected it on. The tool cannot enforce additivity — it is a contract on the publisher — but the manifest schema makes the intent visible.

**No global features.** A workspace-level default that flips a feature in a transitive dep is how Cargo builds end up differing between `cargo build` at the leaf and `cargo build` at the root. Not shipped.

## Workspaces

A root manifest that lists members:

```toml
[workspace]
members = ["compiler", "runtime", "tools/*"]
resolver = "2"             # placeholder; there is only one resolver until there is a reason for a second
```

Every member shares one `cronyx.lock` and one `target/`. The workspace is where `[patch]` lives, where a `[profile.release]` override lives, and where cross-member deps write `{ path = "…" }` without version. A single `cx build` at the root builds every member.

The workspace is *not* a compilation unit. Each member is still one `Type_mono` run. What the workspace shares is resolution, caching, and the lockfile.

## Build scripts and metaprocessing

Cronyx already has `meta` blocks that run at compile time — they are the language's build scripts. A separate `build.cx` in the Rust sense would be a second mechanism doing the same job, which the [Modules](Modules.md) design's five decisions specifically rule out.

What the manifest *does* need is `[build-dependencies]`: packages available to `meta` blocks but not to runtime code. The distinction matters because a build-dep may itself pull in native code or heavy transitive deps that should not end up in the shipping binary.

A `meta` block that calls `readfile` or `embed` participates in the build's input hash — anything read at build time is a build input, and rebuilding when it changes is the tool's job.

## Reproducibility

**One version per package per resolution** (above) is the largest source. The rest is a checklist:

1. **Timestamps are zeroed** in compiled artefacts. The one place a build embeds the wall clock is the one place two identical builds diverge.
2. **Paths in diagnostics are relative to the package root**, never absolute. `Ast.locate` already renders relative to the entry's directory; the tool extends that to *package* root.
3. **The environment the build sees is empty** unless the manifest names variables. A `meta` block that reads `$HOME` is a diagnostic.
4. **The compiler version is part of the lock**, and a mismatch installs the pinned toolchain rather than warning. See [Toolchains](#toolchains-cx-installs-its-own-compiler) — the lockfile records the exact `cronyx` version resolved, and `cx build` fetches it if the machine lacks it. A build on a fresh machine cannot silently use the wrong compiler because the tool never falls back to whatever is on `$PATH`.

## Publishing and trust

**A published package is immutable.** Once `1.4.7` exists, no further upload of `1.4.7` succeeds. Yanking hides from new resolutions; it does not overwrite.

**A checksum is over the tarball**, computed by the registry and returned to the client. A client that already had a copy verifies the checksum before using it. A registry that returns a different tarball for a version it has already served is a bug the client detects.

**Signing is deferred, not skipped.** The manifest reserves `signature = "…"` under `[[package]]` in the lock so a signature check can be added without a format change. What is deferred is the PKI: whether it is Sigstore, a web of trust, or a registry-signed attestation is a policy question, and shipping the wrong one is worse than shipping none.

## Diagnostics

The tool's diagnostics obey the same rules as [Diagnostics.md](Diagnostics.md) — a frame does not render half-drawn, spans carry their file. Two new categories:

**Resolution diagnostics** cite the requirement chain that led to the conflict. PubGrub already produces this; the renderer is what turns "no version of `B` works" into

```
error: no version of `bytes` satisfies every requirement.
  ─ mypkg 0.3.1 requires bytes >=0.1, <0.2
  ─ http 1.4.7 (via mypkg) requires bytes >=0.2, <0.3

  http 1.4.7 was chosen because mypkg requires http ^1.4 and 1.4.7 is the newest.
  A version of http that requires bytes ^0.1 would satisfy both; there is none.
```

**Lock drift diagnostics** name what changed and why: `bytes 0.1.4 → 0.1.5 (patch, transitive via http)`.

## What this does not do

**A monorepo build system.** Bazel-style hermetic remote execution, per-file caching, distributed builds are a different tool. `cx` caches per compilation unit and no finer. A team that needs the former can drive `cx build` from Bazel.

**Native code, C libraries, `pkg-config`.** Cronyx does not yet have an FFI. When it does, `[dependencies.native]` or a `link = "…"` field arrives with it; not before.

**A binary distribution channel.** Publishing pre-compiled binaries is a registry feature and a security surface, and it is orthogonal to the package model. Users who want it can put a URL in their manifest today via a `git` source pointing at a tag.

**Language editions.** There is one edition; no `edition` field. When a second arrives, packages declare it in `[package]`, the resolver accepts a mixed graph, and each package compiles under its own edition — but `meta` runs under the package that wrote it, so a macro never sees a foreign edition. Rust's editions are the reference; the commitment is smaller until it needs to grow.

**`cargo install`-style global installs of binaries** ship at the same time as `cx publish`, since they are the same feature from the other side: a registry that serves source can serve a compiled binary for a target the tool asks for. Deferred until there is a registry.

## Open

**Public vs private dependencies.** The rule "one version per public dep, up to two majors per private dep" needs a manifest syntax and a resolver that respects it. `public = true` on a dependency is the obvious spelling; the semantics are what needs pinning down. Rust's RFC 1977 is the reference and the caveats.

**Feature unification across dev-deps.** If `mypkg`'s dev-deps enable a feature of a runtime dep, does the runtime build see it? Cargo's answer changed once (resolver v2) and the change was disruptive. The design here is: no. Dev-deps live in their own resolution scope, sharing the runtime deps' versions but not their feature sets. This needs a fixture before it is a decision.

**How the compiler consumes a dependency.** A dependency ships typed IR for generics and post-CPS IR for the rest, but *the format* is not designed. Whether it is the compiler's own serialised AST types, a stable IR with its own schema, or something in between decides how quickly the compiler can evolve. The `bootstrap` types change often enough that a stable schema is not free.

**Registry federation.** Two registries defining a package with the same name is a collision waiting to happen. Namespacing by registry (`internal:sekrit`) at the manifest is one answer; a global-name-per-registry policy is another. This is a governance decision as much as a technical one.

**A `cx.workspace.toml` or a `[workspace]` in a leaf manifest.** Both work. The former reads better in a tree with one workspace; the latter matches Cargo's shape. This is small enough to leave for the first user.
