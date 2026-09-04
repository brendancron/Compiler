# CronyxLang

A statically-typed, metaprogramming-first language with Hindley-Milner type inference.

See [docs/Cronyx.md](docs/Cronyx.md) for a language overview.

# Installation

It is recommended to install the cronyx compiler via the build manager

## Package Managers

### Windows

#### Scoop

Add the bucket
```
scoop bucket add cronyx https://github.com/brendancron/scoop-cronyx
```

Install
```
scoop install cronyx
```

Update
```
scoop update
scoop update cronyx
```

### MacOS

#### Homebrew

Add the tap
```
brew tap brendancron/cronyx
```

Install
```
brew install cronyx
```

Update
```
brew update
brew upgrade cronyx
```

This installs `cx`, the package manager, and `cronyxc`, the compiler it links.
`cx` is the one to reach for; `cronyxc` runs a single file and nothing else.

# Starting a package

```
cx new hello
cd hello
cx run
```

`cx build` compiles the package and its dependencies, `cx publish` uploads it,
and `cx toolchain list` says which compilers are installed. A package names the
compiler it needs in `cronyx.toml`, and `cx` hands the job to that one.

# Running a program

```
cronyx run main.cx
```

## Flags

| Flag                  | Description                                                           |
| --------------------- | --------------------------------------------------------------------- |
| `--compile`           | Compile to a native binary via LLVM instead of interpreting           |
| `--out <path>`        | Output binary path when `--compile` is set (default: `a.out`)        |
| `--dump-ast`          | Write `meta_ast.txt` and `meta_ast_graph.txt` to the output directory |
| `--dump-typed-ast`    | Write `meta_ast_typed.txt` and `type_table.txt`                       |
| `--dump-staged`       | Write `staged_forest.txt` and `staged_forest_graph.txt`               |
| `--dump-runtime-ast`  | Write `runtime_ast.txt` and `runtime_ast_graph.txt`                   |
| `--dump-runtime-code` | Write `runtime_code.cx` (pretty-printed generated source)             |
| `--dump-cps`          | Write `cps_info.txt` and `cps_code.cx` (after CPS transform)         |
| `--dump-all`          | Enable all `--dump-*` flags                                           |
| `--out-dir <path>`    | Directory to write debug files into (default: `./out`)                |
| `-V`, `--version`     | Print version and exit                                                |
| `-h`, `--help`        | Print help and exit                                                   |

Debug files are only created when at least one `--dump-*` flag is passed. Without any flags the compiler runs and exits with no file I/O beyond the program itself.
