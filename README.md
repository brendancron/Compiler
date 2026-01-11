# Compiler

## Make commands

```zsh
make run FILE=examples/vanilla/hello.cx
```

# rust_comp Runner

## Build
    cargo build

## Run

From a file:
    cargo run -- path/to/file.cx

From stdin:
    cargo run -- -

## Output
Artifacts are written to ../out.
Relative embed paths resolve from the input file’s directory (or . for stdin).
