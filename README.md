# Slicer — CIL-Based C Source Reducer

Reduces C source code using [goblint-cil](https://github.com/goblint/cil) AST transformations while preserving exact program behavior (stdout + exit code). If the reduced output is larger than the original, the original source is kept as-is.

## Dependencies

- **OCaml 5.x + opam** — [opam.ocaml.org](https://opam.ocaml.org/)
- **goblint-cil** — `opam install goblint-cil`
- **gcc** — system package manager
- **csmith** — system package manager (for test generation)
- **perf** — `linux-tools` package (optional, for CPU cycle measurement)

## Setup

Install opam and initialize it, then install the OCaml dependency:

    opam init
    eval $(opam env)
    opam install goblint-cil

Install csmith and locate its runtime headers (needed for compiling generated tests):

    sudo apt install csmith
    # Find the include path — typically /usr/include/csmith or similar
    ls /usr/include/csmith-*/

## Build

    opam exec -- dune build

## Generating Test Cases

Use `bench.sh` to generate csmith programs into the `suite/` directory:

    ./bench.sh --no-run                    # generate 1000 programs, don't run pipeline
    ./bench.sh --no-run -n 50              # generate 50 programs
    ./bench.sh --no-run -n 100 --seed 42   # 100 reproducible programs from seed 42

## Running the Pipeline

### Single file

    ./pipeline.sh examples/example.c

### Directory of files

    ./pipeline.sh suite/

### Run on the generated suite (skip generation)

    ./bench.sh --no-gen

### Full benchmark (generate + run)

    ./bench.sh
    ./bench.sh -n 100 --seed 42

Results go to `output/<basename>/output.c` (reduced source) and `output/<basename>/stats.txt` (metrics).

## Flags

### pipeline.sh

| Flag | Description |
|------|-------------|
| `--quiet`, `-q` | Only print final averaged summary stats |
| `-n`, `--count <N>` | Limit to first N files when given a directory |

### bench.sh

| Flag | Description |
|------|-------------|
| `-n`, `--count <N>` | Number of csmith programs to generate (default: 1000) |
| `-s`, `--suite <DIR>` | Suite directory name (default: `suite`) |
| `-v`, `--verbose` | Show per-file pipeline output |
| `--no-run` | Generate suite only, skip pipeline |
| `--no-gen` | Skip generation, run pipeline on existing suite |
| `--seed <N>` | Starting seed for reproducible generation |

### Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `CC` | `gcc` | C compiler |
| `CFLAGS` | `-O0 -w` | Compiler flags |
| `RUN_TIMEOUT` | `3` | Seconds before killing a running binary |
| `CSMITH` | `csmith` | Path to csmith binary |
| `CSMITH_INCLUDE` | `/usr/include` | Path to csmith runtime headers |
