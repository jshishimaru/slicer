# Slicer — CIL-Based C Source Reducer

A minimal pipeline that reduces C source code using [goblint-cil](https://github.com/goblint/cil) AST transformations while preserving exact program behavior (stdout + exit code).

## Prerequisites

| Tool | Install |
|------|---------|
| OCaml 5.x + opam | [opam.ocaml.org](https://opam.ocaml.org/) |
| goblint-cil | `opam install goblint-cil` |
| gcc | System package manager |
| perf | `linux-tools` / `perf` package (optional, for cycle measurement) |
| csmith | System package manager (optional, for benchmarking) |

## Build

```bash
opam exec -- dune build
```

## Usage

### Single file

```bash
./pipeline.sh examples/example.c
```

### Directory of C files

```bash
./pipeline.sh examples/
```

Each file gets its own output directory under `output/<basename>/` containing `output.c` (reduced source) and `stats.txt` (before/after metrics).

### Quiet mode

```bash
./pipeline.sh --quiet examples/    # only print final averaged stats
```

### Benchmarking with csmith

```bash
./bench.sh                           # generate 1000 programs + run pipeline
./bench.sh -n 100 --seed 42         # 100 reproducible programs
./bench.sh -n 50 -v                  # verbose per-file output
./bench.sh --no-run                  # generate suite only
./bench.sh --no-gen                  # run pipeline on existing suite
```

The suite is saved in `suite/` and preserved between runs.

## Pipeline

```
input.c → gcc -E -P → slicer (CIL transforms) → output.c → correctness check + perf
```

**Transformations** (applied in order):
1. **Constant folding** — evaluate compile-time expressions
2. **Remove unused** — strip globals unreachable from `main`
3. **Remove empty functions** — drop functions with empty bodies
4. **Remove unused** — second cleanup pass

## Output

- `output/<name>/output.c` — reduced C source
- `output/<name>/stats.txt` — bytes, lines, semicolon-LOC, CPU cycles, correctness, stdout diff

## Project Structure

```
bin/slicer.ml          Orchestrator (parse → transform → emit)
lib/transform.ml       Transform.t type definition
lib/fold_constants.ml  Constant folding transform
lib/remove_unused.ml   Dead code removal (RmUnused)
lib/remove_empty_functions.ml  Empty function removal
lib/transforms.ml      Pipeline registry
lib/stats.ml           Semicolon-LOC counter
pipeline.sh            End-to-end shell orchestrator
bench.sh               Csmith benchmark generator + runner
context.md             Detailed CIL API reference & docs
```

## Adding a transform

1. Create `lib/my_transform.ml` with a `transform : Transform.t` value
2. Add it to the `pipeline` list in `lib/transforms.ml`
3. `opam exec -- dune build && ./pipeline.sh examples/example.c`

See `context.md` §9 for full examples.

## Environment variables

```bash
CC=clang CFLAGS="-O2 -w" ./pipeline.sh input.c
```
