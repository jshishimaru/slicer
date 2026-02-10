#!/usr/bin/env bash
# bench.sh — Generate csmith test suites and run the reducer pipeline
#
# Usage:
#   ./bench.sh [OPTIONS]
#
# Options:
#   -n, --count <N>       Number of csmith programs to generate (default: 1000)
#   -s, --suite <DIR>     Suite directory name (default: suite)
#   -v, --verbose         Show per-file output from pipeline (omit --quiet)
#   --no-run              Generate suite only, don't run the pipeline
#   --no-gen              Skip generation, run pipeline on existing suite
#   --seed <N>            Starting seed for reproducible generation
#   -h, --help            Show this help message
#
# The suite directory is overwritten on a fresh run but never deleted
# after the pipeline finishes.
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PIPELINE="$SCRIPT_DIR/pipeline.sh"
CSMITH="${CSMITH:-csmith}"
CSMITH_INCLUDE="${CSMITH_INCLUDE:-/usr/include}"

# ── Defaults ──
COUNT=1000
SUITE_DIR="$SCRIPT_DIR/suite"
VERBOSE=0
DO_GEN=1
DO_RUN=1
SEED=""

# ── Colors ──
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m'

info()   { echo -e "${GREEN}[bench]${NC} $*"; }
warn()   { echo -e "${YELLOW}[bench]${NC} $*"; }
fail()   { echo -e "${RED}[bench]${NC} $*"; exit 1; }
header() { echo -e "${CYAN}${BOLD}$*${NC}"; }

usage() {
    sed -n '2,/^$/{ s/^# \?//; p }' "$0"
    exit 0
}

# ── Parse arguments ──
while [ $# -gt 0 ]; do
    case "$1" in
        -n|--count)
            COUNT="$2"; shift 2 ;;
        -s|--suite)
            SUITE_DIR="$2"; shift 2 ;;
        -v|--verbose)
            VERBOSE=1; shift ;;
        --no-run)
            DO_RUN=0; shift ;;
        --no-gen)
            DO_GEN=0; shift ;;
        --seed)
            SEED="$2"; shift 2 ;;
        -h|--help)
            usage ;;
        *)
            fail "Unknown option: $1. Use -h for help." ;;
    esac
done

# ── Validation ──
if ! command -v "$CSMITH" &>/dev/null; then
    fail "csmith not found. Install it or set CSMITH=/path/to/csmith"
fi

if [ "$DO_RUN" -eq 1 ] && [ ! -x "$PIPELINE" ]; then
    fail "pipeline.sh not found or not executable at $PIPELINE"
fi

# ── Csmith flags for CIL-friendly output ──
CSMITH_FLAGS=(
    # --no-packed-struct
    # --no-unions
    # --no-bitfields
    # --no-volatiles
    # --no-volatile-pointers
    # --concise
)

# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
# Generation phase
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
generate_suite() {
    header "━━━ Generating $COUNT csmith programs ━━━"

    # Overwrite contents on fresh run, keep the directory
    mkdir -p "$SUITE_DIR"
    rm -f "$SUITE_DIR"/*.c

    local success=0
    local failed=0
    local start_time
    start_time=$(date +%s)

    for i in $(seq 1 "$COUNT"); do
        local outfile="$SUITE_DIR/test_$(printf '%04d' "$i").c"
        local seed_flag=()
        if [ -n "$SEED" ]; then
            seed_flag=(--seed $(( SEED + i - 1 )))
        fi

        if "$CSMITH" "${CSMITH_FLAGS[@]}" "${seed_flag[@]}" > "$outfile" 2>/dev/null; then
            success=$((success + 1))
        else
            warn "csmith failed for program $i, skipping"
            rm -f "$outfile"
            failed=$((failed + 1))
        fi

        # Progress indicator every 50 programs
        if [ $((i % 50)) -eq 0 ]; then
            local elapsed=$(( $(date +%s) - start_time ))
            local rate
            if [ "$elapsed" -gt 0 ]; then
                rate=$(( i / elapsed ))
            else
                rate="$i"
            fi
            info "Generated $i/$COUNT  (${rate} prog/s)"
        fi
    done

    local elapsed=$(( $(date +%s) - start_time ))
    local total_files
    total_files=$(find "$SUITE_DIR" -name '*.c' | wc -l)

    header "━━━ Generation complete ━━━"
    info "Programs generated : $success"
    info "Failures skipped   : $failed"
    info "Total .c files     : $total_files"
    info "Suite directory    : $SUITE_DIR"
    info "Time elapsed       : ${elapsed}s"
}

# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
# Pipeline run phase
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
run_pipeline() {
    local file_count
    file_count=$(find "$SUITE_DIR" -name '*.c' | wc -l)

    if [ "$file_count" -eq 0 ]; then
        fail "No .c files found in $SUITE_DIR"
    fi

    header "━━━ Running pipeline on $file_count programs ━━━"

    local pipeline_flags=()
    if [ "$VERBOSE" -eq 0 ]; then
        pipeline_flags+=(--quiet)
    fi

    # Export CC/CFLAGS so pipeline.sh picks up the csmith include path
    export CC="${CC:-gcc}"
    export CFLAGS="${CFLAGS:--O0 -w} -I${CSMITH_INCLUDE}"

    "$PIPELINE" "${pipeline_flags[@]}" "$SUITE_DIR"
}

# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
# Main
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
header "━━━ bench.sh — csmith benchmark runner ━━━"
info "Count   : $COUNT"
info "Suite   : $SUITE_DIR"
info "Verbose : $( [ "$VERBOSE" -eq 1 ] && echo 'yes' || echo 'no' )"
info "Generate: $( [ "$DO_GEN" -eq 1 ] && echo 'yes' || echo 'skip' )"
info "Run     : $( [ "$DO_RUN" -eq 1 ] && echo 'yes' || echo 'skip' )"
if [ -n "$SEED" ]; then
    info "Seed    : $SEED (to $(( SEED + COUNT - 1 )))"
fi
echo ""

if [ "$DO_GEN" -eq 1 ]; then
    generate_suite
    echo ""
fi

if [ "$DO_RUN" -eq 1 ]; then
    run_pipeline
fi

info "Done. Suite preserved at: $SUITE_DIR"
