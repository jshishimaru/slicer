#!/usr/bin/env bash
# pipeline.sh — End-to-end C reduction pipeline
#
# Usage:
#   ./pipeline.sh <dir>          Process all .c files in <dir>
#   ./pipeline.sh <file.c>       Process a single C file
#
# Output structure:
#   output/
#     <basename>/
#       output.c                 Reduced C source
#       stats.txt                Before/after statistics
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SLICER="$SCRIPT_DIR/_build/default/bin/slicer.exe"
CC="${CC:-gcc}"
CFLAGS="${CFLAGS:--O0 -w}"
RUN_TIMEOUT="${RUN_TIMEOUT:-3}"  # seconds; kills binaries that run too long (e.g. infinite loops)

# ── Colors ──
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m'

# ── Quiet mode (set by --quiet flag) ──
QUIET=0
MAX_COUNT=0  # 0 = process all files

info()  { [ "$QUIET" -eq 0 ] && echo -e "${GREEN}[pipeline]${NC} $*" || true; }
warn()  { echo -e "${YELLOW}[pipeline]${NC} $*"; }
fail()  { echo -e "${RED}[pipeline]${NC} $*"; exit 1; }
header(){ [ "$QUIET" -eq 0 ] && echo -e "${CYAN}${BOLD}$*${NC}" || true; }
# summary_info always prints regardless of quiet mode
summary_info() { echo -e "${GREEN}[pipeline]${NC} $*"; }
summary_header() { echo -e "${CYAN}${BOLD}$*${NC}"; }

# ── Helper: strip commas from perf numbers for arithmetic ──
strip_commas() { echo "$1" | tr -d ','; }

# ── Helper: compute percentage change (integer) ──
# pct_change <before> <after> → prints e.g. "-12" or "+5"
pct_change() {
    local before="$1" after="$2"
    if [ "$before" = "N/A" ] || [ "$after" = "N/A" ] || [ "$before" = "0" ]; then
        echo "N/A"
        return
    fi
    local b a
    b=$(strip_commas "$before")
    a=$(strip_commas "$after")
    if [ "$b" -eq 0 ]; then
        echo "N/A"
        return
    fi
    local diff=$(( a - b ))
    local pct=$(( diff * 100 / b ))
    if [ "$pct" -ge 0 ]; then
        echo "+${pct}%"
    else
        echo "${pct}%"
    fi
}

# ── Check prerequisites ──
if [ ! -f "$SLICER" ]; then
    fail "Slicer binary not found at $SLICER. Run 'opam exec -- dune build' first."
fi

# ───────────────────────────────────────────────────────
# process_file <input.c>
#   Runs the full pipeline on a single C file.
#   Creates output/<basename>/{output.c, stats.txt}
# ───────────────────────────────────────────────────────
process_file() {
    local INPUT="$1"
    local BASENAME
    BASENAME="$(basename "$INPUT" .c)"

    local FILE_OUT_DIR="$SCRIPT_DIR/output/$BASENAME"
    local FILE_TMP_DIR="$SCRIPT_DIR/tmp/$BASENAME"
    mkdir -p "$FILE_OUT_DIR" "$FILE_TMP_DIR"

    local PREPROCESSED="$FILE_TMP_DIR/preprocessed.c"
    local OUTPUT_C="$FILE_OUT_DIR/output.c"
    local SLICER_STATS="$FILE_TMP_DIR/slicer_stats.txt"
    local ORIG_BIN="$FILE_TMP_DIR/original_bin"
    local REDUCED_BIN="$FILE_TMP_DIR/reduced_bin"
    local ORIG_OUT="$FILE_TMP_DIR/original.out"
    local REDUCED_OUT="$FILE_TMP_DIR/reduced.out"
    local STATS_FILE="$FILE_OUT_DIR/stats.txt"
    local GCOV_DIR="$FILE_TMP_DIR/gcov"
    local GCOV_JSON="$FILE_TMP_DIR/gcov_data.json"

    header "━━━ Processing: $BASENAME ━━━"

    # ── Step 1: Preprocess ──
    # NOTE: Do NOT use -P here.  CIL needs the #line directives that gcc -E
    # emits so that each global retains its originating file path.  This is
    # required by remove_header_globals to distinguish user code from headers.
    # The slicer suppresses #line directives in its own output (lineDirectiveStyle := None).
    info "[$BASENAME] Step 1: Preprocessing"
    $CC -E $CFLAGS "$INPUT" -o "$PREPROCESSED"

    # ── Step 1.5: gcov profiling — compile with coverage, run, collect ──
    local GCOV_FLAG=""
    info "[$BASENAME] Step 1.5: gcov coverage profiling"
    mkdir -p "$GCOV_DIR"
    # Compile the preprocessed source with coverage instrumentation
    # Copy preprocessed.c into gcov dir so .gcno/.gcda are co-located
    cp "$PREPROCESSED" "$GCOV_DIR/preprocessed.c"
    if $CC --coverage -O0 -w "$GCOV_DIR/preprocessed.c" -o "$GCOV_DIR/preprocessed" 2>/dev/null; then
        # Run the instrumented binary (with timeout)
        set +e
        timeout "$RUN_TIMEOUT" "$GCOV_DIR/preprocessed" > /dev/null 2>&1
        local GCOV_RUN_EXIT=$?
        set -e

        if [ "$GCOV_RUN_EXIT" -ne 124 ]; then
            # Collect coverage data — gcov reads .gcno/.gcda from the compile dir
            # Use --json-format --stdout to get JSON output
            # Some gcov versions emit gzipped JSON, others emit plain JSON
            pushd "$GCOV_DIR" > /dev/null
            local GCOV_RAW
            GCOV_RAW="$(gcov --json-format --stdout preprocessed.c 2>/dev/null)" || true
            if [ -n "$GCOV_RAW" ]; then
                # Try gunzip first; if it fails, assume plain JSON
                if echo "$GCOV_RAW" | gunzip > "$GCOV_JSON" 2>/dev/null; then
                    : # successfully decompressed
                else
                    echo "$GCOV_RAW" > "$GCOV_JSON"
                fi
                # Verify the JSON file is non-empty and valid
                if [ -s "$GCOV_JSON" ]; then
                    GCOV_FLAG="--gcov-json $GCOV_JSON"
                    info "[$BASENAME]   ✓ gcov data collected"
                else
                    warn "[$BASENAME]   gcov produced empty JSON — skipping"
                    rm -f "$GCOV_JSON"
                fi
            else
                warn "[$BASENAME]   gcov JSON collection failed — skipping"
            fi
            popd > /dev/null
        else
            warn "[$BASENAME]   gcov binary timed out — skipping coverage"
        fi
    else
        warn "[$BASENAME]   gcov compilation failed — skipping coverage"
    fi

    # ── Step 2: CIL transform ──
    info "[$BASENAME] Step 2: CIL transformation"
    if [ "$QUIET" -eq 1 ]; then
        "$SLICER" "$PREPROCESSED" "$OUTPUT_C" "$SLICER_STATS" "$INPUT" $GCOV_FLAG 2>/dev/null
    else
        "$SLICER" "$PREPROCESSED" "$OUTPUT_C" "$SLICER_STATS" "$INPUT" $GCOV_FLAG
    fi

    # ── Step 3: Compile original ──
    info "[$BASENAME] Step 3: Compiling original"
    $CC $CFLAGS "$INPUT" -o "$ORIG_BIN"

    # ── Step 4: Compile reduced ──
    info "[$BASENAME] Step 4: Compiling reduced"
    local COMPILE_REDUCED_OK=1
    if ! $CC $CFLAGS "$OUTPUT_C" -o "$REDUCED_BIN" 2>/dev/null; then
        warn "[$BASENAME]   ✗ Reduced output failed to compile"
        COMPILE_REDUCED_OK=0
    fi

    # ── Step 5: Run both (with timeout to catch infinite loops) ──
    info "[$BASENAME] Step 5: Correctness check (timeout=${RUN_TIMEOUT}s)"
    set +e
    timeout "$RUN_TIMEOUT" "$ORIG_BIN" > "$ORIG_OUT" 2>&1
    local ORIG_EXIT=$?
    if [ "$COMPILE_REDUCED_OK" -eq 1 ]; then
        timeout "$RUN_TIMEOUT" "$REDUCED_BIN" > "$REDUCED_OUT" 2>&1
        local REDUCED_EXIT=$?
    else
        echo "" > "$REDUCED_OUT"
        local REDUCED_EXIT=999
    fi
    set -e

    local ORIG_STDOUT REDUCED_STDOUT
    ORIG_STDOUT="$(cat "$ORIG_OUT")"
    REDUCED_STDOUT="$(cat "$REDUCED_OUT")"

    # ── Correctness: check both stdout AND exit code ──
    local STDOUT_MATCH="PASS"
    local EXIT_MATCH="PASS"
    local CORRECT="PASS"
    local TIMED_OUT=0

    if [ "$COMPILE_REDUCED_OK" -eq 0 ]; then
        CORRECT="FAIL"
        STDOUT_MATCH="FAIL"
        EXIT_MATCH="FAIL"
        warn "[$BASENAME]   ✗ Reduced output did not compile — marking FAIL"
    # Exit code 124 = killed by timeout
    elif [ "$ORIG_EXIT" -eq 124 ] || [ "$REDUCED_EXIT" -eq 124 ]; then
        TIMED_OUT=1
        CORRECT="SKIP"
        STDOUT_MATCH="SKIP"
        EXIT_MATCH="SKIP"
        warn "[$BASENAME]   ⏱ Timed out after ${RUN_TIMEOUT}s — skipping correctness check"
    else
        if ! diff -q "$ORIG_OUT" "$REDUCED_OUT" > /dev/null 2>&1; then
            STDOUT_MATCH="FAIL"
            CORRECT="FAIL"
            warn "[$BASENAME]   ✗ Stdout mismatch!"
        fi
        if [ "$ORIG_EXIT" != "$REDUCED_EXIT" ]; then
            EXIT_MATCH="FAIL"
            CORRECT="FAIL"
            warn "[$BASENAME]   ✗ Exit code mismatch! (original=$ORIG_EXIT, reduced=$REDUCED_EXIT)"
        fi
        if [ "$CORRECT" = "PASS" ]; then
            info "[$BASENAME]   ✓ Stdout matches exactly"
            info "[$BASENAME]   ✓ Exit codes match ($ORIG_EXIT)"
        fi
    fi

    # ── Step 6: Collect size / LOC stats ──
    local ORIG_BYTES REDUCED_BYTES ORIG_LINES REDUCED_LINES
    ORIG_BYTES=$(wc -c < "$INPUT")
    REDUCED_BYTES=$(wc -c < "$OUTPUT_C")
    ORIG_LINES=$(wc -l < "$INPUT")
    REDUCED_LINES=$(wc -l < "$OUTPUT_C")

    # Semicolon LOC — count individual ';' characters in original input and final output
    local BEFORE_SLOC AFTER_SLOC
    BEFORE_SLOC=$(tr -cd ';' < "$INPUT" | wc -c)
    AFTER_SLOC=$(tr -cd ';' < "$OUTPUT_C" | wc -c)

    # Reduction percentages
    local SIZE_REDUCTION=0
    if [ "$ORIG_BYTES" -gt 0 ]; then
        SIZE_REDUCTION=$(( (ORIG_BYTES - REDUCED_BYTES) * 100 / ORIG_BYTES ))
    fi
    local SLOC_REDUCTION=0
    if [ "$BEFORE_SLOC" -gt 0 ]; then
        SLOC_REDUCTION=$(( (BEFORE_SLOC - AFTER_SLOC) * 100 / BEFORE_SLOC ))
    fi
    local LINES_REDUCTION=0
    if [ "$ORIG_LINES" -gt 0 ]; then
        LINES_REDUCTION=$(( (ORIG_LINES - REDUCED_LINES) * 100 / ORIG_LINES ))
    fi

    # ── Step 7: CPU cycles via perf ──
    local ORIG_CYCLES="N/A"
    local REDUCED_CYCLES="N/A"
    local CYCLES_CHANGE="N/A"
    if [ "$TIMED_OUT" -eq 1 ]; then
        warn "[$BASENAME]   Skipping perf — binary timed out"
    elif command -v perf &> /dev/null; then
        info "[$BASENAME] Step 6: Measuring CPU cycles"

        timeout $((RUN_TIMEOUT * 4)) perf stat -e cycles:u -r 3 "$ORIG_BIN" > /dev/null 2> "$FILE_TMP_DIR/perf_orig.txt" || true
        timeout $((RUN_TIMEOUT * 4)) perf stat -e cycles:u -r 3 "$REDUCED_BIN" > /dev/null 2> "$FILE_TMP_DIR/perf_reduced.txt" || true

        ORIG_CYCLES=$(grep 'cycles:u' "$FILE_TMP_DIR/perf_orig.txt" | awk '{print $1}' | head -1)
        REDUCED_CYCLES=$(grep 'cycles:u' "$FILE_TMP_DIR/perf_reduced.txt" | awk '{print $1}' | head -1)
        ORIG_CYCLES="${ORIG_CYCLES:-N/A}"
        REDUCED_CYCLES="${REDUCED_CYCLES:-N/A}"
        CYCLES_CHANGE=$(pct_change "$ORIG_CYCLES" "$REDUCED_CYCLES")
    else
        warn "[$BASENAME]   perf not available — skipping cycle measurement"
    fi

    # ── Write stats.txt ──
    {
        echo "═══════════════════════════════════════════"
        echo "  Stats for: $BASENAME"
        echo "═══════════════════════════════════════════"
        echo ""
        echo "source_file=$INPUT"
        echo ""
        echo "── Correctness ──"
        echo "correctness=$CORRECT"
        echo "stdout_match=$STDOUT_MATCH"
        echo "exit_code_match=$EXIT_MATCH"
        echo "exit_code_original=$ORIG_EXIT"
        echo "exit_code_reduced=$REDUCED_EXIT"
        echo ""
        echo "── Program Output (stdout) ──"
        echo "before_stdout<<EOF"
        echo "$ORIG_STDOUT"
        echo "EOF"
        echo "after_stdout<<EOF"
        echo "$REDUCED_STDOUT"
        echo "EOF"
        echo ""
        echo "── Size (bytes) ──"
        echo "before_bytes=$ORIG_BYTES"
        echo "after_bytes=$REDUCED_BYTES"
        echo "size_reduction_pct=${SIZE_REDUCTION}%"
        echo ""
        echo "── Lines (wc -l) ──"
        echo "before_lines=$ORIG_LINES"
        echo "after_lines=$REDUCED_LINES"
        echo "lines_reduction_pct=${LINES_REDUCTION}%"
        echo ""
        echo "── Semicolon LOC (raw file) ──"
        echo "before_semicolon_loc=$BEFORE_SLOC"
        echo "after_semicolon_loc=$AFTER_SLOC"
        echo "sloc_reduction_pct=${SLOC_REDUCTION}%"
        echo ""
        echo "── CPU Cycles (perf stat -e cycles:u -r 3) ──"
        echo "before_cycles=$ORIG_CYCLES"
        echo "after_cycles=$REDUCED_CYCLES"
        echo "cycles_change_pct=$CYCLES_CHANGE"
    } > "$STATS_FILE"

    # ── Print summary ──
    info "[$BASENAME] ── Summary ──"
    info "[$BASENAME]   Correctness:    $CORRECT (stdout=$STDOUT_MATCH, exit_code=$EXIT_MATCH)"
    info "[$BASENAME]   Stdout (orig):  $ORIG_STDOUT"
    info "[$BASENAME]   Stdout (red):   $REDUCED_STDOUT"
    info "[$BASENAME]   Size:           $ORIG_BYTES → $REDUCED_BYTES bytes (${SIZE_REDUCTION}% reduction)"
    info "[$BASENAME]   Lines:          $ORIG_LINES → $REDUCED_LINES (${LINES_REDUCTION}% reduction)"
    info "[$BASENAME]   Semicolon LOC:  $BEFORE_SLOC → $AFTER_SLOC (${SLOC_REDUCTION}% reduction)"
    info "[$BASENAME]   CPU cycles:     $ORIG_CYCLES → $REDUCED_CYCLES ($CYCLES_CHANGE)"
    info "[$BASENAME]   Output:         $OUTPUT_C"
    info "[$BASENAME]   Stats:          $STATS_FILE"
    echo ""
}

# ───────────────────────────────────────────────────────
# Main — handle file or directory argument
# ───────────────────────────────────────────────────────
# ── Parse flags ──
while [[ $# -gt 0 ]]; do
    case "$1" in
        --quiet|-q) QUIET=1; shift ;;
        -n|--count)
            MAX_COUNT="$2"
            if ! [[ "$MAX_COUNT" =~ ^[0-9]+$ ]] || [ "$MAX_COUNT" -lt 1 ]; then
                fail "--count requires a positive integer"
            fi
            shift 2 ;;
        -*) fail "Unknown flag: $1" ;;
        *) break ;;  # positional arg
    esac
done

TARGET="${1:?Usage: $0 [--quiet] [-n COUNT] <input.c | input_dir>}"
TARGET="$(realpath "$TARGET")"

if [ -d "$TARGET" ]; then
    # Directory mode: process all .c files
    C_FILES=()
    while IFS= read -r -d '' f; do
        C_FILES+=("$f")
    done < <(find "$TARGET" -maxdepth 1 -name '*.c' -type f -print0 | sort -z)

    if [ ${#C_FILES[@]} -eq 0 ]; then
        fail "No .c files found in $TARGET"
    fi

    # Apply -n limit if specified
    TOTAL_FILES=${#C_FILES[@]}
    if [ "$MAX_COUNT" -gt 0 ] && [ "$MAX_COUNT" -lt "$TOTAL_FILES" ]; then
        C_FILES=("${C_FILES[@]:0:$MAX_COUNT}")
        info "Limiting to first $MAX_COUNT of $TOTAL_FILES files (use -n to change)"
    fi
    FILE_COUNT=${#C_FILES[@]}

    header "══════════════════════════════════════════════"
    header "  Processing $FILE_COUNT file(s) from $TARGET"
    header "══════════════════════════════════════════════"
    echo ""

    PASS_COUNT=0
    FAIL_COUNT=0
    SKIP_COUNT=0
    PROCESSED=0
    START_TIME=$(date +%s)

    for f in "${C_FILES[@]}"; do
        PROCESSED=$((PROCESSED + 1))
        
        # Progress indicator (every file in quiet mode, every 10 in verbose)
        if [ "$QUIET" -eq 1 ] || [ $((PROCESSED % 10)) -eq 0 ] || [ "$PROCESSED" -eq "$FILE_COUNT" ]; then
            ELAPSED=$(($(date +%s) - START_TIME))
            if [ "$ELAPSED" -gt 0 ]; then
                RATE=$((PROCESSED / ELAPSED))
                ETA=$(( (FILE_COUNT - PROCESSED) / (RATE > 0 ? RATE : 1) ))
                summary_info "Progress: $PROCESSED/$FILE_COUNT  (${RATE} files/s, ETA ${ETA}s)"
            else
                summary_info "Progress: $PROCESSED/$FILE_COUNT"
            fi
        fi
        
        process_file "$f"
        BASENAME="$(basename "$f" .c)"
        RESULT=$(grep '^correctness=' "$SCRIPT_DIR/output/$BASENAME/stats.txt" | cut -d= -f2)
        if [ "$RESULT" = "PASS" ]; then
            PASS_COUNT=$((PASS_COUNT + 1))
        elif [ "$RESULT" = "SKIP" ]; then
            SKIP_COUNT=$((SKIP_COUNT + 1))
        else
            FAIL_COUNT=$((FAIL_COUNT + 1))
        fi
    done

    # ── Per-file table (skip in quiet mode) ──
    if [ "$QUIET" -eq 0 ]; then
    echo ""
    header "══════════════════════════════════════════════"
    header "  Per-File Results"
    header "══════════════════════════════════════════════"
    echo ""

    printf "${BOLD}%-18s %6s %8s %8s %6s %8s %8s %6s %12s %12s %7s${NC}\n" \
        "FILE" "PASS?" "B_BYTES" "A_BYTES" "SIZE%" "B_SLOC" "A_SLOC" "SLOC%" "B_CYCLES" "A_CYCLES" "CYC%"
    printf '%.0s─' {1..115}; echo ""

    for f in "${C_FILES[@]}"; do
        BASENAME="$(basename "$f" .c)"
        SF="$SCRIPT_DIR/output/$BASENAME/stats.txt"
        CR=$(grep '^correctness=' "$SF" | cut -d= -f2)
        BB=$(grep '^before_bytes=' "$SF" | cut -d= -f2)
        AB=$(grep '^after_bytes=' "$SF" | cut -d= -f2)
        SP=$(grep '^size_reduction_pct=' "$SF" | cut -d= -f2)
        BS=$(grep '^before_semicolon_loc=' "$SF" | cut -d= -f2)
        AS=$(grep '^after_semicolon_loc=' "$SF" | cut -d= -f2)
        SLP=$(grep '^sloc_reduction_pct=' "$SF" | cut -d= -f2)
        BC=$(grep '^before_cycles=' "$SF" | cut -d= -f2)
        AC=$(grep '^after_cycles=' "$SF" | cut -d= -f2)
        CP=$(grep '^cycles_change_pct=' "$SF" | cut -d= -f2)
        printf "%-18s %6s %8s %8s %6s %8s %8s %6s %12s %12s %7s\n" \
            "$BASENAME" "$CR" "$BB" "$AB" "$SP" "$BS" "$AS" "$SLP" "$BC" "$AC" "$CP"
    done
    echo ""
    fi  # end quiet guard for per-file table

    # ── Compute averaged stats (always printed) ──
    summary_header "══════════════════════════════════════════════"
    summary_header "  Overall Averaged Stats (${#C_FILES[@]} files)"
    summary_header "══════════════════════════════════════════════"
    echo ""

    TOT_BB=0; TOT_AB=0
    TOT_BS=0; TOT_AS=0
    TOT_BL=0; TOT_AL=0
    TOT_BC=0; TOT_AC=0; CYCLE_COUNT=0
    FILE_COUNT=${#C_FILES[@]}

    for f in "${C_FILES[@]}"; do
        BASENAME="$(basename "$f" .c)"
        SF="$SCRIPT_DIR/output/$BASENAME/stats.txt"

        BB=$(grep '^before_bytes=' "$SF" | cut -d= -f2)
        AB=$(grep '^after_bytes=' "$SF" | cut -d= -f2)
        BS=$(grep '^before_semicolon_loc=' "$SF" | cut -d= -f2)
        AS=$(grep '^after_semicolon_loc=' "$SF" | cut -d= -f2)
        BL=$(grep '^before_lines=' "$SF" | cut -d= -f2)
        AL=$(grep '^after_lines=' "$SF" | cut -d= -f2)
        BC=$(grep '^before_cycles=' "$SF" | cut -d= -f2)
        AC=$(grep '^after_cycles=' "$SF" | cut -d= -f2)

        TOT_BB=$((TOT_BB + BB))
        TOT_AB=$((TOT_AB + AB))
        TOT_BS=$((TOT_BS + BS))
        TOT_AS=$((TOT_AS + AS))
        TOT_BL=$((TOT_BL + BL))
        TOT_AL=$((TOT_AL + AL))

        if [ "$BC" != "N/A" ] && [ "$AC" != "N/A" ]; then
            BC_NUM=$(strip_commas "$BC")
            AC_NUM=$(strip_commas "$AC")
            TOT_BC=$((TOT_BC + BC_NUM))
            TOT_AC=$((TOT_AC + AC_NUM))
            CYCLE_COUNT=$((CYCLE_COUNT + 1))
        fi
    done

    AVG_BB=$((TOT_BB / FILE_COUNT))
    AVG_AB=$((TOT_AB / FILE_COUNT))
    AVG_BS=$((TOT_BS / FILE_COUNT))
    AVG_AS=$((TOT_AS / FILE_COUNT))
    AVG_BL=$((TOT_BL / FILE_COUNT))
    AVG_AL=$((TOT_AL / FILE_COUNT))

    # Averaged percentages (computed from totals, not avg of avgs)
    if [ "$TOT_BB" -gt 0 ]; then
        AVG_SIZE_PCT=$((  (TOT_BB - TOT_AB) * 100 / TOT_BB ))
    else
        AVG_SIZE_PCT=0
    fi
    if [ "$TOT_BS" -gt 0 ]; then
        AVG_SLOC_PCT=$((  (TOT_BS - TOT_AS) * 100 / TOT_BS ))
    else
        AVG_SLOC_PCT=0
    fi
    if [ "$TOT_BL" -gt 0 ]; then
        AVG_LINES_PCT=$(( (TOT_BL - TOT_AL) * 100 / TOT_BL ))
    else
        AVG_LINES_PCT=0
    fi

    if [ "$CYCLE_COUNT" -gt 0 ]; then
        AVG_BC=$((TOT_BC / CYCLE_COUNT))
        AVG_AC=$((TOT_AC / CYCLE_COUNT))
        if [ "$TOT_BC" -gt 0 ]; then
            AVG_CYC_DIFF=$(( (TOT_AC - TOT_BC) * 100 / TOT_BC ))
            if [ "$AVG_CYC_DIFF" -ge 0 ]; then
                AVG_CYC_PCT="+${AVG_CYC_DIFF}%"
            else
                AVG_CYC_PCT="${AVG_CYC_DIFF}%"
            fi
        else
            AVG_CYC_PCT="N/A"
        fi
    else
        AVG_BC="N/A"; AVG_AC="N/A"; AVG_CYC_PCT="N/A"
    fi

    summary_info "  Files processed:       $FILE_COUNT"
    summary_info "  Correctness:           $PASS_COUNT/$FILE_COUNT passed, $SKIP_COUNT skipped (timeout)"
    echo ""
    summary_info "  ── Avg Size (bytes) ──"
    summary_info "    Before:              $AVG_BB"
    summary_info "    After:               $AVG_AB"
    summary_info "    Reduction:           ${AVG_SIZE_PCT}%"
    echo ""
    summary_info "  ── Avg Lines (wc -l) ──"
    summary_info "    Before:              $AVG_BL"
    summary_info "    After:               $AVG_AL"
    summary_info "    Reduction:           ${AVG_LINES_PCT}%"
    echo ""
    summary_info "  ── Avg Semicolon LOC ──"
    summary_info "    Before:              $AVG_BS"
    summary_info "    After:               $AVG_AS"
    summary_info "    Reduction:           ${AVG_SLOC_PCT}%"
    echo ""
    summary_info "  ── Avg CPU Cycles ──"
    summary_info "    Before:              $AVG_BC"
    summary_info "    After:               $AVG_AC"
    summary_info "    Change:              $AVG_CYC_PCT"
    echo ""

    # ── Batch pass/fail ──
    summary_header "══════════════════════════════════════════════"
    summary_info "  Total: $FILE_COUNT  |  Passed: $PASS_COUNT  |  Failed: $FAIL_COUNT  |  Skipped: $SKIP_COUNT"
    summary_header "══════════════════════════════════════════════"
    echo ""

elif [ -f "$TARGET" ]; then
    # Single file mode
    process_file "$TARGET"
else
    fail "Not a file or directory: $TARGET"
fi

summary_info "═══ Pipeline complete ═══"
summary_info "  Output directory: $SCRIPT_DIR/output/"
