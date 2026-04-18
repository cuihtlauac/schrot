#!/usr/bin/env bash
# Long-running invertibility check for bin/geom_flip_check.exe.
#
# Runs the checker at every n from a lower bound up to MAX_N, in both
# modes (equal splits + generic irrational weights), and writes a
# single Markdown report under reports/.  The report is self-contained
# so a future reader (including Claude in a later session) can
# diagnose any failure without re-running the check.
#
# Usage:
#   scripts/run_long_flip_check.sh [--max-n N] [--min-n N] [--max-failure-details K]
#
# Recommended invocation when running detached for hours:
#
#   nohup scripts/run_long_flip_check.sh --max-n 10 > /dev/null 2>&1 &
#   disown
#   tail -f reports/geom_flip_check_*.md   # follow progress
#
# Non-zero exit from any (n, mode) combination does not abort the
# run; a failing size just writes a FAIL section in the report and
# the run continues.  Final verdict is written at the end.

set -uo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"

MAX_N=10
MIN_N=2
MAX_FAIL_DETAILS=5

while [[ $# -gt 0 ]]; do
  case "$1" in
    --max-n) MAX_N="$2"; shift 2 ;;
    --min-n) MIN_N="$2"; shift 2 ;;
    --max-failure-details) MAX_FAIL_DETAILS="$2"; shift 2 ;;
    -h|--help)
      sed -n '2,20p' "$0"
      exit 0 ;;
    *) echo "unknown arg: $1" >&2; exit 2 ;;
  esac
done

REPORT_DIR="$REPO_ROOT/reports"
mkdir -p "$REPORT_DIR"

TS=$(date -u +%Y%m%dT%H%M%SZ)
REPORT="$REPORT_DIR/geom_flip_check_${TS}.md"

COMMIT=$(git -C "$REPO_ROOT" rev-parse HEAD 2>/dev/null || echo "unknown")
BRANCH=$(git -C "$REPO_ROOT" rev-parse --abbrev-ref HEAD 2>/dev/null || echo "unknown")
DIRTY=$(git -C "$REPO_ROOT" status --porcelain 2>/dev/null | head -1)
UNAME=$(uname -a)
OCAML_VERSION=$(opam exec -- ocaml -version 2>/dev/null || echo "unknown")

{
  echo "# Geometric flip invertibility long run"
  echo
  echo "- **Timestamp (UTC):** $TS"
  echo "- **Commit:** \`$COMMIT\` (branch: $BRANCH)"
  if [[ -n "$DIRTY" ]]; then
    echo "- **Working tree:** DIRTY (uncommitted changes present)"
  else
    echo "- **Working tree:** clean"
  fi
  echo "- **OCaml:** \`$OCAML_VERSION\`"
  echo "- **Host:** \`$UNAME\`"
  echo "- **Command:** \`$0 $*\`"
  echo "- **Binary:** \`bin/geom_flip_check.exe\`"
  echo "- **Range:** n=$MIN_N..$MAX_N, both modes (equal, generic)"
  echo "- **Failure detail cap:** $MAX_FAIL_DETAILS per (n, mode)"
  echo
  cat <<EOF

## What this report tests

This run exercises \`Geom.apply_t_flip\` + \`Geom.subwall_simplicity\` on
every orbit representative of every labeled Schroder tree up to n=$MAX_N.
The two bugs fixed by the preceding commit were:

1. FP-noise comparator in subwall_simplicity (wc-bit ordering overriding
   pos ordering in generic-weight mode).
2. Filter criterion ignoring stem/bar edge alignment, which both
   over-rejected valid flips (cross-junction wall fragmentation) and
   under-rejected invalid flips (stem mid-bar or past-bar).

The fix replaced the sort-and-extremes logic with an edge-match
criterion: stem.edge == bar.edge on the relevant side.

If any (n, mode) FAILs here, the failure detail blocks contain the
tiling string, pre/post rects, pre/post T-joint lists with filter
output, and the best bypass match.  That is enough to reproduce the
counterexample and diagnose whether it is a recurrence of one of the
known bug flavors or something new.
EOF
} > "$REPORT"

echo "Building bin/geom_flip_check.exe..."
opam exec -- dune build bin/geom_flip_check.exe

echo "Report: $REPORT"
echo "Progress: tail -f $REPORT"
echo

ANY_FAIL=0
FAIL_LIST=()

for mode_flag in "" "--generic"; do
  mode_label=$([[ -z "$mode_flag" ]] && echo "equal" || echo "generic")
  echo "--- mode=$mode_label n=$MIN_N..$MAX_N ---"
  # The binary loops n internally from 2 up to --max-leaves, appending
  # one section per n to the report file.  One invocation per mode.
  set +e
  opam exec -- dune exec bin/geom_flip_check.exe -- \
    --max-leaves "$MAX_N" $mode_flag \
    --max-failure-details "$MAX_FAIL_DETAILS" \
    --report "$REPORT"
  rc=$?
  set -e
  if (( rc != 0 )); then
    ANY_FAIL=1
    FAIL_LIST+=("mode=$mode_label")
  fi
done

{
  echo
  echo "## Verdict"
  echo
  if (( ANY_FAIL == 0 )); then
    echo "**PASS** — all (n, mode) combinations invertible."
  else
    echo "**FAIL** — one or more runs had non-invertible flips:"
    for f in "${FAIL_LIST[@]}"; do echo "- $f"; done
  fi
} >> "$REPORT"

echo
echo "Report written: $REPORT"
if (( ANY_FAIL == 0 )); then
  echo "Verdict: PASS"
  exit 0
else
  echo "Verdict: FAIL — see report for details"
  exit 1
fi
