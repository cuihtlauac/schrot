#!/usr/bin/env bash
# Long-running Asinowski-admissibility invertibility check.
#
# Runs bin/asinowski_flip_check.exe at every n from a lower bound up
# to MAX_N, in both modes (equal splits + generic irrational weights),
# and writes a single Markdown report under reports/.  The report is
# self-contained so a future reader (including Claude in a later
# session) can diagnose any failure without re-running the check.
#
# Usage:
#   scripts/run_asinowski_check.sh [--max-n N] [--min-n N] [--max-failure-details K]
#
# Recommended invocation when running detached:
#
#   nohup scripts/run_asinowski_check.sh --max-n 10 > /dev/null 2>&1 &
#   disown
#   tail -f reports/asinowski_check_*.md   # follow progress
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
REPORT="$REPORT_DIR/asinowski_check_${TS}.md"

COMMIT=$(git -C "$REPO_ROOT" rev-parse HEAD 2>/dev/null || echo "unknown")
BRANCH=$(git -C "$REPO_ROOT" rev-parse --abbrev-ref HEAD 2>/dev/null || echo "unknown")
DIRTY=$(git -C "$REPO_ROOT" status --porcelain 2>/dev/null | head -1)
UNAME=$(uname -a)
OCAML_VERSION=$(opam exec -- ocaml -version 2>/dev/null || echo "unknown")

{
  echo "# Asinowski-pivoting invertibility long run"
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
  echo "- **Binary:** \`bin/asinowski_flip_check.exe\`"
  echo "- **Range:** n=$MIN_N..$MAX_N, both modes (equal, generic)"
  echo "- **Failure detail cap:** $MAX_FAIL_DETAILS per (n, mode)"
  echo
  cat <<EOF

## What this report tests

This run exercises Asinowski pivoting flips, geometrically.  For every
orbit representative at each n, enumerate every (T-joint, side) pair
and classify:

- **rejected_by_subwall** — \`Geom.subwall_simplicity\` rejects this side
- **rejected_by_asinowski** — M-M flip succeeds but post-geometry is
  not a strong (guillotine and generic) rectangulation.  These are the
  M-M T-flips that exit the strong poset per Theorem 27.  Expected.
- **admissible** — subwall-simple AND post is strong.  This is an
  Asinowski pivoting flip, geometrically.

For each admissible forward flip, classify the reverse:

- **invertible_in_asinowski** — a subwall-simple reverse in post
  restores the pre-rects.
- **invertible_only_via_windmill** — only a non-subwall-simple reverse
  matches.  Theorem 27 (+ Round 5's subwall fix) predicts zero hits
  here.  Any non-zero count is a bug.
- **genuine_failure** — no reverse matches at all.  Should also be
  zero.

## Ground-truth cross-check (generic mode, n<=7)

From Round 6 oracle (bin/tflip_oracle.exe):

| n | rejected_by_asinowski | admissible |
|---|-----------------------|------------|
| 5 | 1                     | 67         |
| 6 | 19                    | 333        |
| 7 | 150                   | 1632       |

Any deviation means the Asinowski filter (Geom.is_asinowski_admissible
via tree_of_rects + is_generic) diverges from the tflip_oracle path.
EOF
} > "$REPORT"

echo "Building bin/asinowski_flip_check.exe..."
opam exec -- dune build bin/asinowski_flip_check.exe

echo "Report: $REPORT"
echo "Progress: tail -f $REPORT"
echo

ANY_FAIL=0
FAIL_LIST=()

for mode_flag in "" "--generic"; do
  mode_label=$([[ -z "$mode_flag" ]] && echo "equal" || echo "generic")
  echo "--- mode=$mode_label n=$MIN_N..$MAX_N ---"
  set +e
  opam exec -- dune exec bin/asinowski_flip_check.exe -- \
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
    echo "**PASS** — all (n, mode) combinations: all admissible flips are invertible_in_asinowski, zero only-via-windmill, zero genuine failures."
  else
    echo "**FAIL** — one or more runs had failures:"
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
