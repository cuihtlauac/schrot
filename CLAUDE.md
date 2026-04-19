# Schrot

An exploration framework for principled tiling window management, targeting Hyprland. Schroder trees represent tiling topologies (1-to-1 correspondence with guillotine partitions). Three layers of algebraic structure govern operations: an operad (split/close between sizes), a quotientope (flips at fixed size), and a 2-dimensional lattice (geometry within a fixed tree). The keyboard-driven constraint demands that every operation has a well-defined, predictable effect.

## Dirty state

**Round 5 — M-M flip invertibility resolved.**  `Geom.subwall_simplicity` rewritten with an edge-match criterion.  `flip_check` Property C passes at all n tested (2942/2942 at n=7); `geom_flip_check` passes in both equal-splits and generic-weight modes up to n=12 (8.9M flips).  See FLIP_INVERTIBILITY.md Round 5.

**Round 6 — symbolic LCA-rewrite rule derived from M-M oracle.**  `bin/tflip_sym_check.ml` matches `Geom.apply_t_flip` exactly on all 1632 guillotine-producing T-flips at n≤7.  The 150 "anomalies" at n=7 (19 at n=6, 1 at n=5) are M-M T-flips whose post-flip geometry is a windmill — expected by M-M Theorem 19 (generic rectangulation lattice), excluded from Asinowski's strong poset by Theorem 27.  Not bugs.

**Key framing.**  `Geom.enumerate_t_flips` implements **Merino-Mütze §2.2** (generic).  Asinowski pivoting flips = M-M T-flip restricted to guillotine-preserving outputs (same rotation, stricter admissibility).  The Schroder-tree layer must apply the Asinowski restriction.  See THEORY.md Layer 2 for the three-flip-graph distinction (M-M generic / Asinowski strong / weak-guillotine Schroder-tree quotient).

**Round 7 is next — geometric Asinowski PoC.**  Before any further tree-layer work, add `Geom.is_asinowski_admissible` and `bin/asinowski_flip_check.ml` mirroring the Round 3–5 discipline.  Ground-truth cross-check: at n=7 generic mode, `rejected_by_asinowski` must equal 150, `admissible` = 1632.  Phases D/F/G (promote symbolic rule to `lib/tiling.ml`, remove pivot_*, restore T-flip in `enumerate_flips`) are deferred until Round 7 data lands.  See backlog.md Round 7.

**Verification:**
- `dune exec bin/flip_check.exe -- --max-leaves 7`  — tree + geom mixed, A/B/C/D all pass
- `dune exec bin/geom_flip_check.exe -- --max-leaves 7` / `--generic`  — M-M geom-only
- `dune exec bin/tflip_sym_check.exe -- --max-leaves 7`  — symbolic vs M-M oracle
- `scripts/run_long_flip_check.sh --max-n 10`  — stress check with self-contained report under `reports/`

## Build

```sh
dune build
dune exec bin/main.exe -- --output out.svg    # interactive, reads rules from stdin
dune exec bin/test_svg.exe -- --output svg       # generates svg/{policy}_{open_close,move}.svg
dune exec bin/tiling_test.exe -- --output svg    # Schroder tiling enumeration + open/close SVGs
dune exec bin/model_check.exe -- --policy dominance --max-leaves 6
dune exec bin/flip_test.exe -- --output svg      # Layer 2 flip operations SVG
dune exec bin/flip_check.exe -- --max-leaves 7   # verify flip properties A-D
dune exec bin/strong_check.exe -- --max-leaves 8 # strong guillotine counting + D4 orbits
dune exec bin/checker_test.exe                   # smoke tests for the Checker library
dune exec bin/equiv_check.exe -- --max-leaves 7  # verify equivalence predicates
dune exec bin/d4_geom_counterexamples.exe -- --output svg  # D4 adjacency counterexamples
dune exec bin/geom_flip_check.exe -- --max-leaves 7       # pure-geometric T-flip invertibility PoC
scripts/run_long_flip_check.sh --max-n 10                 # long-form stress check, writes reports/*.md
dune exec bin/web.exe                            # browser prototype at http://localhost:8080
```

All generated files (SVGs, etc.) go under `svg/` in the project, never in `/tmp`.

## Architecture

Core library: `lib/schrot.ml` (tree type), `lib/tiling.ml` (operations, flips, D4, junction resolution), `lib/geom.ml` (geometry, adjacency, T-joints, tree reconstruction), `lib/poset.ml` (2D lattice encoding), `lib/svg.ml` (rendering), `lib/checker.ml` (model checker). Binary layer (frozen, TODO migrate): `lib/term.ml`, `lib/rewrite.ml`, `lib/command.ml`, `lib/path.ml`, `lib/policy.ml`. See `.mli` files for APIs.

## Schroder tree model

The core representation is `int Schrot.tiling = bool * int Schrot.t`:
- `bool` = root orientation (true = H = top-to-bottom, false = V = left-to-right)
- `Frame` children alternate orientation with depth
- `Tile n` = leaf window with label n
- No same-type nesting: H(a, H(b, c)) is impossible; instead H(a, b, c) as a 3-ary frame
- Enumeration counts match large Schroder numbers: 1, 2, 6, 22, 90, 394, ...

### Split semantics
- Same orientation as parent frame: insert fresh tile next to target (frame grows by 1 child)
- Cross orientation: wrap target in new 2-ary sub-frame
- Root tile: create new root frame with split direction as orientation

### Close semantics
- Remove tile from parent frame
- Frame with 1 remaining child collapses (child promoted)
- Root collapse flips `is_h` (surviving child was at opposite orientation)

## Conventions

- Leaf numbering starts at 0.
- H = top-to-bottom (first child = top). V = left-to-right (first child = left).
- Never use polymorphic variants. Use regular variant types (e.g., `type dir = H | V`).
- Shorthand: h(0, v(1,2)) = H frame with tile 0 and a V sub-frame containing tiles 1, 2.

## Adjacency model

Two notions of adjacency:
- **Tabstop (potential)**: `tabstop_all_adjacencies` — all tile pairs sharing a tabstop on opposing sides. Maximal set, includes both diagonals at cross junctions.
- **Geometric (authoritative)**: `Geom.of_tiling` — pairs sharing a boundary of positive length. Excludes point contact (per Eppstein).

Geometric is a subset of tabstop. The difference is the unchosen diagonals at cross junctions. Adjacency topology depends on split ratios — the tree alone does not determine it. See adjacencies.md for poset encoding, COUNTING.md for strong guillotine counting, REPORT.md for D4 covariance analysis.

## Test suite

- Symmetry reduction: 2 orbit representatives (h(_v) mixed, h(_h) same-type) x tiles 0,1 x 4 directions = 16 cases covering all 96 by H<->V rotation, outer child swap, inner child swap.
- Color coding: dark blue (#1a3a6b) = target tile, light blue (#a8d0e6) = displaced tiles, white = unchanged.

## Design methodology

This project uses iterative visual verification driving algebraic rule refinement:
1. Implement or change rewriting rules
2. Generate SVG test pairs (before->after) with `test_svg.exe` or `tiling_test.exe`
3. Visually inspect with `eog` (keep the window open; eog auto-refreshes when files change on disk, so do not relaunch it)
4. Refine based on case-by-case feedback
5. Use symmetry to reduce review set

## Record-keeping

After any significant design discussion, implementation, or literature review:
- Update this file (CLAUDE.md) with architectural changes
- Update memory files with process context, references, and decisions
- The design process is itself an experiment the user wants to reproduce

## Theoretical framework

Three layers: (1) **Hipparchus operad** — split/close between sizes, `Tiling.split`/`Tiling.close`; (2) **quotientope** — 3 flip types (simple, pivot, wall slide) at fixed size, `enumerate_flips`; (3) **2D lattice** — segment sliding within a fixed tree, `Poset.of_geom`. The flip graph is the skeleton of a convex polytope; its orientation is a lattice. See THEORY.md for full treatment including Hyprland keybinding mapping and grow operation design.

## Companion docs

- FLIP_INVERTIBILITY.md — Property C diagnosis, fix history, recommended geometric approach
- COUNTING.md — Strong guillotine counting, D4 orbits, Burnside decomposition, OEIS sequences
- REPORT.md — D4 equivalence classes, canonical forms, adjacency analysis
- THEORY.md — Three-layer algebraic framework (operad, quotientope, 2D lattice), Hyprland mapping
- REFERENCES.md — Paper citations with PDF integrity checksums
- adjacencies.md — Adjacency poset architecture (2D lattice encoding, O(1) rank-based check)
- backlog.md — Geometric T-flip tasks, future directions, branches
