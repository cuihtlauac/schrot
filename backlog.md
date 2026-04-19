# Geometric T-flip backlog

## Rounds 3–6 summary (done)

Sub-wall simplicity, geometric flip application, and integration of
M-M T-flips are complete.  `Geom.subwall_simplicity` rewritten with
edge-match criterion (Round 5); `Geom.apply_t_flip` / `enumerate_t_flips`
verified invertible at n≤12 in both equal-splits and generic-weight
modes (8.9M forward flips).  Round 6 derived a symbolic LCA-rewrite
rule from the geometric oracle that matches `apply_t_flip` exactly on
the 1632 guillotine-producing T-flips at n≤7.  See FLIP_INVERTIBILITY.md
for full history.

The 150 windmill-producing T-flips at n=7 (and smaller counts at n=5, 6)
are Merino-Mütze flips that exit the guillotine subspace; they are
expected per M-M Theorem 19 and excluded from the strong poset per
Asinowski Theorem 27.  They are not bugs.

`flip_check` verification at n=7: 2942/2942 invertible (was 1625/4209
failing pre-Round-5).

## Round 7 — Geometric Asinowski PoC

Before any further tree-layer work (Phases D/F/G), verify Asinowski's
pivoting flip structure geometrically in isolation.  This is the same
discipline that worked for M-M in Rounds 3–5.

**What is Asinowski pivoting, operationally?**  Geometrically identical
to M-M's T-flip rotation, but admissible only when the post-flip
rectangulation stays guillotine.  So it factors as `Geom.apply_t_flip`
(already correct) + a guillotine-admissibility predicate.

### Scope

1. **`Geom.is_asinowski_admissible : ?eps:float -> t -> t_joint -> side
   -> bool`**.  Composes `apply_t_flip` with a guillotine predicate
   based on `Geom.tree_of_rects`:
   - `Error _` (windmill) → false
   - `Ok [t']` with `is_generic` → true
   - `Ok [t']` non-generic (cross junction created) → false
   - anything else → false
   Additive; `apply_t_flip` and `subwall_simplicity` untouched.

2. **`bin/asinowski_flip_check.ml`** (mirrors `bin/geom_flip_check.ml`).
   Classify forward flips: `rejected_by_subwall` / `rejected_by_asinowski`
   / `admissible` / `apply_none`.  For each admissible flip, classify
   invertibility three-way: `invertible_in_asinowski` (reverse is also
   admissible) / `invertible_only_via_windmill` (reverse exists but
   crosses out of guillotine) / `genuine_failure`.  Theorem 27 predicts
   category 2 is empty; any hit there is a filter or theorem-reading bug.

3. **`scripts/run_asinowski_check.sh`**.  Copy of
   `run_long_flip_check.sh` with different binary/report names.

### Ground-truth cross-check (cheap, must pass)

At n=7 generic mode, `rejected_by_asinowski` must equal 150 exactly
and `admissible` must equal 1632 exactly (Round 6 oracle data).

### Dependencies

None.  `Geom.apply_t_flip`, `Geom.tree_of_rects`, `Geom.is_generic`,
`Geom.of_weighted`, `Geom.of_tiling` all exist.

### Performance envelope

Expected n=10 in minutes, n=12 possibly hours (tree_of_rects per flip
is 5–20× an apply_t_flip).  Benchmark at n=8 first.

### After Round 7

Phases D/F/G (see FLIP_INVERTIBILITY.md "What remains") become
tractable with a known admissibility predicate and a verified
geometric reference.

## Future directions

- **Layer 2 refinement**: command compiler (`neighbor` → flip type selection → apply) for the keyboard UX; distinguish single-cover pivots from multi-cover jumps.
- **Layer 3 implementation**: mutable split positions, incremental `Poset.of_geom` after single-segment updates. Entry point for Hyprland border-drag resize.
- Model checking with Schroder enumeration (fewer topologies than binary)
- Three-level verification: visual SVG -> model checking -> Rocq proof
- D4 orbit reduction of the paper's counting sequences (see COUNTING.md)
- Proof obligations for Rocq: tiebreaker correctness (poset.ml), lattice morphism of split/close (open question)
- **Operad-lattice bridge**: Aguiar-Livernet (2007) proves that operadic composition = weak order intervals for the *associative* operad (binary trees). Extending this to the Hipparchus operad would connect Layer 1 (split/close) to Layer 2 (quotientope lattice) algebraically. Related: Koszul duality of the Hipparchus operad (Loday-Vallette 2012) may give a formal dual relationship between split and close.
- **Double-category semantics**: Baez's speculative remark (Azimuth, Dec 2022) that guillotine partitions form a double category (2-cells = subdivided rectangles) could provide a compositional semantics for tiling operations across layers. Undeveloped.

### Branches

- `area-preserving` — Rational split ratios (`Q.t` per node) with area-preserving rewrite rules. Reverted from main because full area preservation is too strong. May be revisited with selective preservation.
