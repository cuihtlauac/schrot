# Flip invertibility — two rounds of fixes and lessons learned

Complete record of attempts to fix `pivot_out`/`pivot_in` invertibility (Property C), paper analysis, what worked, what didn't, and the recommended path forward.

## Problem

Commit `171cd50` fixed `pivot_in` to use "merge mode" per Merino-Mutze 2021 SS2.2 (inserting Tile n into an existing Frame boundary: `v(3, 2, 1)` instead of wrapping in a new sub-frame `v(3, h(2, 1))`). But `pivot_out` was not updated to reverse this, breaking Property C (invertibility) in `flip_check`. Original failure count: 1918/3335 at n=7.

## Paper analysis

### Merino-Mutze 2021 (arXiv:2103.09333v2)

**SS2.2 -- Three flip types** (p.6, Figure 3):
- **Wall slide**: swaps two neighboring vertices of types `|-`/`-|` or `T`/`_|_` along a wall.
- **Simple flip**: swaps orientation of a wall separating exactly two rectangles.
- **T-flip**: at vertex v belonging to 3 rectangles, wall w through v (halves w', w''), wall t ending at v. If w' or w'' is an edge (between exactly 2 rectangles), swap its orientation so it "merges with t."

**SS3.1 -- Jumps** (p.10): Generalize all three flip types via insertion point movement. A jump changes the insertion point for exactly one rectangle.

**Lemma 3** (p.10-11) -- classifies jump type by insertion point positions in I(P):
- **(a)** q_k and q_l consecutive on a common wall -> **wall slide**
- **(b)** q_k on last vertical wall, q_l first on next vertical wall (or symmetric horizontal) -> **simple flip**
- **(c)** q_k on vertical wall, q_l first insertion point on next vertical wall of P, or q_k on horizontal and q_l last on previous horizontal -> **T-flip**

**Critical constraint from Lemma 3(c)**: T-flips are **minimal jumps** -- there is no intermediate l with k < l < k+d such that c_l(P) is in C_n. In tree terms: the tile moves to the **immediately adjacent wall**, not across multiple walls.

**Lemma 27** (p.34, Figure 22) -- the workhorse proof connecting permutation insertion order to geometric flips. Two main cases based on whether r_{n+1} is left-fixed+top-fixed vs left-fixed+top-extended. Each produces either R = R' (no change), a simple flip (case a), or a T-flip (cases b, c).

### Asinowski et al. 2024 (arXiv:2402.01483v1)

**SS4.5, Figure 19** (p.25) -- the 5 flip types on strong rectangulations: left-bottom pivoting, right-top pivoting, simple flip, vertical wall slide, horizontal wall slide. These are cover relations of a lattice (Theorem 22). Each has a well-defined inverse **within the strong poset**. These are distinct from M-M §2.2 T-flips, which are invertible within the larger generic rectangulation lattice; the two coincide exactly when the M-M flip's output stays guillotine.  Geometrically, Asinowski's L-B and R-T pivoting flips *are* M-M's T-flip restricted to guillotine-preserving cases (same rotation, stricter admissibility).

**Proposition 9** (p.16) -- the strong poset P_s(R) is a planar 2-dimensional lattice. Implemented in the codebase as `Poset.of_geom`.

**Theorem 27** (p.38) -- guillotine (= strong) rectangulations are characterized by avoidance of two mesh patterns p₁, p₂ (the "windmill" configurations).  This is what makes the strong poset closed under Asinowski's flip types.

**SS5.3** (p.36) -- strong guillotine enumeration recurrence S(n,l,t,r,b).

## Round 1 -- Extend pivot_out, add wrap mode

### Changes made
1. `pivot_out` >=3-ary extraction: widened `len = 2` guard to `len >= 2` in `try_pivot_from_children`. The existing match arms already handled both collapse (2-ary) and shrink (>=3-ary) cases.
2. `pivot_out_root`: new function for root boundary extraction from >=3-ary root. Creates 2-ary root with `not is_h`, `Before`/`After` placement parameter.
3. `pivot_in_wrap`: new function wrapping child containing target m with Tile n in a new 2-ary sub-frame. Reverses 2-ary `pivot_out` extractions.
4. All three enumerated in `enumerate_flips` alongside existing operations. Deduplication by result string handles overlaps.

### Results
- Properties A, B, D: pass at all n (unchanged)
- Property C at n<=3: **fixed** (was failing from n=3)
- Property C at n=4: 10 failures (down from 20)
- Property C at n=7: 710/5015 (down from 1918/3335)

### What worked
- >=3-ary extraction correctly reverses merge/insert into >=3-ary Frames
- Root boundary extraction with both-side placement covers insert-at-root cases
- Wrap mode correctly reverses 2-ary extractions (the original pivot_out behavior)

### What didn't work
- The splice mechanism placed extracted tiles at the boundary of a NEW 2-element list, not adjacent to the shrunken Frame in the parent's existing child list. This changed global structure (wrapping in new root with flipped is_h) instead of making a local change.
- `pivot_in` insert mode allowed non-minimal jumps: inserting at the far boundary of a child Frame (across multiple walls).

## Round 2 -- Rewrite pivot_out, add near-boundary constraint

### Changes made
1. `try_pivot_from_children` rewritten: returns expanded child list (tile placed adjacent to shrunken Frame in parent). Eliminated the G'-wrapping pattern.
2. `go_tree` simplified: `Splice` variant removed, always returns `Tree`.
3. Root handler: uses `is_h` (same orientation) for multi-child expansion, `not is_h` only for singleton collapse.
4. `pivot_in_root`: inverse of `pivot_out_root` -- dissolves 2-ary root, inserts tile into surviving child at boundary.
5. `pivot_in` insert mode restricted to near-boundary only (Lemma 3(c) minimality): when n is before the Frame, only `first_is_m`; when n is after, only `last_is_m`.
6. `pivot_in_wrap` Frame-child guard added then removed (was preventing valid 2-ary reversal).

### Results
- Property C at n<=3: still passes
- Property C at n=4: 9 failures
- Property C at n=7: 1625/4209 (worse than round 1's 710/5015)
- The parent-inline fix resolved the cx4_merge/cx4_pout cases but introduced new failures in deeper nesting

### Why it got worse
The parent-inline approach is correct for >=3-ary extraction (tile stays as sibling of shrunken Frame) but breaks for 2-ary extraction (where the Frame collapses to a single child). The collapsed child loses its Frame wrapper, so reinsertion via wrap creates a DIFFERENT nesting structure than the original. Each fix at one nesting depth creates asymmetries at other depths.

## Key lessons

### The fundamental mismatch
The tree operations (`pivot_in`/`pivot_out`) are defined structurally (boundary children, Frame nesting, splice mechanics), while valid T-flips are defined geometrically (at vertices of the rectangulation). These don't correspond one-to-one:

- A single tree operation can cross multiple geometric walls (non-minimal jump per Lemma 3)
- The same geometric T-flip can require different tree operations depending on the nesting depth where the relevant vertex occurs
- 2-ary Frame collapse/creation interacts with orientation flipping (`not is_h`) in ways that break forward/reverse symmetry
- The `to_string` representation alternates orientation by depth, so the same tree structure prints differently depending on its position in the tree

### The near-boundary insight from Lemma 3
The most valuable theoretical insight: **valid T-flips only move tiles to the immediately adjacent wall**. In tree terms, this means:
- `pivot_in` insert should only insert at the near boundary (the side of the child Frame facing Tile n)
- `pivot_out` should only extract from a boundary position and place the tile adjacent to the Frame it came from

This insight correctly eliminated some invalid operations (far-boundary inserts), but couldn't resolve the structural mismatch at other nesting levels.

### The TDD approach works for diagnosis but not for convergence
The test-driven loop (add counterexample -> fix -> check -> iterate) was excellent for understanding the problem and for making incremental progress. But the incremental approach hit diminishing returns because each structural fix creates new asymmetries. The problem requires a different architecture, not more patches.

## Recommended path forward

Implement T-flips directly from the geometric representation (`Geom.t`), bypassing the tree-structural approach entirely:

1. Compute tile rectangles via `resolve_splits` (already exists)
2. Enumerate T-joints in the geometric layout (extend `degenerate_corners` or similar)
3. At each T-joint, identify the edge (w' or w'') and the wall (t) per SS2.2
4. Apply the rotation: swap the edge's orientation to merge with t
5. Reconstruct the Schroder tree from the modified geometry

This approach uses geometry as the authoritative definition of valid flips, matching the papers exactly. The existing `Geom.of_tiling`, `degenerate_corners`, and `tabstop_all_adjacencies` infrastructure provides the starting point. The tree reconstruction step is the main new work -- essentially inverting `resolve_splits`.

## Round 3 -- Pure-geometric PoC (`bin/geom_flip_check.ml`)

To isolate the geometric flip primitive from tree-reconstruction effects,
this PoC enumerates trees, lowers to rects via `Geom.of_tiling`, applies
every `(t_joint, side)` with a simple sub-wall (Merino-Mütze Lemma 3
minimality, via `Geom.subwall_simplicity`), then searches the post-flip
geometry for any reverse T-flip whose `apply_t_flip` output
coordinate-matches the original rects (`eps = 1e-7`).  No tree
reconstruction, no tree-level flips.

### Results

| n | D4 orbits | flips | invertible | failure rate |
|---|-----------|-------|------------|--------------|
| 3 | 2   | 2    | 2/2       | 0%   |
| 4 | 6   | 14   | 14/14     | 0%   |
| 5 | 18  | 68   | 68/68     | 0%   |
| 6 | 68  | 352  | 349/352   | 0.9% |
| 7 | 270 | 1778 | 1746/1778 | 1.8% |

For comparison: the tree-level `flip_check` reports ~38.6% Property C
failures at n=7.  The pure-geometric formulation is ~20x better but
still not fully invertible.

### What the remaining failures tell us

Smallest counterexamples at n=6 (from `geom_flip_check.exe --max-leaves 6`):

- `h(v(5, h(4, 3)), v(h(2, 1), 0))` -- joint (0.667, 0.500), bar=3, stems=(2,0), Hi
- `h(v(5, 4), 3, v(2, h(1, 0)))` -- joint (0.500, 0.333), bar=3, stems=(5,4), Lo *and* Hi

In each case the forward flip returns `Some post_rects`, but the search
over `subwall_simplicity`-filtered reverse candidates finds none whose
`apply_t_flip` output coordinate-matches the original rects.  The most
likely mechanisms (not yet pinned down with an SVG inspection):

1. **Equal-splits placement leaks.**  `Geom.of_tiling` uses
   `resolve_splits` (equal splits by default).  A forward flip moves one
   wall; the resulting geometry no longer has equal splits for the
   affected tiles.  The "correct" reverse flip geometrically exists but
   at a coordinate that `resolve_splits` would never have produced --
   the minimality filter, keyed off absolute coordinates, rejects it.

2. **Sub-wall simplicity is too strict after a flip.**  A forward flip
   can turn a simple sub-wall into a compound one (by adding a new
   T-joint on the same wall), so the reverse candidate is filtered out
   even though it's the geometric inverse.

3. **Cross-junction traps.**  The filter explicitly excludes 4-way
   vertices.  A T-flip can create a cross junction; the reverse flip
   would have to start from that cross, which `t_joints` won't
   enumerate.

None of these are bugs in `apply_t_flip` itself -- that function is
coordinate-exact.  They are gaps in the *enumeration* of valid reverse
candidates.

### What this means for the architecture

The PoC confirms the half-truth in the Round 2 conclusion: geometric
flips are *closer* to invertible than tree flips, but "pure geometry" is
not automatic.  A production implementation needs either:

- A **principled reverse enumerator** that does not rely on the
  simple-sub-wall filter holding symmetrically across the flip.  For
  each forward flip, derive the reverse joint analytically (the same
  three tiles, swapped bar/stem assignment) rather than re-enumerating
  T-joints in the post-flip geometry.
- Or, accept the `resolve_splits`-induced gap and work with
  **arbitrary-ratio splits** from the start (`of_weighted`), so that
  the flipped geometry is still in the same coordinate regime as the
  original.

Either way, the ~1.8% failure rate at n=7 is a concrete measurement of
the remaining obstruction, not a theoretical unknown.

### Verification

```sh
opam exec -- dune exec bin/geom_flip_check.exe -- --max-leaves 7
```

## Round 4 -- Diagnosis: codebase bug, not paper theorem

The Round 3 PoC was extended with three instrumentation axes (genericity
tracking, filter-bypass reverse search, an `--generic` flag that uses
irrational weights via `Geom.of_weighted`) to decide whether the residual
failures indict the paper (Merino-Mütze Theorem 19) or the codebase.
Procedure in the plan at
`/home/cuihtlauac/.claude/plans/dynamic-sparking-platypus.md`.

### Measurements at n=7

| Mode           | flips | failures | non-generic post | rescued by bypass | genuine |
|----------------|-------|----------|------------------|-------------------|---------|
| equal splits   | 1778  | 32       | 24               | 32                | 0       |
| generic weights| 1781  | 67       | 0                | 59                | 8       |

"Genuine" = no T-flip in the post-flip geometry restores the original
rects, even with the simplicity filter bypassed.  The fact that *every*
failure in equal-splits mode is rescued by bypass immediately rules out
H1 (paper theorem false) for that mode: the valid inverse T-flip exists,
`Geom.subwall_simplicity` just rejects it.  The generic run surfaces a
second, distinct bug.

### The two bugs in `Geom.subwall_simplicity` (lib/geom.ml:192-219)

**Bug 1 -- FP noise breaks the grouping sort.**  The comparator at
line 199-203 sorts by `(through_h, wc, pos)` with raw `compare` on
floating-point `wc`.  Two T-joints at the same wall have `wc` values
computed from different rect corners; with irrational weights these can
differ in the last bit.  Example from a traced n=7 counterexample:
- joint at (0.525, y=0.90800416805774942) — jy computed via tile 5's bottom-right corner.
- joint at (0.783, y=0.90800416805774931) — jy computed via tile 3's bottom-right corner.

Both satisfy `abs_float (c - c') < eps` in the grouping step
(line 209) so they land in the same group — but the sort has already
placed them in `c`-ascending order (0.931 before 0.942), not in
`p`-ascending order (0.525 before 0.783).  The `List.mapi` tagging then
marks the extremes of the *c*-sorted order as simple, inverting
lo_simple/hi_simple for this group:

```
joint(0.525, 0.90800...942) ... lo_simple=false hi_simple=true   <-- should be (true, false)
joint(0.783, 0.90800...931) ... lo_simple=true  hi_simple=false  <-- should be (false, true)
```

Consequence in generic mode: `(0.525, Hi)` is wrongly filter-passed.
`apply_t_flip` then grows a stem at what is actually a middle sub-wall
position, producing geometrically invalid post-flip rects (tile 3
overlaps tile 0 from x=0.525 to x=0.742; the region x∈[0.783, 1],
y∈[0.908, 1] has no tile).  No valid reverse exists from broken
geometry, so bypass also fails → the 8 "genuine" failures at n=7.

**Bug 2 -- Walls are not fragmented at cross junctions.**  In equal-splits
mode a forward T-flip can create a cross junction that splits the
through-wall into two separate walls, but the grouping keeps both
T-joints in the same group because they share `wc` exactly.  The filter
then wrongly rejects the sub-wall pointing through the cross junction
as non-simple.  Result: 32 filter-blocked valid reverses at n=7,
all rescued by bypass.

### Decision

**H1 (paper theorem false) is rejected.**  In equal splits every failure
has a valid T-flip inverse that `Geom.subwall_simplicity` rejects.  In
generic weights, the filter's FP-sort bug causes `apply_t_flip` to be
applied at configurations where it cannot produce valid geometry; none
of these "failures" involve a valid forward T-flip, so they do not
refute the theorem either.

**H2 (codebase bug) is confirmed, and the bug is localized to
`subwall_simplicity` (lib/geom.ml:192-219).**  Two distinct defects,
both in the grouping/tagging logic:

1. sort by `wc` with raw `compare` violates the within-group sort-by-`pos`
   invariant mapi relies on;
2. grouping ignores cross junctions that fragment the wall.

`Geom.apply_t_flip` itself is coordinate-exact when called on valid
(joint, side) pairs; `Geom.t_joints` correctly enumerates 3-way vertices
and correctly excludes 4-way vertices.

### Suggested fixes (out of scope for this diagnostic)

- Bug 1: sort by `(through_h, bucketed_wc, pos)` where `bucketed_wc` is
  `wc` snapped to a canonical value per eps-bucket, e.g. the first `wc`
  seen when scanning a pre-sorted list.  Or: after grouping, re-sort
  each group by `pos` before tagging.
- Bug 2: extend `interior_vertices` to return 4-way vertices too, and
  in `subwall_simplicity` split a group at any cross junction whose
  position falls between two consecutive T-joints' positions.

### Verification

```sh
opam exec -- dune exec bin/geom_flip_check.exe -- --max-leaves 7           # equal splits
opam exec -- dune exec bin/geom_flip_check.exe -- --max-leaves 7 --generic # generic weights
```

Per-n lines report `invertible / total` and, on failure, `N
non-generic-post, M rescued-by-bypass, K genuine`.  The decision above
is the decisive empirical result.

## Round 5 -- Fix applied and verified

The diagnosis in Round 4 pointed to two defects in
`Geom.subwall_simplicity`.  The implemented fix replaces the original
sort-and-extremes grouping with a direct **edge-match** criterion:

> A T-flip at (j, side) is valid iff the chosen stem's far edge
> coincides with the bar's corresponding far edge on that side.

In code (lib/geom.ml:192-219, post-fix):

```ocaml
let edges_match j side =
  let stem_id = match side with
    | Lo -> fst j.stem_tiles
    | Hi -> snd j.stem_tiles in
  let stem = List.assoc stem_id g.rects in
  let bar = List.assoc j.bar_tile g.rects in
  let stem_edge, bar_edge =
    match side with
    | Lo -> if j.through_h then (stem.x, bar.x) else (stem.y, bar.y)
    | Hi -> if j.through_h then (stem.x +. stem.w, bar.x +. bar.w)
            else (stem.y +. stem.h, bar.y +. bar.h)
  in
  abs_float (stem_edge -. bar_edge) < eps
```

Why this subsumes both original defects:

- **FP-noise sort (Bug 1)**: no sort happens.  Each T-joint is
  evaluated in isolation.
- **Cross-junction wall fragmentation (Bug 2)**: the stem's rectangle
  already reflects whatever walls end its extent (cross junctions,
  other T-joints, boundary).  If a cross junction terminates the wall
  before the bar's edge, the stem's edge matches the cross junction's
  position, and the bar's edge differs — correctly rejected.  If the
  cross junction is beyond the bar, stem and bar agree — correctly
  accepted.
- **Stem-past-bar / stem-mid-bar flips**: exactly the case the
  edge-match detects.  If stem extends past bar or sits in the middle
  of bar, stem's far edge does not equal bar's far edge, so the flip
  is rejected.

### Verification

At n <= 7 (both modes, immediately after fix):

```
$ dune exec bin/geom_flip_check.exe -- --max-leaves 7
  n=7: 1782/1782 invertible
$ dune exec bin/geom_flip_check.exe -- --max-leaves 7 --generic
  n=7: 1782/1782 invertible
$ dune exec bin/flip_check.exe -- --max-leaves 7
  n=7: 2942/2942 invertible   (Properties A, B, C, D all pass)
```

`flip_check`'s Property C — previously failing at every n >= 3 since
the project's inception, with rates like 1625/4209 at n=7 — now
reports OK across the board.  The tree-level `pivot_out`/`pivot_in`
path shares `Geom.subwall_simplicity` via `Geom.enumerate_t_flips`
(lib/geom.ml:490, 526), so the fix percolates to that test too.

### Stress test at higher n

The PoC was extended with a `--report PATH` flag and a wrapper script
`scripts/run_long_flip_check.sh` that drives both modes up to
`--max-n`.  Empirical timings on one reference machine:

| n  | mode    | flips     | seconds |
|----|---------|-----------|---------|
| 8  | generic | 9532      | 0.09    |
| 9  | generic | 51574     | 0.55    |
| 10 | generic | 285230    | 3.41    |
| 11 | generic | 1590024   | 22.28   |
| 12 | generic | 8933570   | 142.77  |

All PASS up to n=12 (8.9M forward flips, all invertible).  Runtime is
low enough that a full n=10 run completes in seconds; the long-run
scaffolding exists to surface failures at scale rather than to
budget hours.

### Reusing the report for later sessions

Report files under `reports/geom_flip_check_<timestamp>.md` are
`.gitignore`d and self-contained (commit sha, OCaml version, host,
per-(n,mode) summary table, per-failure detail including pre/post
rects, T-joint filter output, and best bypass match).  A future
session inspecting only the report file can:

- Confirm which commit produced it.
- See which (n, mode) passed or failed.
- Reproduce any counterexample by dropping its tiling string into
  `bin/flip_unit.ml` without needing to re-run the check.

### Residual dirty state

None from the geometric-flip side.  Remaining CLAUDE.md items are
outside Round 5's scope.

## Round 6 -- Symbolic tree-level T-flip derived from geometric oracle

Goal: replace the obsolete `pivot_*` functions with a single
principled `Tiling.t_flip : stem:int -> bar:int -> side -> t -> t option`
whose semantics are defined to match `Geom.apply_t_flip` + tree
reconstruction.

### Approach

Oracle-driven design exploration:

1. **Oracle** (`bin/tflip_oracle.ml`): for each orbit representative
   at n<=6, lower to generic geometry, enumerate every valid T-flip,
   reconstruct the post-flip tree via `Geom.tree_of_rects`.
   Output is a Markdown catalog with structural features
   (stem/bar paths, LCA, parent orientations and arities) and a
   cluster summary.
2. **Pattern classification**: 30 clusters at n<=6, grouped by
   structural feature tuple `(bar_parent_arity, stem_parent_arity,
   same_parent_dir, lca_is_bar_parent, root_flip)`.  Inspection of
   the biggest clusters suggested a unified rule.
3. **Candidate rule** -- the LCA of (stem, bar) in `t` must have
   bar and stem_branch as adjacent siblings.  The flip rewrites
   the LCA:
   - flip LCA's direction;
   - promoted subtree = first child (Lo flip) or last child (Hi
     flip) of stem_branch.  Stem must be a descendant of promoted;
   - rest_of_stem_branch = stem_branch with the promoted child
     removed (collapsed to single child if arity 2);
   - new LCA has promoted on one side and an original-direction
     frame of (bar, rest_of_stem_branch) on the other, ordered so
     bar preserves its relative Lo/Hi position.
4. **Equivalence checker** (`bin/tflip_sym_check.ml`) runs the
   candidate vs. the oracle on every valid T-flip.

Key implementation subtlety: substitution of the new LCA into the
parent tree must collapse same-orientation nestings at every level
up, not just at the leaf.  `rebuild_through` walks the path from
LCA to root, invoking `build_frame` (direction-aware, collapsing
same-orient children) at each level.

### Results

At n<=7, the symbolic implementation matches the geometric oracle
on **every** guillotine-producing T-flip:

| n | total | match | sym=None | mismatch | anomalies (non-guillotine) |
|---|-------|-------|----------|----------|----------------------------|
| 3 | 2     | 2     | 0        | 0        | 0                          |
| 4 | 14    | 14    | 0        | 0        | 0                          |
| 5 | 68    | 67    | 0        | 0        | 1                          |
| 6 | 352   | 333   | 0        | 0        | 19                         |
| 7 | 1782  | 1632  | 0        | 0        | 150                        |

The **150 cases at n=7 (and 19 at n=6, 1 at n=5) are not anomalies in
the sense of bugs.**  They are M-M T-flips (Theorem 19, generic
rectangulations) whose post-flip geometry is a windmill.  Such flips
are valid cover relations of the *generic* flip graph per M-M but are
excluded from the *strong* poset per Asinowski Theorem 27 — and
therefore have no representative in the Schroder-tree (guillotine)
layer.  Any symbolic tree-level flip must filter them out.

**The filter is Asinowski's admissibility criterion.**  Geometrically,
a post-flip rectangulation is guillotine iff `Geom.tree_of_geom` returns
`Ok [single generic tree]`.  That predicate *is* the restriction
"Asinowski pivoting = M-M T-flip ∩ guillotine."  No other test is
needed; the three options listed in earlier drafts (structural tree
test, round-trip, pre-filtering) all reduce to applying this predicate
at different points in the pipeline.

### What remains (Phase D / F / G of the exploration plan)

- **Round 7 — geometric Asinowski PoC** (the next step, before any
  tree-layer work).  Add `Geom.is_asinowski_admissible : t -> t_joint
  -> side -> bool` composing `apply_t_flip` with the guillotine
  predicate above, and mirror `bin/geom_flip_check.ml` as
  `bin/asinowski_flip_check.ml` to verify invertibility of the
  filtered flip set.  Per Theorem 27, every filtered forward flip
  should have a filtered reverse; any flip whose only reverse is
  M-M-but-windmill-producing is a theorem-reading or filter bug and
  must be logged.  Ground-truth sanity check: at n=7 generic mode the
  PoC's `rejected_by_asinowski` count must equal 150 and its
  `admissible` count must equal 1632 (Round 6 oracle data).  See
  backlog.md Round 7 for detailed scope.
- **Phase D — promote to `lib/tiling.ml`**: move the Round 6 LCA-rewrite
  rule from `bin/tflip_sym_check.ml` into a `Tiling.t_flip` function.
  Requires a tree-level admissibility test — either round-trip via
  `Geom.of_tiling` + `Geom.tree_of_geom` (correct, likely slow) or a
  separable-permutation mesh-pattern test (cheaper, if Round 7's
  timing shows the round-trip is prohibitive).  Deferred until
  Round 7 data.
- **Phase F — remove pivot_***: delete `pivot_in`, `pivot_out`,
  `pivot_out_root`, `pivot_in_root`, `pivot_in_wrap` and migrate
  `bin/flip_unit.ml`, `bin/cheatsheet.ml`, `bin/web.ml`.
- **Phase G — restore T-flip in `Tiling.enumerate_flips`** using the
  symbolic op composed with the admissibility filter, replacing the
  geometric arm currently supplied by `bin/flip_check.ml`.

### Verification

```sh
opam exec -- dune exec bin/tflip_oracle.exe -- --max-leaves 6 --report reports/oracle.md
opam exec -- dune exec bin/tflip_sym_check.exe -- --max-leaves 7
```

The oracle report is the ground-truth dataset; the checker confirms
zero mismatches on guillotine-producing flips.  The 150 windmill-
producing flips at n=7 are expected (M-M Theorem 19) and are tracked
as a named class in the oracle, not bundled as errors.

## Round 8 — Phases D / F / G shipped

Geom.t_flip is the production tree-level Asinowski pivoting flip.
The legacy pivot_* tree-heuristic family has been removed; all
callers migrated.  Geom.enumerate_flips is the unified entry point
for all flip families.

**Phase D** (commit f371d53) — Tiling.apply_t_flip_symbolic (Round 6
LCA rewrite, exposed), Geom.t_flip (symbolic composed with
is_asinowski_admissible).  bin/t_flip_equiv_check.ml verifies
Geom.t_flip matches the M-M oracle on every (stem, bar) pair:
n=7: 1632 Some match, 9708 both_none (including 150 windmill cases
where sym returns None via filter), zero divergences.

**Phase F** (commit db9b0e9) — Deleted pivot_out / pivot_out_root /
pivot_in / pivot_in_root / pivot_in_wrap (~378 LOC), Pivot_out /
Pivot_in variants, and count_flip_sites placeholder.  Migrated
bin/cheatsheet.ml and bin/web.ml to Geom.t_flip; dropped the
web.ml 'x' (single-tile extraction) keybinding — 'e' + arrow is
the unified T-flip gesture.  bin/flip_unit.ml replaced pivot_*
regression tests with T-flip versions.  bin/s_equiv.ml pattern
match trimmed.

**Phase G** (this commit) — Geom.enumerate_flips merges
Tiling.enumerate_flips + enumerate_t_flips with dedup.
bin/flip_check.ml's all_flips is a thin alias; all_flips_with_rects
kept for Property C's cross-junction reverse search.

flip_check at n=7: A/B/C/D all pass (2942/2942).

## Test infrastructure

- `bin/flip_unit.ml`: focused per-flip-type regression tests.
- `bin/flip_check.ml`: exhaustive model checker for Properties A-D
  using Geom.enumerate_flips.  D4 orbit reduction.
- `bin/flip_test.ml`: collects counterexamples, generates SVG.
- `bin/geom_flip_check.ml`: pure-geometric M-M invertibility PoC
  (Rounds 3-5).
- `bin/asinowski_flip_check.ml`: pure-geometric Asinowski-pivoting
  invertibility PoC (Round 7).
- `bin/t_flip_equiv_check.ml`: Geom.t_flip vs M-M oracle equivalence
  (Round 8 / Phase D).
- `bin/tflip_oracle.ml`, `bin/tflip_sym_check.ml`: Round 6 oracle
  and symbolic comparator; retained for historical reproducibility.

**Testing workflow**: (1) add smallest failing case from `flip_check`
as unit test, (2) implement fix, (3) run `flip_unit.exe`, (4) run
`flip_check --max-leaves 4` (fast), (5) run `--max-leaves 7`
(thorough).
