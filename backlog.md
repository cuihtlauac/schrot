# Geometric T-flip backlog

T-joint enumeration is done (`Geom.t_joints`, verified through n=8).
Tree reconstruction is done (`Geom.tree_of_rects`, verified through n=8).

## 1. Sub-wall simplicity check

At each T-joint, determine whether sub-wall w' (and w'') is a simple edge
(separates exactly 2 tiles, no other vertex on it). This is the T-flip
precondition from Merino-Mutze §2.2.

Approach: for each T-joint at P on through-wall w, walk from P toward the
wall boundary on each side. If no other T-joint lies on w between P and the
boundary, that sub-wall is simple. The `t_joints` list has all the vertices;
group by through-wall (same axis and coordinate), sort along the wall, check
gaps to boundary.

Output: for each T-joint, a pair `(bool * bool)` — whether w' and w'' are
flippable.

## 2. Geometric flip application

Given a T-joint with a simple sub-wall w' between bar tile B and stem tile A,
compute the new tile rectangles.

The operation (paper §2.2): "swap w' orientation so it merges with t." The
vertical segment between A and B becomes horizontal (or vice versa), extending
the terminating wall t.

Concretely: find the rectangular block formed by A and the adjacent portion of
B (bounded by P and the far end of w'). Re-partition that block with the
rotated wall. All other tiles unchanged.

Feed modified rects to `Geom.tree_of_rects` to recover the new Schroder tree.

Split ratio choice: preserve the area ratio between A and B's adjacent portion
(simplest invariant that gives exact invertibility).

## 3. Integration and verification

Wire geometric T-flips into `Tiling.enumerate_flips` alongside `wall_slide`
and `simple_dissolve`/`simple_create`. The geometric T-flips replace the
tree-based `pivot_out`/`pivot_in`.

Verification: `dune exec bin/flip_check.exe -- --max-leaves 7` — Property C
(invertibility) should pass at all n. Currently 1625/4209 failures at n=7.

## Future directions

- **Layer 2 refinement**: pivot_in insert mode (grow Frame, inverse of pivot_out from >=3-ary). Command compiler: `neighbor` -> flip type selection -> apply. Distinguish single-cover pivots from multi-cover jumps.
- **Layer 3 implementation**: mutable split positions, incremental `Poset.of_geom` after single-segment updates. Entry point for Hyprland border-drag resize.
- Model checking with Schroder enumeration (fewer topologies than binary)
- Three-level verification: visual SVG -> model checking -> Rocq proof
- D4 orbit reduction of the paper's counting sequences (see COUNTING.md)
- Proof obligations for Rocq: tiebreaker correctness (poset.ml), lattice morphism of split/close (open question)
- **Operad-lattice bridge**: Aguiar-Livernet (2007) proves that operadic composition = weak order intervals for the *associative* operad (binary trees). Extending this to the Hipparchus operad would connect Layer 1 (split/close) to Layer 2 (quotientope lattice) algebraically. Related: Koszul duality of the Hipparchus operad (Loday-Vallette 2012) may give a formal dual relationship between split and close.
- **Double-category semantics**: Baez's speculative remark (Azimuth, Dec 2022) that guillotine partitions form a double category (2-cells = subdivided rectangles) could provide a compositional semantics for tiling operations across layers. Undeveloped.

### Branches

- `area-preserving` — Rational split ratios (`Q.t` per node) with area-preserving rewrite rules. Reverted from main because full area preservation is too strong. May be revisited with selective preservation.
