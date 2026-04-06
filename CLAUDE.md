# Schrot

An exploration framework for principled tiling window management, targeting Hyprland. The goal is to work out the mathematics of tiling operations — split, close, move, resize — using simplified geometry (SVG rendering) and a language where data structure invariants are easy to express (OCaml), without the complexities of a real compositor. The resulting algebra and algorithms are intended to be ported to Hyprland's C++ once the theory is solid.

A core design constraint is that the tiling manager must be **keyboard-driven**: every operation — open, close, move, swap, resize, navigate — must be expressible as a discrete command bound to a key, without requiring mouse interaction. This rules out continuous-gesture operations and demands that each command has a well-defined, predictable effect on the tiling. The three-layer algebraic framework (operad, quotientope, 2-dimensional lattice) is evaluated against this constraint: Layers 1 and 2 (split, close, flips) are inherently discrete. Layer 3 (resize) requires discretization — e.g., fixed-step resize increments or ratio presets — to remain keyboard-accessible.

Schroder trees represent tiling topologies (1-to-1 correspondence with guillotine partitions). Three layers of algebraic structure govern operations: an operad (split/close between sizes), a quotientope (flips at fixed size), and a 2-dimensional lattice (geometry within a fixed tree). The project is heading toward formally verified configurable policies (visual SVG → model checking → Rocq proof).

## Build

```sh
dune build
dune exec bin/main.exe -- --output out.svg    # interactive, reads rules from stdin
dune exec bin/test_svg.exe -- --output svg       # generates svg/{policy}_{open_close,move}.svg
dune exec bin/tiling_test.exe -- --output svg    # Schroder tiling enumeration + open/close SVGs
dune exec bin/model_check.exe -- --policy dominance --max-leaves 6
dune exec bin/flip_test.exe -- --output svg      # Layer 2 flip operations SVG
dune exec bin/flip_check.exe -- --max-leaves 7   # verify flip properties A-D
dune exec bin/web.exe                            # browser prototype at http://localhost:8080
```

All generated files (SVGs, etc.) go under `svg/` in the project, never in `/tmp`.


## Architecture

### Schroder tiling layer

- `lib/list2.ml` — `List2.t = Cons2 of 'a * 'a * 'a list`. Lists with >= 2 elements, mirroring Stdlib.List API.
- `lib/schrot.ml` — `Schrot.t = Tile of 'a | Frame of 'a t List2.t`. Schroder trees where internal nodes have >= 2 children. `tiling = bool * 'a t` (bool = root is H). Provides `fold`, `unfold`, `map`, `enum`.
- `lib/tiling.ml` — Tiling operations on `int Schrot.tiling`. `dir = H | V`. Layer 1: `split`, `close`, `neighbor` (tree-based navigation). Layer 2: `simple_dissolve`, `simple_create`, `pivot_out`, `pivot_in`, `wall_slide` (the 3 atomic flip types generating the quotientope flip graph); `enumerate_flips` (all applicable flips); `flip_to_string`. Junction resolution via relaxation toward evenly spaced targets (`resolve_splits`). Tabstop extraction and potential adjacency (`tabstop_all_adjacencies`). `cut_depth` (depth of the separating cut between two tiles). D4/V4 symmetry actions and canonical forms. Degenerate vertex detection (`degenerate_corners`, `degenerate_cuts`). Graph utilities (`graphs_isomorphic`, `adjacency_fingerprint`).
- `lib/geom.ml` — Tiling geometry on the unit square. `Geom.t` holds tile rectangles (from `resolve_splits`) and adjacency edges (geometric, excluding point contact per Eppstein). `of_tiling`, `rect_of`, `center_of`, `edges`, `neighbors`.
- `lib/svg.ml` — SVG rendering. `render_tiling_group` (resolved splits with spectral cut colors). `render_tree_diagram` (node-link tree). `render_adjacency_graph` (planar graph from Geom.t, edges colored by `cut_depth`). Legacy `render_group`/`render`/`render_interactive` for binary Term.t (marked TODO).
- `bin/tiling_test.ml` — Generates `svg/schroeder_N.svg` (all tilings grouped by D4 orbit), `svg/d4_shrot_N.svg` (one representative per D4 orbit with tree + adjacency graph), `svg/operations.svg`. Supports `--max-svg N` to skip SVG for large N.
- `bin/adjacency_check.ml` — Verifies geometric adjacency ⊆ tabstop potential adjacency.
- `bin/topology_check.ml` — Verification that D4 orbits have isomorphic adjacency graphs.
- `bin/conjecture_check.ml` — Efficient single-pass verification via fingerprinting.
- `bin/cross_check.ml` — Verifies corner-counting and cut-intersection degenerate detection agree.
- `lib/poset.ml` — Adjacency poset as a 2-dimensional lattice (Asinowski et al. 2024, Prop. 9). `Poset.t` holds two rank maps (linear extensions of P_a) whose intersection is the partial order. `of_geom`, `compare`, `is_covering`, `coverings`, `minimum`, `maximum`. Poset direction: `a < b` means a is left-of or below b.
- `bin/poset_check.ml` — Verifies poset encoding: (A) every geometric edge is comparable, (B) every covering is a geometric edge, (C) antisymmetry, (D) extrema exist. Verified up to n=8 (10,879 tilings).
- `bin/flip_test.ml` — SVG visualization of all 5 flip operations on example tilings. Generates `svg/flips.svg`.
- `bin/flip_check.ml` — Model checker for flip properties: (A) size preservation, (B) validity, (C) invertibility, (D) connectivity. Verified A/B/D up to n=7 (1806 tilings). C: ~40% directly invertible; the rest are multi-level pivots.

### Binary tree layer (legacy — each file marked `TODO: Bring up to Schroder tilings`)

This layer is functional but frozen. It operates on binary `Term.t` trees with fixed H/V splits. The next major milestone is migrating these operations to native Schroder tree operations (`split`, `close`, `neighbor` in `tiling.ml`).

- `lib/term.ml` — `Term.t = Leaf of int | H of t * t | V of t * t`. H splits top/bottom, V splits left/right.
- `lib/rewrite.ml` — Low-level rules: `Split_h`, `Split_v`, `Close_*`, `Swap`, `Promote`, `Demote`, `Rotate`, `Transpose`, `Slide`, `Exchange`. `apply rule term` transforms a tree.
- `lib/command.ml` — High-level spatial commands (`Move(n, dir)`, `Split(n, dir)`, `Close(n)`). `compile cmd term` produces a `Rewrite.rule list`.
- `lib/path.ml` — Path from root to leaf as list of steps. Dyadic rational metrics: `perp_overlap`, `aspect_distortion`, `center_moved`, `perp_depth`, `touches_wall`, `extent_increased`.
- `lib/tabstop.ml` — Symbolic boundary extraction and neighbor finding for binary terms.
- `lib/policy.ml` — First-class policy modules (`positional`, `dominance`, `territorial`).
- `lib/parser.ml` — Parses rule names from strings (stdin protocol).
- `bin/main.ml` — Interactive CLI: reads rules from stdin, updates SVG.
- `bin/test_svg.ml` — Visual test suite generator. One file per policy per category.
- `bin/model_check.ml` — Exhaustive checker: enumerates all trees up to k leaves, checks every command against policy predicates.
- `bin/reachability.ml` — SCC analysis of the state graph under move operations.
- `bin/web.ml` — Browser prototype (Dream server). Uses Schroder tilings for rendering but legacy Command/Rewrite for operations.

## Schroder tree model

The core representation is `int Schrot.tiling = bool * int Schrot.t`:
- `bool` = root orientation (true = H = top-to-bottom, false = V = left-to-right)
- `Frame` children alternate orientation with depth
- `Tile n` = leaf window with label n
- No same-type nesting: H(a, H(b, c)) is impossible; instead H(a, b, c) as a 3-ary frame
- Tiling topology bijects with (orientation, Schroder tree shape)
- Enumeration counts match large Schroder numbers: 1, 2, 6, 22, 90, 394, ...
- Fewer topologies than binary tree shapes × H/V bitmask (flattening collapses equivalences)

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

Two notions of adjacency, both derived from the Schroder tree:

- **Tabstop (potential)**: `tabstop_all_adjacencies` returns all tile pairs sharing a tabstop on opposing sides. This is the maximal set — every pair that is adjacent in *some* concrete layout. Includes both diagonals at cross junctions.
- **Geometric (authoritative)**: `Geom.of_tiling` computes tile rectangles via `resolve_splits` (iterative repulsion), then finds pairs sharing a boundary of positive length. Excludes point contact. This is the adjacency of the rendered layout.

Adjacency topology depends on split ratios. Guillotine partitions with cross junctions are not one-sided (Eppstein et al. 2009), so their adjacency graph changes with the geometry: tiles that touch at a point under one set of ratios can share a boundary segment under another. This means the tree structure alone does not determine adjacency — the split positions do.

Geometric ⊆ tabstop. The difference is exactly the unchosen diagonals at cross junctions (degenerate vertices under equal splits). At each cross junction, the diagonal is chosen deterministically by structural bias (D4-covariant).

### Adjacency poset

`Poset.of_geom` encodes the adjacency poset P_a as a 2-dimensional lattice: two total orders (stored as `IntMap.t` rank maps) whose intersection is the partial order. Tile a < b in P_a iff a is left-of or below b (transitively through adjacency edges). The poset is computed from `Geom.t` by orienting each adjacency edge, then running two topological sorts with different tiebreakers (left-right priority vs bottom-top priority).

The Schroder tree determines a total order on tiles via lowest common ancestor, but this is NOT the adjacency poset. P_a requires adjacency for its base relation, and two tiles can be tree-ordered without being adjacent or transitively connected. This happens at cross junctions: in h(v(0,1), v(2,3)) without the diagonal, tiles 0 and 3 are incomparable in P_a (the Hasse diagram is a diamond, not a chain). The two total orders diverge on such incomparable pairs.

When `resolve_splits` adds a diagonal edge, the diamond collapses to a total order and both rank maps agree. Under dynamic geometry (user-driven segment sliding), the topology transitions: near-degenerate = total order, at the cross = diamond, past the cross = different total order. The two-order encoding tracks this smoothly.

Covering relations of P_a are a subset of adjacency edges. When a < c < b transitively, the edge a-b is not a covering even though a and b are geometrically adjacent. `Poset.coverings` recovers the Hasse diagram; `Poset.compare` gives the full partial order.

### Junction resolution

`resolve_splits` eliminates cross junctions (4-multiplicity points under equal splits) by spreading coincident cuts at each parent boundary into evenly spaced positions. Only boundaries with actual coincidences (same position, different frames) are affected; tilings without crosses keep exact equal splits.

D4-covariant structural bias: cuts are sorted by `(before_height - after_height)` — each cut moves toward its shallower side, giving the deeper subtree more room. This is invariant under D4 symmetry because subtree height is a tree property.

Relaxation: `pos += 0.3 * (target - pos)` with early exit when max displacement < 1e-6 (typically ~31 iterations).

### References

- Zeidler, Weber, Gavryushkin, Lutteroth. "Tiling Algebra for Constraint-based Layout Editing." J. Logical and Algebraic Methods in Programming, 2017. — Tabstops as shared constraint variables; adjacency = shared tabstop.
- Eppstein, Mumford, Speckmann, Verbeek. "Area-Universal and Constrained Rectangular Layouts." SIAM J. Computing, 2012. — Area-universality iff one-sided; point contact excluded from adjacency; adjacency depends on split ratios in non-one-sided layouts.
- Baez. "Guillotine Partitions and the Hipparchus Operad." Azimuth blog, 2022. — Bijection between guillotine partition types and Schroder trees. Operad structure: split = composition, Schroder numbers count tilings.
- Asinowski, Cardinal, Felsner, Fusy. "Combinatorics of rectangulations: Old and new bijections." 2024. — P_a is a planar 2-dimensional lattice (Prop. 9); strong poset, flip graph, permutation bijections, guillotine characterization via windmill avoidance.
- Reading. "Generic rectangulations." Europ. J. Comb., 33(4):610-623, 2012. — Strong equivalence classes form a lattice congruence of the weak Bruhat order; 2-clumped permutations. Foundation for the flip graph lattice structure.
- Pilaud, Santos. "Quotientopes." Bull. London Math. Soc., 51(3):406-420, 2019. — Any lattice congruence of the weak Bruhat order on S_n yields a polytope (quotientope) whose skeleton is the flip graph. Proves the rectangulation flip graph is polytopal.

## Move compilation (binary layer)

Scan path from leaf toward root, find first aligned+favorable ancestor:

- **Slide** `[Slide n]` — immediate parent is aligned+favorable. Slide enters same-type compound siblings past the first geometric neighbor; degenerates to Swap for leaf/perpendicular siblings.
- **Exchange** `[Exchange(n, k)]` — ancestor (not parent) is aligned+favorable. Swaps leaf values n <-> k where k is the nearest geometric neighbor. Preserves tree structure and tile sizes.
- **Impossible** `[]` — no aligned+favorable ancestor in path.

### Invariant
Moves must not cause perpendicular drift: the tile's extent in the perpendicular axis may change, but its position must not shift sideways.

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

Three layers of algebraic structure govern tiling operations, each with its own mathematical object, operations, and symmetry:

### Layer 1 — Between sizes: the Hipparchus operad

**Object**: the family of guillotine tilings across all sizes n = 1, 2, 3, ...
**Operations**: split (SR_n → SR_{n+1}), close (SR_{n+1} → SR_n).
**Structure**: operad — composition replaces a tile with a sub-tiling.
**Reference**: Baez 2022 (Hipparchus operad); Schroder numbers count tilings.

Split and close change the number of tiles. They are maps between quotientopes at different n. In operad language, split is composition: given a tiling of size n, replace a tile with a 2-tile sub-tiling to get a tiling of size n+1. Close is the partial inverse. Whether these maps are lattice morphisms (preserving the quotientope structure at each level) is an open question.

In the codebase: `Tiling.split` and `Tiling.close` implement these directly on Schroder trees.

### Layer 2 — Fixed size: the quotientope

**Object**: the set SR_n of strong rectangulations of size n.
**Operations**: 3 flip types (simple flip, pivot, wall slide) — 5 under the full classification, 3 under D4.
**Structure**: the flip graph is the skeleton of a convex polytope (quotientope), and its orientation is a lattice.
**References**: Reading 2012 (lattice congruence); Pilaud-Santos 2019 (polytopality); Asinowski et al. 2024, Theorem 22 (the 5 flip types).

Flips preserve size. They are the cover relations of the lattice on SR_n: every flip either goes "up" or "down," there are no cycles, and every pair of tilings has a unique meet and join. The polytope structure guarantees connectivity (any tiling can reach any other through a sequence of flips) and rules out dead ends.

In the codebase: `simple_dissolve`/`simple_create` (simple flip: dissolve or create a 2-ary all-Tile sub-frame), `pivot_out`/`pivot_in` (pivot: extract or insert a boundary leaf across Frame levels, with merge-into-context for alternation), `wall_slide` (swap two consecutive children). `enumerate_flips` generates all applicable flips for a tiling. Verified: size preservation, validity, and flip graph connectivity up to n=7. See also `lib/rewrite.ml` TODO for the legacy binary layer these replace.

### Layer 3 — Fixed tree, varying geometry: the 2-dimensional lattice

**Object**: the set of strong representatives within one weak equivalence class (= one Schroder tree with varying split positions).
**Operations**: segment sliding (continuous geometry change).
**Structure**: the adjacency poset P_a is a planar 2-dimensional lattice, encoded as the intersection of two total orders.
**Reference**: Asinowski et al. 2024, Proposition 9.

Segment sliding preserves the tree but changes which tiles are adjacent. At cross junctions, sliding a cut past the degenerate point switches the diagonal adjacency, changing the strong equivalence class. The two-order encoding tracks this topology change smoothly: both orders agree when all cross junctions are resolved, and diverge on incomparable pairs at unresolved junctions.

In the codebase: `Poset.of_geom` computes the two-order encoding from `Geom.t`. D4 acts on the pair of orders by swapping and reversing (the hyperoctahedral group B_2).

### Mapping to Hyprland user actions

Each layer corresponds to a mode of user interaction with the compositor. The keyboard interface uses two focus modes — tile focus and segment focus — sharing the same arrow-key vocabulary:

| Focus | Keys | Action | Layer | Codebase entry point |
|---|---|---|---|---|
| Tile | split key | Open a window | 1 (operad) | `Tiling.split` |
| Tile | close key | Close a window | 1 (operad) | `Tiling.close` |
| Tile | arrow | Move/swap the tile | 2 (quotientope) | TODO: 3 flip types |
| Tile | modifier+arrow | Grow the tile in a direction | 3→1 (cross-layer) | TODO: see below |
| Segment | arrow | Slide the segment by one step | 3 (2D lattice) | TODO: split ratio update |

Segment focus makes Layer 3 keyboard-driven without mouse drag. A segment is a tabstop — already a first-class object via `tabstop_extract`, where each Frame boundary has an identity. Selecting a segment = selecting a tabstop. Moving it = changing the split ratio of the corresponding Frame node, by a fixed increment quantum. The command compiler translates both focus modes: tile-focused arrows → find neighbor, select flip type, apply; segment-focused arrows → adjust split ratio, recompute adjacency.

Resize is not yet modeled incrementally — `resolve_splits` computes positions from scratch. The incremental version would update one Frame's split ratio and call `Poset.of_geom` on the resulting geometry. `Poset.of_geom` is ready for this — it takes any `Geom.t`, not just one from `resolve_splits`.

### Grow operation (lead, not yet designed)

A possible third tile-focus action: modifier+arrow grows the focused tile in a direction, consuming adjacent space. This is a **cross-layer operation** — Layer 3 (segment slide) with a Layer 1 callback (eviction when a neighbor is consumed):

```
grow(T, dir, quantum):
  1. s ← boundary segment of T in direction dir   (tabstop lookup)
  2. move s by quantum in dir                      (Layer 3: ratio change)
  3. for each tile on the far side of s with size < minimum:
       evict(tile)                                 (Layer 1: close + policy)
```

The eviction policy is an open design question:
- **Destroy**: `close(tile)`. Simplest. Loses the window.
- **Displace**: `close(tile)` then `split(target, dir)` to reinsert elsewhere. Preserves all windows. Needs a placement policy to choose the reinsertion target.
- **Compress**: enforce a minimum tile size, growth stops at the limit. Pure Layer 3, no eviction.

The mathematical semantics: grow is a Layer 3 operation parameterized by a Layer 1 eviction callback. The segment slide is primary; eviction is a boundary condition triggered when the geometry degenerates. This keeps the layers cleanly separated — the resize logic doesn't need to know the eviction policy.

### Connections between layers

- D4 symmetry acts on all three layers: on the operad (tree symmetry), on the quotientope (flip graph symmetry), and on the 2-dimensional lattice (swap/reverse the two orders).
- The tiling algebra (Zeidler et al. 2017) provides the specification language (`|` beside, `/` stacked, tabstops as shared constraint variables). Split and close are the algebra's introduction and elimination rules; flips are its equational theory.
- The three layers suggest a three-level verification strategy: visual SVG (all layers), model checking (layers 1-2), Rocq proof (all layers, with the tiebreaker correctness and lattice morphism questions as proof obligations).

## Branches

- `area-preserving` — Rational split ratios (`Q.t` per node) with area-preserving rewrite rules. Reverted from main because full area preservation is too strong. May be revisited with selective preservation.

## Future directions

- **Layer 2 refinement**: pivot_in insert mode (grow Frame, inverse of pivot_out from >=3-ary). Command compiler: `neighbor` → flip type selection → apply. Distinguish single-cover pivots from multi-cover jumps.
- **Layer 3 implementation**: mutable split positions, incremental `Poset.of_geom` after single-segment updates. Entry point for Hyprland border-drag resize.
- Model checking with Schroder enumeration (fewer topologies than binary)
- Three-level verification: visual SVG → model checking → Rocq proof
- D4 orbit reduction of the paper's counting sequences (see below)
- Proof obligations for Rocq: tiebreaker correctness (poset.ml), lattice morphism of split/close (open question)

### D4 reduction of counting sequences

D4 orbit reduction applied to Schroder tilings (weak guillotine rectangulations) produces the sequence 1, 1, 2, 6, 18, 68, 270, 1195, ... which is not in OEIS. The same technique applies to any D4-invariant family of rectangulations.

The paper (Asinowski et al. 2024) introduces several new counting sequences. Each can be D4-reduced:

| Family | Sequence | OEIS | D4 reduction |
|---|---|---|---|
| Weak guillotine (Schroder) | 1, 2, 6, 22, 90, 394, ... | A006318 | 1, 1, 2, 6, 18, 68, ... (computed, not in OEIS) |
| Strong guillotine | 1, 2, 6, 24, 114, 606, ... | none | not yet computed |
| All strong rectangulations | 1, 2, 6, 24, 116, 642, ... | A342141 | not yet computed |
| One-sided rectangulations | 1, 2, 6, 20, 72, 274, ... | A348351 | not yet computed |

To compute D4 orbits of strong guillotine rectangulations: enumerate all diagonal choices at cross junctions within each weak tiling (2^k strong classes per weak class with k junctions), then group the resulting strong classes by D4 orbit. The multiplicity per weak class follows the paper's Section 5.3 recurrence. The D4 tree operations are already in `tiling.ml`.
