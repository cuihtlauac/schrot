# Nachum

A tiling window manager algebra in OCaml. Schroder trees represent tiling topologies (1-to-1 correspondence). Low-level rewrite rules transform trees; a high-level command compiler translates spatial intent (move, split, close) into rule sequences. The project is heading toward formally verified configurable policies (visual → model checking → Rocq).

## Build

```sh
dune build
dune exec bin/main.exe -- --output out.svg    # interactive, reads rules from stdin
dune exec bin/test_svg.exe -- --output svg       # generates svg/{policy}_{open_close,move}.svg
dune exec bin/tiling_test.exe -- --output svg    # Schroder tiling enumeration + open/close SVGs
dune exec bin/model_check.exe -- --policy dominance --max-leaves 6
dune exec bin/web.exe                            # browser prototype at http://localhost:8080
```

All generated files (SVGs, etc.) go under `svg/` in the project, never in `/tmp`.


## Architecture

### Schroder tiling layer

- `lib/list2.ml` — `List2.t = Cons2 of 'a * 'a * 'a list`. Lists with >= 2 elements, mirroring Stdlib.List API.
- `lib/schrot.ml` — `Schrot.t = Tile of 'a | Frame of 'a t List2.t`. Schroder trees where internal nodes have >= 2 children. `tiling = bool * 'a t` (bool = root is H). Provides `fold`, `unfold`, `map`, `enum`.
- `lib/tiling.ml` — Tiling operations on `int Schrot.tiling`. `dir = H | V`. `split`, `close`, `neighbor` (tree-based navigation). Junction resolution via relaxation toward evenly spaced targets (`resolve_splits`). Tabstop extraction and potential adjacency (`tabstop_all_adjacencies`). `cut_depth` (depth of the separating cut between two tiles). D4/V4 symmetry actions and canonical forms. Degenerate vertex detection (`degenerate_corners`, `degenerate_cuts`). Graph utilities (`graphs_isomorphic`, `adjacency_fingerprint`).
- `lib/geom.ml` — Tiling geometry on the unit square. `Geom.t` holds tile rectangles (from `resolve_splits`) and adjacency edges (geometric, excluding point contact per Eppstein). `of_tiling`, `rect_of`, `center_of`, `edges`, `neighbors`.
- `lib/svg.ml` — SVG rendering. `render_tiling_group` (resolved splits with spectral cut colors). `render_tree_diagram` (node-link tree). `render_adjacency_graph` (planar graph from Geom.t, edges colored by `cut_depth`). Legacy `render_group`/`render`/`render_interactive` for binary Term.t (marked TODO).
- `bin/tiling_test.ml` — Generates `svg/schroeder_N.svg` (all tilings grouped by D4 orbit), `svg/d4_shrot_N.svg` (one representative per D4 orbit with tree + adjacency graph), `svg/operations.svg`. Supports `--max-svg N` to skip SVG for large N.
- `bin/adjacency_check.ml` — Verifies geometric adjacency ⊆ tabstop potential adjacency.
- `bin/topology_check.ml` — Verification that D4 orbits have isomorphic adjacency graphs.
- `bin/conjecture_check.ml` — Efficient single-pass verification via fingerprinting.
- `bin/cross_check.ml` — Verifies corner-counting and cut-intersection degenerate detection agree.
- `lib/poset.ml` — Adjacency poset as a 2-dimensional lattice (Asinowski et al. 2024, Prop. 9). `Poset.t` holds two rank maps (linear extensions of P_a) whose intersection is the partial order. `of_geom`, `compare`, `is_covering`, `coverings`, `minimum`, `maximum`. Poset direction: `a < b` means a is left-of or below b.
- `bin/poset_check.ml` — Verifies poset encoding: (A) every geometric edge is comparable, (B) every covering is a geometric edge, (C) antisymmetry, (D) extrema exist. Verified up to n=8 (10,879 tilings).

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
- Baez. "Guillotine Partitions and the Hipparchus Operad." Azimuth blog, 2022. — Bijection between guillotine partition types and Schroder trees.
- Asinowski, Cardinal, Felsner, Fusy. "Combinatorics of rectangulations: Old and new bijections." 2024. — P_a is a planar 2-dimensional lattice (Prop. 9); strong poset, flip graph, permutation bijections, guillotine characterization via windmill avoidance.

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

The tiling algebra (Zeidler et al. 2017) provides the specification language:
- `|` (beside) and `/` (stacked) operators on rectangular areas
- Tabstops as shared constraint variables (x-tabs, y-tabs)
- D4 symmetry (dihedral group of the square)
- Fragments under | and / form cancellative semigroups with involution

The Schroder tree connection: large Schroder numbers count rectangular tilings. Each Schroder tree shape with a root orientation tag uniquely determines a tiling topology. The algebra states geometric policies, term rewriting implements them at runtime (no solver needed), and proofs connect the two.

## Branches

- `area-preserving` — Rational split ratios (`Q.t` per node) with area-preserving rewrite rules. Reverted from main because full area preservation is too strong. May be revisited with selective preservation.

## Future directions

- Migrate rewrite rules and policies from binary Term.t to Schroder Tiling.t
- Move compilation on Schroder trees (n-ary slide, exchange, promote/demote)
- Model checking with Schroder enumeration (fewer topologies than binary)
- Split-ratio/aspect annotations per frame
- Three-level verification: visual SVG → model checking → Rocq proof
