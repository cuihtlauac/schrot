# Theoretical framework

Three layers of algebraic structure govern tiling operations, each with its own mathematical object, operations, and symmetry.

## Layer 1 — Between sizes: the Hipparchus operad

**Object**: the family of guillotine tilings across all sizes n = 1, 2, 3, ...
**Operations**: split (SR_n -> SR_{n+1}), close (SR_{n+1} -> SR_n).
**Structure**: operad — composition replaces a tile with a sub-tiling.
**Reference**: Baez 2022 (Hipparchus operad); Schroder numbers count tilings.

Split and close change the number of tiles. They are maps between quotientopes at different n. In operad language, split is composition: given a tiling of size n, replace a tile with a 2-tile sub-tiling to get a tiling of size n+1. Close is the partial inverse. Whether these maps are lattice morphisms (preserving the quotientope structure at each level) is an open question.

**Flattening quotient.** The Hipparchus operad is the *free* operad on one operation per arity >= 2. Schrot's no-same-type-nesting constraint (H(a, H(b,c)) = H(a,b,c)) is a quotient of this free operad: same-orientation compositions are identified with higher-arity operations. The split code implements this directly — same-orientation splits insert siblings into the existing frame rather than nesting a new sub-frame. Baez does not discuss this quotient; it is specific to oriented (H/V) guillotine partitions.

In the codebase: `Tiling.split` and `Tiling.close` implement these directly on Schroder trees.

## Layer 2 — Fixed size: the quotientope

**Object**: the set SR_n of rectangulations of size n (the precise set depends on which flip graph is under discussion — see below).
**Structure**: the flip graph is the skeleton of a convex polytope (quotientope), and its orientation is a lattice.
**References**: Reading 2012 (lattice congruence); Pilaud-Santos 2019 (polytopality); Merino-Mütze 2021 §2.2 and Theorem 19 (generic flip graph); Asinowski et al. 2024 Theorem 22 (strong-poset flip graph) and Theorem 27 (windmill characterization).

### Three distinct flip graphs

1. **Merino-Mütze generic** (2021 §2.2, Theorem 19).  Object: all generic rectangulations (including windmills).  Operations: T-flip, simple flip, wall slide.  Invertible by Theorem 19 *within the generic space*.  The T-flip is not closed on the guillotine subset: applied to a guillotine input it can produce a windmill output.  The codebase's `Geom.apply_t_flip` / `Geom.enumerate_t_flips` implement this operation.

2. **Asinowski strong poset** (2024 §4.5, Theorem 22).  Object: strong (guillotine, i.e. windmill-avoiding per Theorem 27) rectangulations.  Operations: L-B pivoting, R-T pivoting, simple flip, V-slide, H-slide — five types, three under D4 symmetry.  Cover relations of a lattice, **closed on guillotine by construction**.  Geometrically, Asinowski's pivoting flips *are* M-M's T-flip restricted to guillotine-preserving cases — same rotation, stricter admissibility.

3. **Weak guillotine / Schroder-tree quotient**.  Object: weak-guillotine classes, i.e. Schroder trees.  This is the quotient of the strong poset by S-equivalence (cross-junction diagonals identified).  It is what the Schrot codebase represents as `int Schrot.tiling`.  Flips on this quotient descend from Asinowski's strong-poset flips; some strong-poset flips become identity on the quotient (endpoints in the same weak class), others become non-trivial cover relations.

Flips preserve size.  They are the cover relations of the lattice on their respective object set; every flip either goes "up" or "down," there are no cycles, and every pair of tilings has a unique meet and join.  The polytope structure guarantees connectivity and rules out dead ends *within the graph under consideration*.

In the codebase: `simple_dissolve`/`simple_create` (simple flip), `wall_slide` (swap two consecutive children), and `T_flip` (Asinowski pivoting, produced by `Geom.t_flip` or `Geom.enumerate_t_flips`).  `Tiling.enumerate_flips` generates simple/wall_slide; the `Geom.*` arm supplies T-flips.  Verified at n≤7: size preservation, validity, flip graph connectivity, and invertibility (Property C at 2942/2942).  The legacy `pivot_*` tree-heuristic attempt was removed in Phase F — `Geom.t_flip` with Asinowski admissibility is the correct tree-level T-flip.

## Layer 3 — Fixed tree, varying geometry: the 2-dimensional lattice

**Object**: the set of strong representatives within one weak equivalence class (= one Schroder tree with varying split positions).
**Operations**: segment sliding (continuous geometry change).
**Structure**: the adjacency poset P_a is a planar 2-dimensional lattice, encoded as the intersection of two total orders.
**Reference**: Asinowski et al. 2024, Proposition 9.

Segment sliding preserves the tree but changes which tiles are adjacent. At cross junctions, sliding a cut past the degenerate point switches the diagonal adjacency, changing the strong equivalence class. The two-order encoding tracks this topology change smoothly: both orders agree when all cross junctions are resolved, and diverge on incomparable pairs at unresolved junctions.

In the codebase: `Poset.of_geom` computes the two-order encoding from `Geom.t`. In theory, D4 acts on the pair of orders by swapping and reversing (the hyperoctahedral group B_2). In practice, `resolve_splits` does not produce D4-covariant geometry (see REPORT.md), so the poset of a D4 image may differ from the D4 transform of the original poset. The abstract action is correct; the concrete realization via `resolve_splits` is not.

## Mapping to Hyprland user actions

Each layer corresponds to a mode of user interaction with the compositor. The keyboard interface uses two focus modes — tile focus and segment focus — sharing the same arrow-key vocabulary:

| Focus | Keys | Action | Layer | Codebase entry point |
|---|---|---|---|---|
| Tile | split key | Open a window | 1 (operad) | `Tiling.split` |
| Tile | close key | Close a window | 1 (operad) | `Tiling.close` |
| Tile | arrow | Move/swap the tile | 2 (quotientope) | TODO: 3 flip types |
| Tile | modifier+arrow | Grow the tile in a direction | 3->1 (cross-layer) | TODO: see below |
| Segment | arrow | Slide the segment by one step | 3 (2D lattice) | TODO: split ratio update |

Segment focus makes Layer 3 keyboard-driven without mouse drag. A segment is a tabstop — already a first-class object via `tabstop_extract`, where each Frame boundary has an identity. Selecting a segment = selecting a tabstop. Moving it = changing the split ratio of the corresponding Frame node, by a fixed increment quantum. The command compiler translates both focus modes: tile-focused arrows -> find neighbor, select flip type, apply; segment-focused arrows -> adjust split ratio, recompute adjacency.

Resize is not yet modeled incrementally — `resolve_splits` computes positions from scratch. The incremental version would update one Frame's split ratio and call `Poset.of_geom` on the resulting geometry. `Poset.of_geom` is ready for this — it takes any `Geom.t`, not just one from `resolve_splits`.

## Grow operation (lead, not yet designed)

A possible third tile-focus action: modifier+arrow grows the focused tile in a direction, consuming adjacent space. This is a **cross-layer operation** — Layer 3 (segment slide) with a Layer 1 callback (eviction when a neighbor is consumed):

```
grow(T, dir, quantum):
  1. s <- boundary segment of T in direction dir   (tabstop lookup)
  2. move s by quantum in dir                      (Layer 3: ratio change)
  3. for each tile on the far side of s with size < minimum:
       evict(tile)                                 (Layer 1: close + policy)
```

The eviction policy is an open design question:
- **Destroy**: `close(tile)`. Simplest. Loses the window.
- **Displace**: `close(tile)` then `split(target, dir)` to reinsert elsewhere. Preserves all windows. Needs a placement policy to choose the reinsertion target.
- **Compress**: enforce a minimum tile size, growth stops at the limit. Pure Layer 3, no eviction.

The mathematical semantics: grow is a Layer 3 operation parameterized by a Layer 1 eviction callback. The segment slide is primary; eviction is a boundary condition triggered when the geometry degenerates. This keeps the layers cleanly separated — the resize logic doesn't need to know the eviction policy.

## Connections between layers

- D4 symmetry acts on all three layers: on the operad (tree symmetry), on the quotientope (flip graph symmetry), and on the 2-dimensional lattice (swap/reverse the two orders). The action is well-defined abstractly; the concrete realization via `resolve_splits` is not D4-covariant at Layer 3 (see REPORT.md).
- The tiling algebra (Zeidler et al. 2017) provides the specification language (`|` beside, `/` stacked, tabstops as shared constraint variables). Split and close are the algebra's introduction and elimination rules; flips are its equational theory.
- The three layers suggest a three-level verification strategy: visual SVG (all layers), model checking (layers 1-2), Rocq proof (all layers, with the tiebreaker correctness and lattice morphism questions as proof obligations).
