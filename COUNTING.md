# Counting strong guillotine rectangulations

## Summary

We independently verify and extend the enumeration of strong guillotine rectangulations from Asinowski et al. [4], compute D4-reduced counts (new sequences not in OEIS), and introduce non-generic variants allowing 4-way junctions. The work produces 6+ new integer sequences and a Burnside decomposition that reduces the D4 counting problem to 3 component sequences.

## What was done

### Three cross-validated counting methods

For each weak guillotine tiling (Schroder tree), three independent methods compute the number of strong equivalence classes:

1. **Cross-junction resolutions** (Σ 2^k per tree): counts diagonal choices at multiplicity-4 degenerate vertices only. A strict lower bound on the true strong count.

2. **Boundary multiplicity** (Σ Π C(a_j + b_j, a_j) per tree): for each internal boundary, a_j and b_j count perpendicular segments terminating from each side (including nested segments that transitively reach the boundary via `boundary_tiles_ordered`). This is the tree-recursive implementation of the paper's multiplicity formula. Matches the paper's values exactly.

3. **§5.3 recurrence** S(n, ℓ, t, r, b): the paper's 5-variable recurrence, parameterized by a weight function. For generic: weight(r', ℓ') = C(r'+ℓ', r'). For non-generic: weight(r', ℓ') = D(r', ℓ') (Delannoy number). Completely independent of tree enumeration.

Methods 2 and 3 agree for all n ≤ 8 (8558 weak tilings). Method 1 is strictly less from n = 5 onward.

### Non-generic counting via Delannoy numbers

Replacing the binomial interleaving weight C(a+b, a) with the Delannoy number D(a, b) = Σ_k C(a,k)C(b,k)2^k counts strong classes including those where perpendicular segments coincide (the + pattern, 4-way junctions). D(a, b) counts lattice paths from (0,0) to (a,b) with steps (1,0), (0,1), and (1,1). The (1,1) step represents a coincident position.

Full enumeration of non-generic strong classes via `enumerate_all_strong_adjacencies_nongeneric` generates all D(a_j, b_j) interleavings per boundary and verifies distinctness.

### D4-reduced counting and Burnside decomposition

For each counting variant (weak, generic strong, non-generic strong), D4-reduced counts are computed by enumerating all strong classes, computing a canonical D4 form for each (tiling, edge_set) pair, and counting distinct canonical forms.

Burnside's lemma decomposes each D4-reduced count:

  |orbits| = (1/8) Σ_{g ∈ D4} |Fix_g|

For guillotine tilings with n ≥ 2, only 3 of the 8 D4 elements produce non-zero fixed-point counts:

| D4 element | Fix > 0? | Reason |
|---|---|---|
| identity | yes | all tilings |
| rot90, rot270 | **no** | rotation swaps H↔V at root; no fixed tiling |
| rot180 | yes | palindromic child lists at every node |
| flip_h, flip_v | yes (equal) | conjugate in D4 |
| diag_ne, diag_nw | **no** | also swap H↔V at root |

So: D4-reduced = (Fix_id + Fix_rot180 + 2·Fix_flip) / 8.

### Sequences computed

All verified by `strong_check.ml` up to n = 8, with recurrence extension to n = 10.

| Family | Sequence (n=1..8) | OEIS | D4 reduction (n=1..8) |
|---|---|---|---|
| Weak guillotine (Schroder) | 1, 2, 6, 22, 90, 394, 1806, 8558 | A006318 | 1, 1, 2, 6, 18, 68, 270, 1195 |
| Strong guillotine (generic) | 1, 2, 6, 24, 114, 606, 3494, 21434 | A375913 | 1, 1, 2, 6, 20, 93, 474, 2800 |
| Cross-junction resolutions | 1, 2, 6, 24, 106, 514, 2610, 13810 | — | 1, 1, 2, 6, 19, 81, 368, 1903 |
| Non-generic strong guillotine | 1, 2, 6, 26, 138, 834, 5542, 39638 | — | 1, 1, 2, 7, 24, 128, 744, 5143 |

Extended to n = 20 via sparse bottom-up recurrence (tracking only non-zero states):

Generic (A375913):
1, 2, 6, 24, 114, 606, 3494, 21434, 138100, 926008, 6418576, 45755516, 334117246, 2491317430, 18919957430, 146034939362, 1143606856808, 9072734766636, 72827462660824, 590852491725920

Non-generic:
1, 2, 6, 26, 138, 834, 5542, 39638, 300610, 2391614, 19803342, 169640190, 1496293066, 13538467058, 125273931422, 1182487124842, 11362214115730, 110940123610378, 1099044317272334, 11032559127756038

Burnside component sequences (n = 1..8):

| Variant | Fix_rot180 | Fix_flip |
|---|---|---|
| Weak | 1, 2, 2, 6, 6, 22, 22, 90, 90, 394, 394, 1806 | 1, 2, 4, 10, 24, 64, 166, 456, 1234, 3454, 9600, 27246 |
| Generic strong | 1, 2, 2, 8, 6, 38, 26, 210 | 1, 2, 4, 8, 20, 50, 136, 378 |
| Non-generic strong | 1, 2, 2, 10, 6, 54, 30, 346 | 1, 2, 4, 10, 24, 68, 190, 580 |

## What remains to do

### 1. ~~Optimize the §5.3 recurrence~~ (DONE)

Sparse bottom-up recurrence (`guillotine_recurrence_sparse`) tracks only non-zero entries, indexed by (n, ℓ) for left-part lookup and (n, r) for right-part lookup. Extends both generic and non-generic sequences to n = 20 in seconds. Cross-validated against the dense recurrence for n ≤ 8.

### 2. Constrained generating functions for weak Burnside components (partially done)

The weak guillotine GF is algebraic: G(y) = (1 − y − √(1 − 6y + y²)) / 2.

**Fix_rot180 — SOLVED**: Fix_rot180(n) = S(⌊n/2⌋ + 1) where S(k) is the k-th large Schroder number. Verified against enumeration for n ≤ 12. This requires no generating function — the palindromic constraint reduces counting to Schroder numbers at half the size.

**Fix_flip — functional equation derived, not yet numerically solved**: flip_h reverses children at H-nodes only. A flip_h-fixed tree has conjugate-palindromic children at H-nodes (child i paired with flip_h(child_{k+1-i})) and individually fixed children at V-nodes. The functional equation is:

```
f_V(y) = y + f_H(y)² / (1 − f_H(y))
f_H(y) = y + g_V(y²) · (1 + f_V(y)) / (1 − g_V(y²))
```

where g_V(y) = (G(y) − y)/2 is the GF for all V-type Schroder trees (known). The g_V(y²) term reflects that palindrome halves at H-nodes use arbitrary V-trees (partner = flip conjugate, not equal). The system involves y² substitution, making it a functional equation (not purely algebraic). Iterative solution in PARI/GP is in progress; needs debugging of the coefficient-level iteration.

With Fix_rot180 exact and 12 enumerated terms of Fix_flip, the D4-reduced weak guillotine count is: D4(n) = (S(n) + S(⌊n/2⌋+1) + 2·flip(n)) / 8.

### 3. Constrained §5.3 recurrence for strong Burnside components

Adapt the 5-variable recurrence to count only g-fixed strong rectangulations by imposing boundary parameter constraints:

| g | Constraint on (ℓ, t, r, b) |
|---|---|
| rot180 | (ℓ, t, r, b) = (r, b, ℓ, t) |
| flip_h | (ℓ, t, r, b) = (ℓ, b, r, t) |

These constraints reduce the summation space, potentially giving tractable recurrences for Fix_rot180 and Fix_flip of the strong and non-generic variants.

### 4. Recurrence guessing from extended sequences

With 20 terms now available for both generic and non-generic raw sequences, use `gfun` (Maple/Sage) to:
- Test for P-recursive (holonomic) recurrences: a_n = f(n)·a_{n−1} + g(n)·a_{n−2} + …
- Test for algebraic generating functions
- Compute asymptotic growth rates and compare with known constants

The Burnside components are the primary targets — they should have simpler recurrences than the combined D4-reduced sequences.

### 5. OEIS submissions

Six sequences are candidates for OEIS submission once sufficiently many terms are computed:

1. D4-reduced weak guillotine: 1, 1, 2, 6, 18, 68, 270, 1195, …
2. D4-reduced generic strong guillotine: 1, 1, 2, 6, 20, 93, 474, 2800, …
3. Cross-junction resolutions (raw): 1, 2, 6, 24, 106, 514, 2610, 13810, …
4. D4-reduced cross-junction resolutions: 1, 1, 2, 6, 19, 81, 368, 1903, …
5. Non-generic strong guillotine (raw): 1, 2, 6, 26, 138, 834, 5542, 39638, …
6. D4-reduced non-generic strong guillotine: 1, 1, 2, 7, 24, 128, 744, 5143, …

Each needs: definition, offset, at least 10–15 terms, references, and cross-references to A006318 (Schroder) and A375913 (strong guillotine).

## Implementation

All code is in OCaml. The relevant files:

| File | Role |
|---|---|
| `lib/tiling.ml` | `multiplicity`, `multiplicity_nongeneric`, `boundary_tiles_ordered`, `enumerate_all_strong_adjacencies`, `enumerate_all_strong_adjacencies_nongeneric`, `cross_junction_tiles`, `diagonal_pairs`, `d4_actions`, `binom`, `delannoy` |
| `lib/poset.ml` | `of_adjacency` (factored from `of_geom` for explicit edge sets) |
| `bin/strong_check.ml` | Model checker: 3 counting methods, Burnside decomposition, non-generic recurrence, extended sequences |

```sh
dune exec bin/strong_check.exe -- --max-leaves 8
```

## References

[1] J. C. Baez, "Guillotine Partitions and the Hipparchus Operad," Azimuth blog, 2022.

[2] D. Eppstein, E. Mumford, B. Speckmann, K. Verbeek, "Area-Universal and Constrained Rectangular Layouts," SIAM J. Computing, 2012.

[3] N. Reading, "Generic Rectangulations," Europ. J. Comb., 33(4):610–623, 2012.

[4] A. Asinowski, J. Cardinal, S. Felsner, É. Fusy, "Combinatorics of rectangulations: Old and new bijections," 2024. (A375913 in OEIS.)

[5] V. Pilaud, F. Santos, "Quotientopes," Bull. London Math. Soc., 51(3):406–420, 2019.
