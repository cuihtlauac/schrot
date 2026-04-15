# References

PDF files are git-ignored. To verify integrity: `sha256sum *.pdf`.

| File | Paper | SHA-256 |
|---|---|---|
| `rectangulations.pdf` | Asinowski, Cardinal, Felsner, Fusy. "Combinatorics of rectangulations: Old and new bijections." 2024. | `e4795ec9781dffa9bfc934cfbde3864455ed0d08b395c1f3c3ccfc3b7f5d8868` |
| `generic_rectangulations.pdf` | Reading. "Generic rectangulations." arXiv:1105.3093v3, 2012. | `55414c1ffc00382e3f30a1010034ec44c02795d12f8e172a847284a460c77b2f` |
| `quotientopes.pdf` | Pilaud, Santos. "Quotientopes." 2019. | `e8f6c093011ebc0c6a281f8f859510fd24b641585bee07bf0521054852b176b2` |
| `2103.09333v2.pdf` | Merino, Mütze. "Combinatorial generation via permutation languages. III. Rectangulations." arXiv:2103.09333v2. | `318c58aeeb0ce7865832131d9dffec694436a3dea5420b0a6b0cd331a082241e` |

## Citations

- Zeidler, Weber, Gavryushkin, Lutteroth. "Tiling Algebra for Constraint-based Layout Editing." J. Logical and Algebraic Methods in Programming, 2017. — Tabstops as shared constraint variables; adjacency = shared tabstop.
- Eppstein, Mumford, Speckmann, Verbeek. "Area-Universal and Constrained Rectangular Layouts." SIAM J. Computing, 2012. — Area-universality iff one-sided; point contact excluded from adjacency; adjacency depends on split ratios in non-one-sided layouts.
- Baez. "Guillotine Partitions and the Hipparchus Operad." Azimuth blog, 2022. — Bijection between guillotine partition types and Schroder trees. Operad structure: split = composition, Schroder numbers count tilings. See also: "The Hipparchus Operad." n-Category Cafe, 2013 (original naming; permutation-polynomial connection via Kontsevich; Vallette comment linking to Koszul duality in Loday-Vallette 2012). "Dividing a Square into Similar Rectangles." Azimuth, 2022 (speculative double-category remark: 2-cells = subdivided rectangles). Baez's treatment is combinatorial (counting, tree enumeration, operad identification) — he does not address flattening, flip graphs, quotientopes, or lattice congruences.
- Loday, Vallette. *Algebraic Operads.* Springer, 2012. — Koszul duality for operads. The magmatic-infinity operad (= Hipparchus operad) is treated. Koszul dual may govern close-as-dual-to-split; not yet worked out for tilings.
- Aguiar, Livernet. "The associative operad and the weak order on the symmetric groups." J. Homotopy Relat. Struct., 2007. — Operadic composition on symmetric group algebras = intervals of the weak order. Establishes the operad-to-lattice bridge for the associative operad (binary trees / Catalan). Whether this extends to the Hipparchus operad (Schroder trees / rectangulation quotientopes) is an open question.
- Asinowski, Cardinal, Felsner, Fusy. "Combinatorics of rectangulations: Old and new bijections." 2024. — P_a is a planar 2-dimensional lattice (Prop. 9); strong poset, flip graph, permutation bijections, guillotine characterization via windmill avoidance.
- Reading. "Generic rectangulations." Europ. J. Comb., 33(4):610-623, 2012. — Strong equivalence classes form a lattice congruence of the weak Bruhat order; 2-clumped permutations. Foundation for the flip graph lattice structure.
- Pilaud, Santos. "Quotientopes." Bull. London Math. Soc., 51(3):406-420, 2019. — Any lattice congruence of the weak Bruhat order on S_n yields a polytope (quotientope) whose skeleton is the flip graph. Proves the rectangulation flip graph is polytopal.
