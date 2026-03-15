# Nachum

A tiling window manager algebra in OCaml. Binary trees (H/V splits with numbered leaves) represent screen layouts. Low-level rewrite rules transform trees; a high-level command compiler translates spatial intent (move, split, close) into rule sequences. The project is heading toward formally verified configurable policies (visual → model checking → Rocq).

## Build

```sh
dune build
dune exec bin/main.exe -- --output out.svg    # interactive, reads rules from stdin
dune exec bin/test_svg.exe -- --output svg       # generates svg/{policy}_{open_close,move}.svg
dune exec bin/model_check.exe -- --policy dominance --max-leaves 6
```

All generated files (SVGs, etc.) go under `svg/` in the project, never in `/tmp`.


## Architecture

- `lib/term.ml` — `Term.t = Leaf of int | H of t * t | V of t * t`. H splits top/bottom, V splits left/right. Left child = top (H) or left (V).
- `lib/rewrite.ml` — Low-level rules: `Split_h`, `Split_v`, `Close_h_l`, `Close_h_r`, `Close_v_l`, `Close_v_r`, `Swap`, `Promote`, `Demote`, `Rotate`. `apply rule term` transforms a tree.
- `lib/command.ml` — High-level spatial commands (`Move(n, dir)`, `Split(n, dir)`, `Close(n)`). `compile cmd term` produces a `Rewrite.rule list` by analyzing the tree path to the target leaf. Returns `[]` for impossible moves. Also exports `find_path`, `to_string`, `rule_list_to_string`.
- `lib/geometry.ml` — Geometric interpretation: `interpret` maps a tree to `(int * rect) list` on the unit square (equal 50/50 splits). Helpers: `center_x`, `center_y`, `edge_extent`.
- `lib/policy.ml` — First-class policy modules (`module type S` with `name`, `compile`, `predicate`). Two policies: `positional` (center movement) and `dominance` (cascade: positional, then rotate for edge extent). Also exports `find`, `all`.
- `lib/svg.ml` — SVG rendering. `render_group` accepts optional `~color_of` for custom leaf coloring. `render` wraps in `<svg>` tag.
- `lib/parser.ml` — Parses rule names from strings (stdin protocol).
- `bin/main.ml` — Interactive CLI: reads rules from stdin, updates SVG.
- `bin/test_svg.ml` — Visual test suite generator. One file per policy per category (open_close, move). `--output DIR --policies name1,name2` (default: all policies).
- `bin/model_check.ml` — Exhaustive checker: enumerates all trees up to k leaves (shapes × splits × permutations), checks every Move/Split/Close against policy predicates.

## Conventions

- Leaf numbering starts at 0.
- H(a, b): a = top, b = bottom. V(a, b): a = left, b = right.
- In paths: side L = first child, R = second child.
- Shorthand: h(_v) = h(0, v(1,2)), h(h_) = h(h(0,1), 2), etc.

## Move compilation (current policy)

5 outcomes based on tree path analysis:
- **Swap** `[Swap n]` — aligned parent, favorable side, leaf or perpendicular sibling
- **Slide** `[Swap n; Demote n; Swap n]` — aligned parent, favorable side, same-type compound sibling
- **Promote** `[Promote n]` — perpendicular parent, aligned+favorable outer
- **Double Promote** `[Promote n; Promote n]` — same-type nesting, unfavorable inner, favorable outer
- **Impossible** `[]` — all other cases

## Test suite

- Symmetry reduction: 2 orbit representatives (h(_v) mixed, h(_h) same-type) × tiles 0,1 × 4 directions = 16 cases covering all 96 by H↔V rotation, outer child swap, inner child swap.
- Color coding: dark blue (#1a3a6b) = target tile, light blue (#a8d0e6) = displaced tiles, white = unchanged.

## Design methodology

This project uses iterative visual verification driving algebraic rule refinement:
1. Implement or change rewriting rules
2. Generate SVG test pairs (before→after) with `test_svg.exe`
3. Visually inspect with `eog`
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
- D₄ symmetry (dihedral group of the square)
- Fragments under | and / form cancellative semigroups with involution

The connection to nachum: the algebra states geometric policies, term rewriting implements them at runtime (no solver needed), and proofs connect the two. Different policies = different compilations of the same high-level commands.

## Future directions

- Tree will gain split-ratio/aspect annotations (like i3 percent)
- Tree will support n-ary splits (Hipparchus operad generalization)
- Multiple configurable policies with geometric contracts
- Three-level verification: visual SVG → model checking → Rocq proof
