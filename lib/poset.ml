(* Adjacency poset of a tiling, encoded as a 2-dimensional lattice.

   BACKGROUND

   A rectangulation tiles a rectangle with interior-disjoint rectangles.
   Two tiles are *adjacent* when they share a boundary segment of positive
   length (point contact excluded, following Eppstein et al. 2009).

   Tile a *blocks* tile b (written a ◁ b) when they are adjacent and a is
   to the left of or below b.  The *adjacency poset* P_a is the partial
   order on tiles defined as the transitive closure of ◁.

   Asinowski, Cardinal, Felsner & Fusy ("Combinatorics of rectangulations",
   2024, Proposition 9) prove that P_a is a planar 2-dimensional lattice.
   "2-dimensional" means it is exactly the intersection of two total orders:

     a <_P b  iff  order1(a) < order1(b)  AND  order2(a) < order2(b)

   This module computes such a pair of total orders from the geometric
   layout produced by Geom.of_tiling.


   WHY TWO ORDERS, NOT ONE

   A Schroder tree determines a total order on tiles via the lowest
   common ancestor: for any two tiles, the LCA's orientation (H or V)
   tells which is left-of or above the other.  However, P_a is built
   from *adjacency* edges, and two tiles can be tree-ordered without
   being adjacent or transitively connected through adjacency.

   This happens at cross junctions — places where four tiles meet at a
   point.  Consider h(v(0,1), v(2,3)):

       +---+---+
       | 0 | 1 |     adjacency edges: 0-1, 2-3, 0-2, 1-3
       +---+---+     (no diagonal: 0-3 and 1-2 share only a point)
       | 2 | 3 |
       +---+---+

   The tree says 0 is above 3 (via the root H-frame), but there is no
   adjacency chain from 0 to 3 or from 3 to 0.  They are *incomparable*
   in P_a.  The Hasse diagram is a diamond:

           1
          / \
         0   3        order1: 2 < 0 < 3 < 1  (left-right priority)
          \ /         order2: 2 < 3 < 0 < 1  (bottom-top priority)
           2

   Each order alone is a valid linear extension of P_a, but they
   disagree on the incomparable pair {0, 3}: order1 puts 0 first
   (more to the left), order2 puts 3 first (more to the bottom).
   Their intersection correctly recovers the diamond.


   GEOMETRY DEPENDENCE

   resolve_splits breaks cross junctions by nudging coincident cuts
   apart, which adds a short shared boundary to one diagonal pair.
   This collapses the diamond into a total order — but which total
   order depends on the split positions.  When the geometry is updated
   dynamically (user-driven segment sliding):

   - Near-degenerate: one diagonal has a tiny shared boundary, P_a
     is a total order, both orders agree.
   - At the exact cross: no diagonal is adjacent, P_a has a diamond,
     the two orders diverge on the incomparable pair.
   - Past the cross: the OTHER diagonal gets the boundary, P_a is a
     different total order.

   The two-order representation tracks this transition smoothly.


   COVERING RELATIONS vs ADJACENCY EDGES

   The covering relations of P_a (its Hasse diagram) are a *subset* of
   the adjacency edges.  When a < c < b in the poset (via transitivity
   through c), the adjacency edge a-b is not a covering relation even
   though a and b are geometrically adjacent.  This is standard: the
   adjacency edges *generate* P_a, but the covering relations are only
   those not implied by transitivity.


   ALGORITHM

   1. Orient each edge in Geom.t.adjacency using rectangle coordinates
      to determine which tile is left-of or below the other.

   2. Run Kahn's topological sort twice on the resulting DAG, with
      different tiebreakers for choosing among tiles with zero in-degree:
      - order1: prefer smallest x-center  (leftmost first)
      - order2: prefer largest y-center   (bottommost first)

   3. Store as two IntMap.t rank maps (tile_id -> position in the
      total order).  All data structures are immutable.

   For guillotine tilings with all cross junctions resolved (as
   produced by resolve_splits), both orders are identical — the
   tiebreaker is never consulted because exactly one tile has zero
   in-degree at each step.  The two orders diverge only at unresolved
   cross junctions, which arise under user-controlled split positions.


   VERIFICATION

   poset_check.ml verifies exhaustively for all tilings up to n=8
   (10,879 tilings) that:
   A. Every geometric adjacency edge is comparable in the poset.
   B. Every covering relation of the poset is a geometric edge.
   C. No two tiles share the same (rank1, rank2) pair.
   D. A unique minimum and maximum exist.

   Checks A and B together imply the poset equals P_a: coverings
   generate the same transitive closure as the adjacency edges.

   The tiebreaker correctness (that the two topological sorts produce
   a valid realizer for all guillotine tilings) is verified
   exhaustively but not yet proven.


   REFERENCES

   - Asinowski, Cardinal, Felsner, Fusy. "Combinatorics of
     rectangulations: Old and new bijections." 2024.  Proposition 9:
     P_a is a planar 2-dimensional lattice.  Section 4.1: the strong
     poset and its special relations.

   - Eppstein, Mumford, Speckmann, Verbeek. "Area-Universal and
     Constrained Rectangular Layouts." SIAM J. Computing, 2012.
     Point contact excluded from adjacency; adjacency depends on
     split ratios in non-one-sided layouts. *)

module IntMap = Map.Make(Int)

type t = {
  order1 : int IntMap.t;  (* tile_id -> rank in left-right-priority extension *)
  order2 : int IntMap.t;  (* tile_id -> rank in bottom-top-priority extension *)
  n : int;                (* number of tiles *)
}

(* Orient an unoriented adjacency edge (a, b) into (lo, hi) where
   lo < hi in the poset.  The poset direction is: left-of or below
   means smaller.  Screen coordinates: y=0 is top, y=1 is bottom.

   Uses the same epsilon (1e-9) as Geom.compute_adjacency. *)
let orient_edge rects (a, b) =
  let ar = List.assoc a rects
  and br = List.assoc b rects in
  let eps = 1e-9 in
  if abs_float ((ar.Geom.x +. ar.Geom.w) -. br.Geom.x) < eps then
    (* a's right edge = b's left edge: a is left of b, a < b *)
    (a, b)
  else if abs_float ((br.Geom.x +. br.Geom.w) -. ar.Geom.x) < eps then
    (* b's right edge = a's left edge: b is left of a, b < a *)
    (b, a)
  else if abs_float ((ar.Geom.y +. ar.Geom.h) -. br.Geom.y) < eps then
    (* a's bottom edge = b's top edge: a is above b, b is below a, b < a *)
    (b, a)
  else
    (* b's bottom edge = a's top edge: b is above a, a is below b, a < b *)
    (a, b)

(* Build a DAG from oriented adjacency edges.
   Returns (successors, in_degrees, tile_list) as IntMaps. *)
let build_dag (g : Geom.t) =
  let tiles = List.map fst g.rects in
  let empty_succ =
    List.fold_left (fun m n -> IntMap.add n [] m) IntMap.empty tiles in
  let empty_indeg =
    List.fold_left (fun m n -> IntMap.add n 0 m) IntMap.empty tiles in
  let succ, indeg =
    List.fold_left (fun (s, d) edge ->
      let lo, hi = orient_edge g.rects edge in
      let s' = IntMap.update lo (function
        | None -> Some [hi] | Some l -> Some (hi :: l)) s in
      let d' = IntMap.update hi (function
        | None -> Some 1 | Some k -> Some (k + 1)) d in
      (s', d')
    ) (empty_succ, empty_indeg) g.adjacency
  in
  (succ, indeg, tiles)

(* Pick the element with lowest priority value from a non-empty list.
   Returns (best, remaining).  O(n). *)
let pick_min priority = function
  | [] -> failwith "Poset.pick_min: empty"
  | first :: rest ->
    List.fold_left (fun (best, others) x ->
      if compare (priority x) (priority best) < 0
      then (x, best :: others)
      else (best, x :: others)
    ) (first, []) rest

(* Kahn's topological sort with priority tiebreaker.  When multiple
   tiles have zero in-degree, the one with the lowest priority value
   is emitted first.  Purely functional: all state in IntMap.
   O(n^2) — adequate for window manager tile counts. *)
let topo_sort successors in_degrees priority tiles =
  let ready =
    List.filter (fun n -> IntMap.find n in_degrees = 0) tiles in
  let rec loop ready indeg acc =
    match ready with
    | [] -> List.rev acc
    | _ ->
      let tile, rest_ready = pick_min priority ready in
      let succs = match IntMap.find_opt tile successors with
        | Some l -> l | None -> [] in
      let indeg', newly_ready =
        List.fold_left (fun (d, nr) s ->
          let old = IntMap.find s d in
          let d' = IntMap.add s (old - 1) d in
          if old - 1 = 0 then (d', s :: nr) else (d', nr)
        ) (indeg, []) succs
      in
      loop (newly_ready @ rest_ready) indeg' (tile :: acc)
  in
  loop ready in_degrees []

let to_rank_map lst =
  let m, _ =
    List.fold_left (fun (m, rank) id ->
      (IntMap.add id rank m, rank + 1)
    ) (IntMap.empty, 0) lst
  in
  m

(* Compute the two-order encoding of P_a from a geometric layout. *)
let of_geom (g : Geom.t) =
  let n = List.length g.rects in
  if n <= 1 then
    let order = match g.rects with
      | [(id, _)] -> IntMap.singleton id 0
      | _ -> IntMap.empty
    in
    { order1 = order; order2 = order; n }
  else
    let succ, indeg, tiles = build_dag g in
    let priority_lr id =
      let r = List.assoc id g.rects in
      r.Geom.x +. r.Geom.w /. 2.
    in
    let priority_bt id =
      let r = List.assoc id g.rects in
      -. (r.Geom.y +. r.Geom.h /. 2.)
    in
    let order1_list = topo_sort succ indeg priority_lr tiles in
    let order2_list = topo_sort succ indeg priority_bt tiles in
    { order1 = to_rank_map order1_list;
      order2 = to_rank_map order2_list;
      n }

(* Compare two tiles in the poset.
   Returns Some (-1) if a < b, Some 1 if b < a, Some 0 if a = b,
   None if incomparable. *)
let compare p a b =
  if a = b then Some 0
  else
    let r1a = IntMap.find a p.order1 and r1b = IntMap.find b p.order1 in
    let r2a = IntMap.find a p.order2 and r2b = IntMap.find b p.order2 in
    if r1a < r1b && r2a < r2b then Some (-1)
    else if r1a > r1b && r2a > r2b then Some 1
    else None

(* Is (a, b) a covering relation?  a covers b iff a < b and no tile c
   satisfies a < c < b.  O(n) per call. *)
let is_covering p a b =
  match compare p a b with
  | Some (-1) ->
    let r1a = IntMap.find a p.order1 and r1b = IntMap.find b p.order1 in
    let r2a = IntMap.find a p.order2 and r2b = IntMap.find b p.order2 in
    not (IntMap.exists (fun c _ ->
      c <> a && c <> b &&
      let r1c = IntMap.find c p.order1 in
      let r2c = IntMap.find c p.order2 in
      r1a < r1c && r1c < r1b && r2a < r2c && r2c < r2b
    ) p.order1)
  | _ -> false

(* All covering pairs (a, b) with a < b, sorted.  These are the
   Hasse diagram edges — a subset of the geometric adjacency edges. *)
let coverings p =
  IntMap.fold (fun a _ acc ->
    IntMap.fold (fun b _ acc ->
      if is_covering p a b then (min a b, max a b) :: acc else acc
    ) p.order1 acc
  ) p.order1 []
  |> List.sort_uniq Stdlib.compare

(* Unique minimum (bottom-left tile) and maximum (top-right tile).
   Guaranteed to exist for the adjacency poset of a rectangulation,
   which is a lattice. *)
let minimum p =
  IntMap.fold (fun id _ found ->
    match found with
    | Some _ -> found
    | None ->
      if IntMap.find id p.order1 = 0 && IntMap.find id p.order2 = 0
      then Some id else None
  ) p.order1 None

let maximum p =
  let last = p.n - 1 in
  IntMap.fold (fun id _ found ->
    match found with
    | Some _ -> found
    | None ->
      if IntMap.find id p.order1 = last && IntMap.find id p.order2 = last
      then Some id else None
  ) p.order1 None
