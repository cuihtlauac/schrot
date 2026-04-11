
(* Verify the two-permutation encoding of the adjacency poset.

   The adjacency poset P_a is the transitive closure of the blocking
   relation (a ◁ b when a is adjacent to b and a is left-of or below b).
   Its covering relations (Hasse diagram) are a SUBSET of the adjacency
   edges — non-covering edges are implied by transitivity.  For example,
   in h(0, 1, 2), tiles 0 and 2 are adjacent but the edge 0-2 is not a
   covering because 0 < 1 < 2.

   Four checks establish correctness:
   A. Every geometric adjacency edge is comparable in the poset.
   B. Every covering relation of the poset is a geometric edge.
   C. No two tiles share the same (rank1, rank2) pair (antisymmetry).
   D. A unique minimum and maximum exist (lattice property).

   A+B together imply the poset equals P_a: the coverings generate the
   same transitive closure as the adjacency edges. *)

let label_tiling (is_h, unit_tree) =
  let counter = ref 0 in
  let rec label = function
    | Schrot.Tile () -> let k = !counter in incr counter; Schrot.Tile k
    | Schrot.Frame ch -> Schrot.Frame (List2.map (fun (w, c) -> (w, label c)) ch)
  in
  ((is_h, label unit_tree) : Tiling.t)

let edge_set edges =
  List.map (fun (a, b) -> (min a b, max a b)) edges
  |> List.sort_uniq compare

let () =
  let max_leaves = ref 7 in
  Arg.parse [
    "--max-leaves", Arg.Set_int max_leaves, "N Max leaves (default: 7)";
  ] (fun _ -> ()) "poset_check [--max-leaves N]";
  let total_tilings = ref 0 in
  let total_comparable_fail = ref 0 in
  let total_covering_fail = ref 0 in
  let total_antisym_fail = ref 0 in
  let total_extrema_fail = ref 0 in
  for n = 1 to !max_leaves do
    let tilings = Schrot.enum n in
    let count = List.length tilings in
    Printf.printf "n=%d: %d tilings\n%!" n count;
    List.iter (fun unit_tiling ->
      incr total_tilings;
      let t = label_tiling unit_tiling in
      let g = Geom.of_tiling t in
      let p = Poset.of_geom g in
      (* Check A: every geom edge is comparable in the poset *)
      let incomparable = List.filter (fun (a, b) ->
        Poset.compare p a b = None
      ) g.adjacency in
      if incomparable <> [] then begin
        incr total_comparable_fail;
        Printf.printf "  COMPARABLE FAIL for %s: [%s]\n"
          (Tiling.to_string t)
          (String.concat "; " (List.map (fun (a, b) ->
            Printf.sprintf "%d-%d" a b) incomparable))
      end;
      (* Check B: every poset covering is a geom edge *)
      let geom_edges = edge_set g.adjacency in
      let coverings = edge_set (Poset.coverings p) in
      let spurious = List.filter (fun e ->
        not (List.mem e geom_edges)
      ) coverings in
      if spurious <> [] then begin
        incr total_covering_fail;
        Printf.printf "  COVERING FAIL for %s: [%s]\n"
          (Tiling.to_string t)
          (String.concat "; " (List.map (fun (a, b) ->
            Printf.sprintf "%d-%d" a b) spurious))
      end;
      (* Check C: antisymmetry — no two tiles share (rank1, rank2) *)
      if n > 1 then begin
        let pairs = Poset.IntMap.fold (fun id _ acc ->
          let r1 = Poset.IntMap.find id p.order1 in
          let r2 = Poset.IntMap.find id p.order2 in
          (r1, r2) :: acc
        ) p.order1 [] in
        let unique = List.sort_uniq compare pairs in
        if List.length unique <> List.length pairs then begin
          incr total_antisym_fail;
          Printf.printf "  ANTISYMMETRY FAIL for %s\n" (Tiling.to_string t)
        end
      end;
      (* Check D: minimum and maximum exist *)
      if n > 0 then begin
        let has_min = Poset.minimum p <> None in
        let has_max = Poset.maximum p <> None in
        if not (has_min && has_max) then begin
          incr total_extrema_fail;
          Printf.printf "  EXTREMA FAIL for %s (min=%b max=%b)\n"
            (Tiling.to_string t) has_min has_max
        end
      end
    ) tilings
  done;
  Printf.printf "\nTotal: %d tilings\n" !total_tilings;
  Printf.printf "Comparable failures: %d\n" !total_comparable_fail;
  Printf.printf "Covering failures: %d\n" !total_covering_fail;
  Printf.printf "Antisymmetry failures: %d\n" !total_antisym_fail;
  Printf.printf "Extrema failures: %d\n" !total_extrema_fail;
  if !total_comparable_fail = 0 && !total_covering_fail = 0
     && !total_antisym_fail = 0 && !total_extrema_fail = 0 then
    Printf.printf "PASS: poset encoding is correct for all tilings.\n"
  else
    Printf.printf "FAIL: see errors above.\n"
