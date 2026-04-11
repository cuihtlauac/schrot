
(* Cross-check Hasse diagrams against independent transitive reduction.

   Computes the transitive reduction of the equal-split adjacency DAG
   directly (orient edges from rect coordinates, DFS reachability, remove
   implied edges), then compares with Poset.coverings.  This validates
   the two-order encoding without relying on it.

   Also re-runs poset checks A-D on equal-split geometry. *)

let label_tiling (is_h, unit_tree) =
  let counter = ref 0 in
  let rec label = function
    | Schrot.Tile () -> let k = !counter in incr counter; Schrot.Tile k
    | Schrot.Frame ch -> Schrot.Frame (List2.map (fun (w, c) -> (w, label c)) ch)
  in
  ((is_h, label unit_tree) : Tiling.t)

(* Orient edge (a,b) -> (lo, hi) from rect coordinates.
   Independent of Poset.orient_edge. *)
let orient_edge rects (a, b) =
  let ar = List.assoc a rects and br = List.assoc b rects in
  let eps = 1e-9 in
  if abs_float ((ar.Geom.x +. ar.Geom.w) -. br.Geom.x) < eps then (a, b)
  else if abs_float ((br.Geom.x +. br.Geom.w) -. ar.Geom.x) < eps then (b, a)
  else if abs_float ((ar.Geom.y +. ar.Geom.h) -. br.Geom.y) < eps then (b, a)
  else (a, b)

(* Transitive reduction of a DAG on nodes 0..n-1. *)
let transitive_reduction n oriented_edges =
  let succs = Array.make n [] in
  List.iter (fun (a, b) -> succs.(a) <- b :: succs.(a)) oriented_edges;
  (* DFS reachability from each node *)
  let reach = Array.init n (fun src ->
    let visited = Array.make n false in
    let rec dfs node =
      if not visited.(node) then begin
        visited.(node) <- true;
        List.iter dfs succs.(node)
      end
    in
    dfs src; visited
  ) in
  (* Keep (a,b) iff no intermediate c with a->*c and c->*b *)
  List.filter (fun (a, b) ->
    let dominated = ref false in
    for c = 0 to n - 1 do
      if c <> a && c <> b && reach.(a).(c) && reach.(c).(b) then
        dominated := true
    done;
    not !dominated
  ) oriented_edges

let edge_set edges =
  List.map (fun (a, b) -> (min a b, max a b)) edges
  |> List.sort_uniq compare

let () =
  let max_leaves = ref 7 in
  Arg.parse [
    "--max-leaves", Arg.Set_int max_leaves, "N Max leaves (default: 7)";
  ] (fun _ -> ()) "hasse_check [--max-leaves N]";
  let total = ref 0 in
  let fail_comparable = ref 0 in
  let fail_covering = ref 0 in
  let fail_antisym = ref 0 in
  let fail_extrema = ref 0 in
  let fail_reduction = ref 0 in
  for n = 1 to !max_leaves do
    let tilings = Schrot.enum n in
    Printf.printf "n=%d: %d tilings\n%!" n (List.length tilings);
    List.iter (fun unit_tiling ->
      incr total;
      let t = label_tiling unit_tiling in
      let g = Geom.of_tiling_equal t in
      let p = Poset.of_geom g in
      let name = Tiling.to_string t in
      (* A: every geom edge comparable *)
      List.iter (fun (a, b) ->
        if Poset.compare p a b = None then begin
          incr fail_comparable;
          Printf.printf "  COMPARABLE FAIL %s: %d-%d\n" name a b
        end
      ) g.adjacency;
      (* B: every covering is a geom edge *)
      let geom_es = edge_set g.adjacency in
      let cov_es = edge_set (Poset.coverings p) in
      List.iter (fun e ->
        if not (List.mem e geom_es) then begin
          incr fail_covering;
          Printf.printf "  COVERING FAIL %s: %d-%d\n" name (fst e) (snd e)
        end
      ) cov_es;
      (* C: antisymmetry *)
      if n > 1 then begin
        let pairs = Poset.IntMap.fold (fun id _ acc ->
          (Poset.IntMap.find id p.order1,
           Poset.IntMap.find id p.order2) :: acc
        ) p.order1 [] in
        if List.length (List.sort_uniq compare pairs) <> List.length pairs then begin
          incr fail_antisym;
          Printf.printf "  ANTISYMMETRY FAIL %s\n" name
        end
      end;
      (* D: extrema *)
      if n > 0 && (Poset.minimum p = None || Poset.maximum p = None) then begin
        incr fail_extrema;
        Printf.printf "  EXTREMA FAIL %s\n" name
      end;
      (* E: transitive reduction = coverings *)
      let oriented = List.map (orient_edge g.rects) g.adjacency in
      let red_es = edge_set (transitive_reduction n oriented) in
      if red_es <> cov_es then begin
        incr fail_reduction;
        Printf.printf "  REDUCTION FAIL %s: red=[%s] cov=[%s]\n" name
          (String.concat "; " (List.map (fun (a,b) ->
            Printf.sprintf "%d-%d" a b) red_es))
          (String.concat "; " (List.map (fun (a,b) ->
            Printf.sprintf "%d-%d" a b) cov_es))
      end
    ) tilings
  done;
  Printf.printf "\nTotal: %d tilings\n" !total;
  Printf.printf "Comparable failures: %d\n" !fail_comparable;
  Printf.printf "Covering failures: %d\n" !fail_covering;
  Printf.printf "Antisymmetry failures: %d\n" !fail_antisym;
  Printf.printf "Extrema failures: %d\n" !fail_extrema;
  Printf.printf "Reduction failures: %d\n" !fail_reduction;
  if !fail_comparable + !fail_covering + !fail_antisym
     + !fail_extrema + !fail_reduction = 0 then
    Printf.printf "PASS: Hasse diagrams correct for all tilings.\n"
  else
    Printf.printf "FAIL: see errors above.\n"
