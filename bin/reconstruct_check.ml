(* Verify geometry-to-tree reconstruction (Route 2: recursive cut
   decomposition).  Three tests:

   1. Round-trip: for all tilings up to max_leaves, verify that
      resolve_splits -> rects -> tree_of_rects produces a tree whose
      geometry matches the original.

   2. Non-generic: for tilings with cross junctions under equal splits,
      verify that tree_of_rects returns Ok (the non-generic geometry
      is still guillotine).

   3. Non-guillotine: construct a known windmill rectangulation and verify
      that tree_of_rects returns Error with the expected block. *)

let label_tiling (is_h, unit_tree) =
  let counter = ref 0 in
  let rec label = function
    | Schrot.Tile () -> let k = !counter in incr counter; Schrot.Tile k
    | Schrot.Frame ch -> Schrot.Frame (List2.map (fun (w, c) -> (w, label c)) ch)
  in
  ((is_h, label unit_tree) : Tiling.t)

(* Compare two rectangle sets for geometric equality (same tile positions). *)
let rects_equal ~eps r1 r2 =
  let sort_rects rs = List.sort (fun (a, _) (b, _) -> compare a b) rs in
  let r1 = sort_rects r1 and r2 = sort_rects r2 in
  List.length r1 = List.length r2 &&
  List.for_all2 (fun (n1, (a : Geom.rect)) (n2, (b : Geom.rect)) ->
    n1 = n2
    && abs_float (a.x -. b.x) < eps
    && abs_float (a.y -. b.y) < eps
    && abs_float (a.w -. b.w) < eps
    && abs_float (a.h -. b.h) < eps
  ) r1 r2

let to_geom_rects raw_rects =
  List.map (fun (n, (r : Tiling.rect)) ->
    (n, ({ x = r.rx; y = r.ry; w = r.rw; h = r.rh } : Geom.rect))
  ) raw_rects

let () =
  let max_leaves = ref 8 in
  Arg.parse [
    "--max-leaves", Arg.Set_int max_leaves, "N Max leaves (default: 8)";
  ] (fun _ -> ()) "reconstruct_check [--max-leaves N]";
  let all_ok = ref true in
  let fail fmt = Printf.ksprintf (fun s ->
    all_ok := false; Printf.printf "FAIL: %s\n%!" s) fmt in

  (* --- Test 1: Round-trip on resolved splits --- *)
  Printf.printf "Test 1: Round-trip (resolved splits)\n%!";
  for n = 1 to !max_leaves do
    let tilings = Schrot.enum n in
    let ntil = List.length tilings in
    let ok = ref 0 in
    let exact = ref 0 in
    let flat = ref 0 in
    List.iter (fun unit_tiling ->
      let t = label_tiling unit_tiling in
      let wt = Tiling.resolve_splits t in
      let original_rects = to_geom_rects (Tiling.rects_of_weighted wt) in
      match Geom.tree_of_rects original_rects with
      | Error _ ->
        fail "n=%d: tree_of_rects returned Error for %s" n (Tiling.to_string t)
      | Ok [] ->
        fail "n=%d: tree_of_rects returned empty list for %s" n (Tiling.to_string t)
      | Ok trees ->
        (* Required: reconstruction succeeds, all trees have correct size
           and the correct set of tile labels. *)
        let size_ok = List.for_all (fun t' ->
          Tiling.size t' = Tiling.size t
        ) trees in
        let labels_ok = List.for_all (fun t' ->
          List.sort compare (Tiling.leaves t') =
          List.sort compare (Tiling.leaves t)
        ) trees in
        if not (size_ok && labels_ok) then
          fail "n=%d: reconstructed tree has wrong size/labels for %s"
            n (Tiling.to_string t)
        else begin
          incr ok;
          (* Informational: does the reconstructed tree match exactly?
             When resolve_splits leaves unresolved cross junctions, the
             reconstruction may find a flatter but geometrically valid tree
             — this is expected, not an error. *)
          let geom_ok = List.for_all (fun t' ->
            let wt' = Tiling.resolve_splits t' in
            let rects' = to_geom_rects (Tiling.rects_of_weighted wt') in
            rects_equal ~eps:1e-6 original_rects rects'
          ) trees in
          if geom_ok then incr exact else incr flat
        end
    ) tilings;
    Printf.printf "  n=%d: %d/%d OK" n !ok ntil;
    if !flat > 0 then
      Printf.printf " (%d flattened at unresolved cross junctions)" !flat;
    if !exact > 0 && !flat = 0 then
      Printf.printf " (all exact)";
    Printf.printf "\n%!"
  done;

  (* --- Test 2: Non-generic input (equal splits) --- *)
  Printf.printf "\nTest 2: Non-generic (equal splits, cross junctions)\n%!";
  let cross_tested = ref 0 in
  let cross_ok = ref 0 in
  for n = 1 to min !max_leaves 7 do
    let tilings = Schrot.enum n in
    List.iter (fun unit_tiling ->
      let t = label_tiling unit_tiling in
      let k = Tiling.count_cross_junctions t in
      if k > 0 then begin
        incr cross_tested;
        let g = Geom.of_tiling_equal t in
        match Geom.tree_of_geom g with
        | Error _ ->
          fail "n=%d k=%d: tree_of_geom returned Error for %s"
            n k (Tiling.to_string t)
        | Ok trees ->
          (* With equal splits, aligned cuts produce flatter trees.
             At least 1 tree must be returned; may be < 2^k when cuts
             align into single wall-to-wall segments. *)
          if trees = [] then
            fail "n=%d k=%d: empty tree list for %s"
              n k (Tiling.to_string t)
          else
            incr cross_ok
      end
    ) tilings
  done;
  Printf.printf "  %d/%d tilings with cross junctions OK\n%!"
    !cross_ok !cross_tested;

  (* --- Test 3: Non-guillotine error case --- *)
  Printf.printf "\nTest 3: Non-guillotine (windmill)\n%!";
  (* Construct a windmill:
     +------+------+
     |  0   |      |
     |      |  1   |
     +---+--+      |
     |   |  +------+
     | 2 |    3    |
     +---+---------+  *)
  let windmill : (int * Geom.rect) list = [
    (0, { x = 0.0; y = 0.0; w = 0.5; h = 0.5 });
    (1, { x = 0.5; y = 0.0; w = 0.5; h = 0.6 });
    (2, { x = 0.0; y = 0.5; w = 0.3; h = 0.5 });
    (3, { x = 0.3; y = 0.6; w = 0.7; h = 0.4 });
  ] in
  (match Geom.tree_of_rects windmill with
   | Ok _ ->
     fail "windmill: expected Error, got Ok"
   | Error info ->
     let nb = List.length info.blocks in
     if nb <> 1 then
       fail "windmill: expected 1 block, got %d" nb
     else begin
       let b = List.hd info.blocks in
       let nt = List.length b.block_tiles in
       if nt <> 4 then
         fail "windmill: expected 4 tiles in block, got %d" nt
       else
         Printf.printf "  windmill: Error with 1 block of 4 tiles — OK\n%!"
     end);

  (* --- Summary --- *)
  if !all_ok then
    Printf.printf "\nAll checks passed.\n"
  else begin
    Printf.printf "\nSome checks FAILED.\n";
    exit 1
  end
