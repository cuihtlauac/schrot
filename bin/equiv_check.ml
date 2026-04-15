
let label_tiling (is_h, unit_tree) =
  let counter = ref 0 in
  let rec label = function
    | Schrot.Tile () -> let k = !counter in incr counter; Schrot.Tile k
    | Schrot.Frame ch -> Schrot.Frame (List2.map (fun (w, c) -> (w, label c)) ch)
  in
  ((is_h, label unit_tree) : Tiling.t)

let () =
  let max_leaves = ref 7 in
  Arg.parse [
    "--max-leaves", Arg.Set_int max_leaves, "N Max leaves (default: 7)";
  ] (fun _ -> ()) "equiv_check [--max-leaves N]";
  let total_tilings = ref 0 in
  let total_failures = ref 0 in
  for n = 1 to !max_leaves do
    let tilings = Schrot.enum n in
    let count = List.length tilings in
    let n_failures = ref 0 in
    let labeled = List.map label_tiling tilings in
    (* Pre-compute geometries *)
    let resolved = List.map (fun t -> (t, Geom.of_tiling t)) labeled in
    let equal = List.map (fun t -> (t, Geom.of_tiling_equal t)) labeled in

    List.iter (fun (t, g) ->
      (* Check A: of_tiling always produces generic geometry *)
      if not (Geom.is_generic g) then begin
        incr n_failures;
        Printf.printf "  FAIL A: %s of_tiling not generic\n" (Tiling.to_string t)
      end
    ) resolved;

    List.iter (fun (t, g) ->
      (* Check B: of_tiling_equal is generic iff no cross junctions *)
      let crosses = Tiling.count_cross_junctions t in
      let generic = Geom.is_generic g in
      if generic <> (crosses = 0) then begin
        incr n_failures;
        Printf.printf "  FAIL B: %s crosses=%d, is_generic=%b\n"
          (Tiling.to_string t) crosses generic
      end
    ) equal;

    List.iter (fun (t, g) ->
      (* Check C: weak_equivalent reflexivity *)
      if not (Geom.weak_equivalent g g) then begin
        incr n_failures;
        Printf.printf "  FAIL C: %s weak_equivalent not reflexive\n"
          (Tiling.to_string t)
      end
    ) resolved;

    List.iter (fun (t, g) ->
      (* Check D: strong_equivalent reflexivity *)
      if not (Geom.strong_equivalent g g) then begin
        incr n_failures;
        Printf.printf "  FAIL D: %s strong_equivalent not reflexive\n"
          (Tiling.to_string t)
      end
    ) resolved;

    (* Check E: different tree shapes imply not weakly equivalent.
       Group tilings by erased tree shape, pick one from each group,
       check all cross-group pairs. *)
    if n >= 2 then begin
      let shape_groups = Hashtbl.create 16 in
      List.iter (fun (t, g) ->
        let (is_h, tree) = Tiling.erase t in
        let key = Tiling.unit_tree_to_string is_h tree in
        if not (Hashtbl.mem shape_groups key) then
          Hashtbl.add shape_groups key (t, g)
      ) resolved;
      let reps = Hashtbl.fold (fun _ v acc -> v :: acc) shape_groups [] in
      let rec check = function
        | [] -> ()
        | (t1, g1) :: rest ->
          List.iter (fun (t2, g2) ->
            if Geom.weak_equivalent g1 g2 then begin
              incr n_failures;
              Printf.printf "  FAIL E: %s and %s different shapes but weak_equivalent\n"
                (Tiling.to_string t1) (Tiling.to_string t2)
            end
          ) rest;
          check rest
      in
      check reps
    end;

    total_tilings := !total_tilings + count;
    total_failures := !total_failures + !n_failures;
    Printf.printf "n=%d: %d tilings, %d failures%s\n%!"
      n count !n_failures
      (if !n_failures = 0 then "" else " ***")
  done;
  Printf.printf "\nTotal: %d tilings, %d failures%s\n"
    !total_tilings !total_failures
    (if !total_failures = 0 then " -- all checks pass" else " *** FAILURES ***")
