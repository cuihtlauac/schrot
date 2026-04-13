
let label_tiling (is_h, unit_tree) =
  let counter = ref 0 in
  let rec label = function
    | Schrot.Tile () -> let k = !counter in incr counter; Schrot.Tile k
    | Schrot.Frame ch -> Schrot.Frame (List2.map (fun (w, c) -> (w, label c)) ch)
  in
  ((is_h, label unit_tree) : Tiling.t)

let pair a b = if a < b then (a, b) else (b, a)

let () =
  let max_leaves = ref 7 in
  Arg.parse [
    "--max-leaves", Arg.Set_int max_leaves, "N Max leaves (default: 7)";
  ] (fun _ -> ()) "tjoint_check [--max-leaves N]";
  let total_tilings = ref 0 in
  let total_joints = ref 0 in
  let total_failures = ref 0 in
  for n = 1 to !max_leaves do
    let tilings = Schrot.enum n in
    let count = List.length tilings in
    let n_joints = ref 0 in
    let n_failures = ref 0 in
    List.iter (fun unit_tiling ->
      let t = label_tiling unit_tiling in
      let g = Geom.of_tiling t in
      let joints = Geom.t_joints g in
      n_joints := !n_joints + List.length joints;

      (* Check A: adjacency consistency *)
      List.iter (fun (j : Geom.t_joint) ->
        let bar = j.bar_tile in
        let (s1, s2) = j.stem_tiles in
        let adj = Geom.edges g in
        if not (List.mem (pair s1 s2) adj) then begin
          incr n_failures;
          Printf.printf "  FAIL A: %s joint at (%.6f,%.6f): stem %d,%d not adjacent\n"
            (Tiling.to_string t) j.jx j.jy s1 s2
        end;
        if not (List.mem (pair bar s1) adj) then begin
          incr n_failures;
          Printf.printf "  FAIL A: %s joint at (%.6f,%.6f): bar %d not adjacent to stem %d\n"
            (Tiling.to_string t) j.jx j.jy bar s1
        end;
        if not (List.mem (pair bar s2) adj) then begin
          incr n_failures;
          Printf.printf "  FAIL A: %s joint at (%.6f,%.6f): bar %d not adjacent to stem %d\n"
            (Tiling.to_string t) j.jx j.jy bar s2
        end
      ) joints;

      (* Check B: for tilings without cross junctions, T-joint count
         should match multiplicity-3 count from degenerate_corners *)
      let crosses = Tiling.count_cross_junctions t in
      if crosses = 0 then begin
        let degen = Tiling.degenerate_corners t in
        let mult3 = List.length (List.filter (fun (_, m) -> m = 3) degen) in
        if List.length joints <> mult3 then begin
          incr n_failures;
          Printf.printf "  FAIL B: %s no crosses, t_joints=%d vs degenerate_mult3=%d\n"
            (Tiling.to_string t) (List.length joints) mult3
        end
      end;

      (* Check C: subwall simplicity invariants *)
      let sw = Geom.subwall_simplicity g in
      (* C1: total simple sub-walls = 2 * number of through-wall groups.
         Each group has exactly one lo-extreme, so count lo=true entries. *)
      let total_simple = List.fold_left (fun acc (_, lo, hi) ->
        acc + (if lo then 1 else 0) + (if hi then 1 else 0)) 0 sw in
      let group_count = List.length (List.filter (fun (_, lo, _) -> lo) sw) in
      if group_count > 0 && total_simple <> 2 * group_count then begin
        incr n_failures;
        Printf.printf "  FAIL C1: %s simple=%d, 2*groups=%d\n"
          (Tiling.to_string t) total_simple (2 * group_count)
      end;
      (* C2: for n <= 3, all sub-walls should be simple *)
      if n <= 3 then
        List.iter (fun (j, lo, hi) ->
          if not (lo && hi) then begin
            incr n_failures;
            Printf.printf "  FAIL C2: %s n=%d joint at (%.6f,%.6f) not all-simple\n"
              (Tiling.to_string t) n j.Geom.jx j.Geom.jy
          end
        ) sw
    ) tilings;
    total_tilings := !total_tilings + count;
    total_joints := !total_joints + !n_joints;
    total_failures := !total_failures + !n_failures;
    Printf.printf "n=%d: %d tilings, %d T-joints, %d failures%s\n%!"
      n count !n_joints !n_failures
      (if !n_failures = 0 then "" else " ***")
  done;
  Printf.printf "\nTotal: %d tilings, %d T-joints, %d failures%s\n"
    !total_tilings !total_joints !total_failures
    (if !total_failures = 0 then " -- all checks pass" else " *** FAILURES ***")
