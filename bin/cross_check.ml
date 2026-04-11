
let label_tiling (is_h, unit_tree) =
  let counter = ref 0 in
  let rec label = function
    | Schrot.Tile () -> let k = !counter in incr counter; Schrot.Tile k
    | Schrot.Frame ch -> Schrot.Frame (List2.map (fun (w, c) -> (w, label c)) ch)
  in
  ((is_h, label unit_tree) : Tiling.t)

let point_to_string (p : Tiling.rational_point) =
  Printf.sprintf "(%d/%d, %d/%d)" p.px p.pd p.qx p.qd

let () =
  let max_leaves = ref 8 in
  Arg.parse [
    "--max-leaves", Arg.Set_int max_leaves, "N Max leaves (default: 8)";
  ] (fun _ -> ()) "cross_check [--max-leaves N]";
  let total_tilings = ref 0 in
  let total_with_degen = ref 0 in
  let total_mismatches = ref 0 in
  for n = 1 to !max_leaves do
    let tilings = Schrot.enum n in
    let count = List.length tilings in
    let n_with_degen = ref 0 in
    let n_mismatches = ref 0 in
    List.iter (fun unit_tiling ->
      let t = label_tiling unit_tiling in
      let corners = Tiling.degenerate_corners t in
      let cuts = Tiling.degenerate_cuts t in
      let corner_points =
        List.map (fun (p, _) -> Tiling.normalize_rpoint p)
          corners |> List.sort Tiling.compare_rpoint in
      let cut_points =
        List.map Tiling.normalize_rpoint cuts
        |> List.sort_uniq Tiling.compare_rpoint in
      let agree = List.length corner_points = List.length cut_points
        && List.for_all2 (fun a b -> Tiling.compare_rpoint a b = 0)
             corner_points cut_points in
      if not agree then begin
        incr n_mismatches;
        Printf.printf "  MISMATCH: %s\n" (Tiling.to_string t);
        Printf.printf "    corners: [%s]\n"
          (String.concat "; " (List.map (fun (p, m) ->
            Printf.sprintf "%s×%d" (point_to_string p) m) corners));
        Printf.printf "    cuts:    [%s]\n"
          (String.concat "; " (List.map point_to_string cut_points))
      end;
      if corners <> [] then incr n_with_degen
    ) tilings;
    total_tilings := !total_tilings + count;
    total_with_degen := !total_with_degen + !n_with_degen;
    total_mismatches := !total_mismatches + !n_mismatches;
    Printf.printf "n=%d: %d tilings, %d with degenerate vertices, %d mismatches%s\n%!"
      n count !n_with_degen !n_mismatches
      (if !n_mismatches = 0 then "" else " *** DISAGREEMENT ***")
  done;
  Printf.printf "\nTotal: %d tilings, %d with degenerate vertices, %d mismatches%s\n"
    !total_tilings !total_with_degen !total_mismatches
    (if !total_mismatches = 0 then " -- approaches agree"
     else " *** APPROACHES DISAGREE ***")
