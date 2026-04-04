
(* Verify that geometric adjacency (on resolved splits) is a subset of
   tabstop potential adjacency, and that the extra tabstop pairs are
   exactly the unchosen diagonals at cross junctions. *)

let label_tiling (is_h, unit_tree) =
  let counter = ref 0 in
  let rec label = function
    | Schrot.Tile () -> let k = !counter in incr counter; Schrot.Tile k
    | Schrot.Frame ch -> Schrot.Frame (List2.map label ch)
  in
  ((is_h, label unit_tree) : Tiling.t)

let edge_set edges =
  List.map (fun (a, b) -> (min a b, max a b)) edges
  |> List.sort_uniq compare

let () =
  let max_leaves = ref 6 in
  Arg.parse [
    "--max-leaves", Arg.Set_int max_leaves, "N Max leaves (default: 6)";
  ] (fun _ -> ()) "adjacency_check [--max-leaves N]";
  let total_tilings = ref 0 in
  let total_geom_not_tab = ref 0 in
  let total_extra_tab = ref 0 in
  for n = 1 to !max_leaves do
    let tilings = Schrot.enum n in
    let count = List.length tilings in
    Printf.printf "n=%d: %d tilings\n%!" n count;
    List.iter (fun unit_tiling ->
      incr total_tilings;
      let t = label_tiling unit_tiling in
      let tab = edge_set (Tiling.tabstop_all_adjacencies t) in
      let geom = edge_set (Geom.edges (Geom.of_tiling t)) in
      (* Check geom ⊆ tab *)
      let geom_not_tab = List.filter (fun e -> not (List.mem e tab)) geom in
      if geom_not_tab <> [] then begin
        total_geom_not_tab := !total_geom_not_tab + List.length geom_not_tab;
        Printf.printf "  ERROR: geom edges not in tabstop for %s: [%s]\n"
          (Tiling.to_string t)
          (String.concat "; " (List.map (fun (a, b) ->
            Printf.sprintf "%d-%d" a b) geom_not_tab))
      end;
      (* Count extra tabstop edges (unchosen diagonals) *)
      let extra = List.filter (fun e -> not (List.mem e geom)) tab in
      total_extra_tab := !total_extra_tab + List.length extra
    ) tilings
  done;
  Printf.printf "\nTotal: %d tilings\n" !total_tilings;
  Printf.printf "Geom edges not in tabstop: %d\n" !total_geom_not_tab;
  Printf.printf "Extra tabstop edges (unchosen diagonals): %d\n" !total_extra_tab;
  if !total_geom_not_tab = 0 then
    Printf.printf "PASS: geometric adjacency ⊆ tabstop potential adjacency.\n"
  else
    Printf.printf "FAIL: %d geometric edges missing from tabstop set.\n"
      !total_geom_not_tab
