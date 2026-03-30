open Nachum

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
  ] (fun _ -> ()) "geom_vs_lca_check [--max-leaves N]";
  let total_tilings = ref 0 in
  let all_agree = ref 0 in
  let lca_tab_disagree = ref 0 in
  let lca_geom_only = ref 0 in
  let tab_geom_only = ref 0 in
  let geom_only = ref 0 in
  let lca_tab_only = ref 0 in
  for n = 1 to !max_leaves do
    let tilings = Schrot.enum n in
    Printf.printf "n=%d: %d tilings\n%!" n (List.length tilings);
    List.iter (fun unit_tiling ->
      incr total_tilings;
      let tiling = label_tiling unit_tiling in
      let lca = edge_set (Tiling.adjacency_graph tiling) in
      let bounds = Tiling.tabstop_extract tiling in
      let tab_edges = ref [] in
      List.iter (fun (a, _) ->
        List.iter (fun (b, _) ->
          if a < b then
            match Tiling.tabstop_adjacent a b bounds tiling with
            | Some _ -> tab_edges := (a, b) :: !tab_edges
            | None -> ()
        ) bounds
      ) bounds;
      let tab = edge_set !tab_edges in
      let g = Geom.of_tiling tiling in
      let geom = edge_set (List.map (fun (a, b, _) -> (a, b))
        (Geom.edges g)) in
      if lca = tab && tab = geom then
        incr all_agree
      else begin
        let in_lca e = List.mem e lca in
        let in_tab e = List.mem e tab in
        let in_geom e = List.mem e geom in
        let all_edges = List.sort_uniq compare (lca @ tab @ geom) in
        let diffs = List.filter (fun e ->
          not (in_lca e && in_tab e && in_geom e)
        ) all_edges in
        Printf.printf "  %s:\n" (Tiling.to_string tiling);
        List.iter (fun (a, b) ->
          let l = in_lca (a,b) and t = in_tab (a,b) and g = in_geom (a,b) in
          let tag = Printf.sprintf "%s%s%s"
            (if l then "LCA " else "    ")
            (if t then "TAB " else "    ")
            (if g then "GEOM" else "    ") in
          Printf.printf "    %d-%d: %s\n" a b tag;
          if l && t && not g then incr lca_tab_only;
          if l && not t && g then incr lca_geom_only;
          if not l && t && g then incr tab_geom_only;
          if not l && not t && g then incr geom_only;
          if l && not t && not g then incr lca_geom_only; (* lca only *)
          if not l && t && not g then incr tab_geom_only; (* tab only *)
          if l <> t then incr lca_tab_disagree
        ) diffs
      end
    ) tilings
  done;
  Printf.printf "\nTotal: %d tilings, %d all-agree\n" !total_tilings !all_agree;
  Printf.printf "Disagreements: LCA!=TAB: %d, LCA+TAB not GEOM: %d, \
                 GEOM only: %d, LCA+GEOM not TAB: %d, TAB+GEOM not LCA: %d\n"
    !lca_tab_disagree !lca_tab_only !geom_only !lca_geom_only !tab_geom_only
