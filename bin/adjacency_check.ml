open Nachum

let adjacency_to_string = function
  | Tiling.North -> "N" | Tiling.South -> "S"
  | Tiling.East -> "E" | Tiling.West -> "W"

let compare_adj (d1, n1) (d2, n2) =
  let c = compare d1 d2 in
  if c <> 0 then c else compare n1 n2

let check_tiling tiling =
  let tiles = List.sort compare (Tiling.leaves tiling) in
  let bounds = Tiling.tabstop_extract tiling in
  let errors = ref 0 in
  List.iter (fun a ->
    let lca_nb = List.sort compare_adj (Tiling.lca_neighbors a tiling) in
    let tab_nb = List.sort compare_adj (Tiling.tabstop_neighbors a bounds tiling) in
    if lca_nb <> tab_nb then begin
      incr errors;
      Printf.printf "  MISMATCH tile %d in %s\n" a (Tiling.to_string tiling);
      Printf.printf "    LCA:    [%s]\n"
        (String.concat "; " (List.map (fun (d, m) ->
          Printf.sprintf "%s:%d" (adjacency_to_string d) m) lca_nb));
      Printf.printf "    Tabstop:[%s]\n"
        (String.concat "; " (List.map (fun (d, m) ->
          Printf.sprintf "%s:%d" (adjacency_to_string d) m) tab_nb))
    end
  ) tiles;
  !errors

let () =
  let max_leaves = ref 5 in
  Arg.parse [
    "--max-leaves", Arg.Set_int max_leaves, "N Max leaves (default: 5)";
  ] (fun _ -> ()) "adjacency_check [--max-leaves N]";
  let total_tilings = ref 0 in
  let total_pairs = ref 0 in
  let total_errors = ref 0 in
  for n = 1 to !max_leaves do
    let tilings = Schrot.enum n in
    let count = List.length tilings in
    Printf.printf "n=%d: %d tilings\n%!" n count;
    List.iteri (fun i (is_h, unit_tree) ->
      let counter = ref 0 in
      let rec label = function
        | Schrot.Tile () -> let k = !counter in incr counter; Schrot.Tile k
        | Schrot.Frame ch -> Schrot.Frame (List2.map label ch)
      in
      let tiling : Tiling.t = (is_h, label unit_tree) in
      let errs = check_tiling tiling in
      total_errors := !total_errors + errs;
      total_pairs := !total_pairs + n * (n - 1) / 2;
      incr total_tilings;
      if (i + 1) mod 100 = 0 then
        Printf.printf "  checked %d/%d\n%!" (i + 1) count
    ) tilings
  done;
  Printf.printf "\nTotal: %d tilings, %d pair-checks, %d errors\n"
    !total_tilings !total_pairs !total_errors;
  if !total_errors = 0 then
    Printf.printf "PASS: LCA-interval and tabstop methods agree.\n"
  else
    Printf.printf "FAIL: %d mismatches found.\n" !total_errors
