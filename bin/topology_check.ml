open Nachum

let label_tiling (is_h, unit_tree) =
  let counter = ref 0 in
  let rec label = function
    | Schrot.Tile () -> let k = !counter in incr counter; Schrot.Tile k
    | Schrot.Frame ch -> Schrot.Frame (List2.map label ch)
  in
  ((is_h, label unit_tree) : Tiling.t)

let () =
  let max_leaves = ref 7 in
  Arg.parse [
    "--max-leaves", Arg.Set_int max_leaves, "N Max leaves (default: 7)";
  ] (fun _ -> ()) "topology_check [--max-leaves N]";
  let total_tilings = ref 0 in
  let total_classes = ref 0 in
  let total_errors = ref 0 in
  for n = 1 to !max_leaves do
    let tilings = Schrot.enum n in
    let count = List.length tilings in
    Printf.printf "n=%d: %d tilings\n%!" n count;
    (* Group by unordered tree shape *)
    let tbl : (string, (bool * unit Schrot.t) list) Hashtbl.t =
      Hashtbl.create 64 in
    List.iter (fun ((_is_h, _tree) as tiling) ->
      let canon = Tiling.canonical_d4 (label_tiling tiling) in
      let prev = try Hashtbl.find tbl canon with Not_found -> [] in
      Hashtbl.replace tbl canon (tiling :: prev)
    ) tilings;
    let classes = Hashtbl.fold (fun canon members acc ->
      (canon, members) :: acc
    ) tbl [] in
    let n_classes = List.length classes in
    Printf.printf "  %d D4 orbit classes\n%!" n_classes;
    if n = 5 then begin
      (* Print orbit of tiling #0 *)
      let t0 = label_tiling (List.hd tilings) in
      Printf.printf "  Orbit of #0 (%s):\n" (Tiling.to_string t0);
      List.iter (fun img ->
        Printf.printf "    %s\n" (Tiling.to_string img)
      ) (Tiling.d4_orbit t0)
    end;
    total_tilings := !total_tilings + count;
    total_classes := !total_classes + n_classes;
    (* For each class, verify all members have isomorphic adjacency graphs *)
    List.iter (fun (_canon, members) ->
      match members with
      | [] | [_] -> ()
      | first :: rest ->
        let t1 = label_tiling first in
        let edges1 = Tiling.tabstop_all_adjacencies t1 in
        List.iter (fun m ->
          let t2 = label_tiling m in
          let edges2 = Tiling.tabstop_all_adjacencies t2 in
          if not (Tiling.graphs_isomorphic n edges1 edges2) then begin
            incr total_errors;
            Printf.printf "  MISMATCH: %s vs %s\n"
              (Tiling.to_string t1) (Tiling.to_string t2)
          end
        ) rest
    ) classes
  done;
  Printf.printf "\nTotal: %d tilings, %d classes, %d errors\n"
    !total_tilings !total_classes !total_errors;
  if !total_errors = 0 then
    Printf.printf "PASS: all tilings in each unordered shape class have isomorphic adjacency graphs.\n"
  else
    Printf.printf "FAIL: %d mismatches found.\n" !total_errors
