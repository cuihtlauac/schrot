
let label_tiling (is_h, unit_tree) =
  let counter = ref 0 in
  let rec label = function
    | Schrot.Tile () -> let k = !counter in incr counter; Schrot.Tile k
    | Schrot.Frame ch -> Schrot.Frame (List2.map label ch)
  in
  ((is_h, label unit_tree) : Tiling.t)

let () =
  let max_leaves = ref 10 in
  Arg.parse [
    "--max-leaves", Arg.Set_int max_leaves, "N Max leaves (default: 10)";
  ] (fun _ -> ()) "conjecture_check [--max-leaves N]";
  for n = 1 to !max_leaves do
    let tilings = Schrot.enum n in
    let total = List.length tilings in
    Printf.printf "n=%d: %d tilings\n%!" n total;
    (* Single pass: collect one representative per D4 orbit *)
    let tbl : ((int * int list) list,
               (string * (int * int) list) list) Hashtbl.t =
      Hashtbl.create (total / 4) in
    let n_orbits = ref 0 in
    let checked = ref 0 in
    List.iter (fun unit_tiling ->
      incr checked;
      if !checked mod 50000 = 0 then
        Printf.printf "  enumerated %d/%d\n%!" !checked total;
      let labeled = label_tiling unit_tiling in
      let self_str = Tiling.to_string labeled in
      let canon = Tiling.canonical_d4 labeled in
      (* Is this tiling the canonical representative of its orbit? *)
      (* canonical_d4 returns a string of the erased canonical form.
         We need to compare with erased self. *)
      let self_erased =
        let (is_h, tree) = Tiling.erase labeled in
        Tiling.unit_tree_to_string is_h tree
      in
      if canon = self_erased then begin
        incr n_orbits;
        let edges = Geom.edges (Geom.of_tiling labeled) in
        let fp = Tiling.adjacency_fingerprint n edges in
        let prev = try Hashtbl.find tbl fp with Not_found -> [] in
        Hashtbl.replace tbl fp ((self_str, edges) :: prev)
      end
    ) tilings;
    (* Check fingerprint collisions *)
    let n_collisions = ref 0 in
    let max_group = ref 0 in
    let counterexample = ref false in
    Hashtbl.iter (fun _fp members ->
      let sz = List.length members in
      if sz > !max_group then max_group := sz;
      if sz > 1 then begin
        incr n_collisions;
        (* Check pairwise isomorphism *)
        let arr = Array.of_list members in
        for i = 0 to Array.length arr - 1 do
          for j = i + 1 to Array.length arr - 1 do
            let (s1, e1) = arr.(i) in
            let (s2, e2) = arr.(j) in
            if Tiling.graphs_isomorphic n e1 e2 then begin
              counterexample := true;
              Printf.printf "  COUNTEREXAMPLE: %s and %s have isomorphic adjacency graphs!\n"
                s1 s2
            end
          done
        done
      end
    ) tbl;
    Printf.printf "  %d orbits, %d fingerprint collisions (max group: %d)%s\n%!"
      !n_orbits !n_collisions !max_group
      (if !counterexample then " *** CONJECTURE DISPROVED ***"
       else " -- conjecture holds")
  done
