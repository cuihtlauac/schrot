(* Model checking for the 3 atomic flip operations.
   Properties verified under D4 orbit reduction:
   A. Size preservation
   B. Validity (well-formed Schroder tiling)
   C. Invertibility (hard failure)
   D. Flip graph connectivity (on the D4 quotient graph) *)

let label_tiling (is_h, tree) =
  let counter = ref 0 in
  let rec go = function
    | Schrot.Tile () -> let n = !counter in incr counter; Schrot.Tile n
    | Schrot.Frame ch -> Schrot.Frame (List2.map (fun (w, c) -> (w, go c)) ch)
  in
  (is_h, go tree)

let is_valid_labeling t =
  let lvs = Tiling.leaves t in
  let n = List.length lvs in
  let sorted = List.sort compare lvs in
  sorted = List.init n Fun.id

let rec tree_valid = function
  | Schrot.Tile _ -> true
  | Schrot.Frame ch ->
    List2.length ch >= 2 && List2.for_all (fun (_, c) -> tree_valid c) ch

let tiling_valid (_, tree) = tree_valid tree && is_valid_labeling (false, tree)

let check_properties max_n =
  let all_ok = ref true in
  let counterexamples = Buffer.create 256 in
  let add_cx prop tiling_str flip_str =
    Printf.bprintf counterexamples "  (%S, %S, %S);\n" tiling_str flip_str prop
  in
  let fail fmt = Printf.ksprintf (fun s ->
    all_ok := false; Printf.printf "FAIL: %s\n%!" s) fmt in
  for n = 1 to max_n do
    let unit_tilings = Schrot.enum n in
    let all_tilings = List.map label_tiling unit_tilings in
    let ntil = List.length all_tilings in
    (* Group by D4 orbit: canonical_d4 key -> (orbit_index, representative) *)
    let orbit_tbl : (string, int) Hashtbl.t = Hashtbl.create 64 in
    let orbits = ref [] in
    let norbit = ref 0 in
    let tiling_to_orbit = Hashtbl.create 64 in
    List.iter (fun t ->
      let key = Tiling.canonical_d4 t in
      if not (Hashtbl.mem orbit_tbl key) then begin
        let idx = !norbit in
        incr norbit;
        Hashtbl.replace orbit_tbl key idx;
        orbits := (idx, t, key) :: !orbits
      end;
      let orbit_idx = Hashtbl.find orbit_tbl key in
      Hashtbl.replace tiling_to_orbit (Tiling.to_string t) orbit_idx
    ) all_tilings;
    let norbits = !norbit in
    let orbit_reps = Array.make norbits (List.hd all_tilings) in
    List.iter (fun (idx, t, _) -> orbit_reps.(idx) <- t) !orbits;
    Printf.printf "n=%d: %d tilings, %d D4 orbits\n%!" n ntil norbits;
    if n <= 1 then begin
      Printf.printf "  (no flips for n=%d)\n%!" n;
    end else begin
      (* Check A, B, C on orbit representatives only.
         Build D4 quotient graph for D. *)
      let orbit_adj = Array.make norbits [] in
      let total_flips = ref 0 in
      let prop_a_ok = ref true in
      let prop_b_ok = ref true in
      let prop_c_ok = ref true in
      let prop_c_count = ref 0 in
      let prop_c_fail_count = ref 0 in
      Array.iteri (fun oi t ->
        let flips = Tiling.enumerate_flips t in
        List.iter (fun (flip, t') ->
          incr total_flips;
          (* Property A *)
          if Tiling.size t' <> n then begin
            fail "n=%d orbit %d: %s changed size to %d"
              n oi (Tiling.flip_to_string flip) (Tiling.size t');
            add_cx "A" (Tiling.to_string t) (Tiling.flip_to_string flip);
            prop_a_ok := false
          end;
          (* Property B *)
          if not (tiling_valid t') then begin
            fail "n=%d orbit %d: %s produced invalid tiling %s"
              n oi (Tiling.flip_to_string flip) (Tiling.to_string t');
            add_cx "B" (Tiling.to_string t) (Tiling.flip_to_string flip);
            prop_b_ok := false
          end;
          (* Property D: map result to orbit, record edge *)
          let result_key = Tiling.to_string t' in
          let result_orbit =
            match Hashtbl.find_opt tiling_to_orbit result_key with
            | Some idx -> idx
            | None ->
              let t'_rel = Tiling.relabel t' in
              let key2 = Tiling.to_string t'_rel in
              (match Hashtbl.find_opt tiling_to_orbit key2 with
               | Some idx -> idx
               | None ->
                 fail "n=%d orbit %d: %s produced unknown tiling %s"
                   n oi (Tiling.flip_to_string flip) result_key;
                 oi)
          in
          if not (List.mem result_orbit orbit_adj.(oi)) then
            orbit_adj.(oi) <- result_orbit :: orbit_adj.(oi);
          (* Property C: invertibility *)
          let reverse_flips = Tiling.enumerate_flips t' in
          let original_str = Tiling.to_string t in
          let found_inverse = List.exists (fun (_, t'') ->
            Tiling.to_string t'' = original_str
          ) reverse_flips in
          if found_inverse then incr prop_c_count
          else begin
            let t'_rel = Tiling.relabel t' in
            let t_rel = Tiling.relabel t in
            let rev_rel = Tiling.enumerate_flips t'_rel in
            let found_rel = List.exists (fun (_, t'') ->
              Tiling.to_string t'' = Tiling.to_string t_rel
            ) rev_rel in
            if found_rel then incr prop_c_count
            else begin
              if !prop_c_fail_count < 5 then
                fail "n=%d orbit %d: %s -> %s has no inverse"
                  n oi (Tiling.flip_to_string flip) (Tiling.to_string t');
              add_cx "C" (Tiling.to_string t) (Tiling.flip_to_string flip);
              prop_c_ok := false;
              incr prop_c_fail_count
            end
          end
        ) flips
      ) orbit_reps;
      Printf.printf "  %d flip applications (on %d reps)\n%!" !total_flips norbits;
      if !prop_a_ok then Printf.printf "  Property A (size preservation): OK\n%!";
      if !prop_b_ok then Printf.printf "  Property B (validity): OK\n%!";
      if !prop_c_ok then
        Printf.printf "  Property C (invertibility): OK (%d/%d)\n%!"
          !prop_c_count !prop_c_count
      else
        Printf.printf "  Property C (invertibility): FAIL (%d/%d, %d failures)\n%!"
          !prop_c_count (!prop_c_count + !prop_c_fail_count) !prop_c_fail_count;
      (* Property D: connectivity of the D4 quotient graph.
         Theorem 19 (Merino-Mütze 2021, arXiv:2103.09333v2) proves a
         Hamilton cycle exists on the flip graph for guillotine
         rectangulations, implying connectivity for all n.  This
         empirical check covers n <= max_n. *)
      let visited = Array.make norbits false in
      let queue = Queue.create () in
      Queue.push 0 queue;
      visited.(0) <- true;
      let count = ref 1 in
      while not (Queue.is_empty queue) do
        let v = Queue.pop queue in
        List.iter (fun w ->
          if not visited.(w) then begin
            visited.(w) <- true;
            incr count;
            Queue.push w queue
          end
        ) orbit_adj.(v)
      done;
      if !count = norbits then
        Printf.printf "  Property D (connectivity): OK (%d/%d orbits reachable)\n%!" !count norbits
      else begin
        fail "n=%d: quotient graph not connected: %d/%d orbits reachable" n !count norbits;
        Array.iteri (fun i v ->
          if not v then
            Printf.printf "    unreachable orbit %d: %s\n" i
              (Tiling.to_string orbit_reps.(i))
        ) visited
      end
    end
  done;
  if Buffer.length counterexamples > 0 then begin
    Printf.printf "\n--- Counterexamples (for flip_unit.ml) ---\n";
    Printf.printf "let cases = [\n%s]\n" (Buffer.contents counterexamples)
  end;
  if !all_ok then
    Printf.printf "\nAll properties verified.\n"
  else begin
    Printf.printf "\nSome properties FAILED.\n";
    exit 1
  end

let () =
  let max_n = ref 6 in
  Arg.parse [
    "--max-leaves", Arg.Set_int max_n, "Maximum number of leaves (default: 6)";
  ] (fun _ -> ()) "flip_check [--max-leaves N]";
  check_properties !max_n
