(* Smoke test for the Checker API *)

(* Property: closing any tile in a tiling with n>=2 tiles gives n-1 tiles *)
let close_decrements_size =
  Checker.for_all_tiles (fun n ->
    Checker.property
      (Printf.sprintf "close(%d) decrements size" n)
      (fun t ->
        let sz = Tiling.size t in
        if sz < 2 then true
        else
          let t' = Tiling.close n t in
          Tiling.size t' = sz - 1))

(* Property: split always increases size by 1 *)
let split_increments_size =
  Checker.for_all_tiles (fun n ->
    Checker.for_all_dirs (fun dir ->
      let dir_t = match dir with
        | Tiling.Left | Tiling.Right -> Tiling.V
        | Tiling.Up | Tiling.Down -> Tiling.H
      in
      Checker.property
        (Printf.sprintf "split(%d) increments size" n)
        (fun t ->
          let t' = Tiling.split n dir_t t in
          Tiling.size t' = Tiling.size t + 1)))

(* A property that should fail: all tilings have size <= 2 *)
let all_small =
  Checker.property "size <= 2" (fun t -> Tiling.size t <= 2)

let () =
  (* Test 1: close decrements size — should pass *)
  let r = Checker.check ~max_tiles:5 ~mode:Progressive close_decrements_size in
  Format.printf "--- close_decrements_size ---@.";
  Format.printf "%a@." Checker.pp_result r;
  assert (Checker.passed r);

  (* Test 2: split increments size — should pass *)
  let r = Checker.check ~max_tiles:5 ~mode:Orbit_representatives split_increments_size in
  Format.printf "--- split_increments_size ---@.";
  Format.printf "%a@." Checker.pp_result r;
  assert (Checker.passed r);

  (* Test 3: all_small — should fail at size 3 *)
  let r = Checker.check ~max_tiles:5 ~policy:Stop_at_first all_small in
  Format.printf "--- all_small (expect failure) ---@.";
  Format.printf "%a@." Checker.pp_result r;
  assert (not (Checker.passed r));
  assert (List.length r.counterexamples = 1);

  (* Test 4: all_small with Collect_all *)
  let r = Checker.check ~max_tiles:4 ~policy:Collect_all all_small in
  Format.printf "--- all_small Collect_all ---@.";
  Format.printf "%a@." Checker.pp_result r;
  assert (not (Checker.passed r));
  assert (List.length r.counterexamples > 1);

  (* Test 5: shrink *)
  let r = Checker.check ~max_tiles:5 ~policy:Stop_at_first all_small in
  let cx = List.hd r.counterexamples in
  let shrunk = Checker.shrink all_small cx in
  Format.printf "--- shrink: size %d -> %d ---@." cx.size shrunk.size;
  assert (shrunk.size = 3);

  (* Test 6: find_witness *)
  let w = Checker.find_witness ~max_tiles:5
      (Checker.property "size >= 3" (fun t -> Tiling.size t >= 3)) in
  (match w with
   | Some t -> Format.printf "--- witness: %s (size %d) ---@." (Tiling.to_string t) (Tiling.size t)
   | None -> assert false);

  (* Test 7: imply — conditional property *)
  let only_big =
    Checker.imply
      (Checker.property "size >= 3" (fun t -> Tiling.size t >= 3))
      (Checker.property "has neighbor"
         (fun t ->
           let g = Geom.of_tiling t in
           Geom.neighbors g 0 <> []))
  in
  let r = Checker.check ~max_tiles:5 only_big in
  Format.printf "--- imply (size>=3 => has neighbor) ---@.";
  Format.printf "%a@." Checker.pp_result r;
  assert (Checker.passed r);
  (* Small tilings should be skipped *)
  let small_skipped = List.fold_left (fun acc (s : Checker.size_stats) -> if s.n < 3 then acc + s.skipped else acc) 0 r.per_size in
  assert (small_skipped > 0);

  Printf.printf "\nAll checker tests passed!\n"
