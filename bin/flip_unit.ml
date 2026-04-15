(* Unit tests for flip operations.
   Add counterexamples from flip_check here as regression tests. *)

let failures = ref 0

let all_flips t =
  let tree_flips = Tiling.enumerate_flips t in
  let g = Geom.of_tiling t in
  let geom_flips = List.map (fun (f, t', _) -> (f, t'))
    (Geom.enumerate_t_flips g) in
  let all = tree_flips @ geom_flips in
  let seen = Hashtbl.create 16 in
  List.filter (fun (_, t') ->
    let key = Tiling.to_string t' in
    if Hashtbl.mem seen key then false
    else (Hashtbl.add seen key (); true)
  ) all

let assert_invertible_fn name t (apply : Tiling.t -> Tiling.t option) =
  match apply t with
  | None ->
    Printf.printf "FAIL %s: flip not applicable\n" name;
    incr failures
  | Some t' ->
    let reverse = all_flips t' in
    let original_str = Tiling.to_string t in
    let ok = List.exists (fun (_, t'') ->
      Tiling.to_string t'' = original_str
    ) reverse in
    if ok then
      Printf.printf "OK   %s\n" name
    else begin
      Printf.printf "FAIL %s: %s -> %s (no inverse among %d flips)\n"
        name (Tiling.to_string t) (Tiling.to_string t') (List.length reverse);
      incr failures
    end

let assert_invertible name t flip =
  let apply = match flip with
    | Tiling.Simple_dissolve n -> Tiling.simple_dissolve n
    | Simple_create (a, b) -> Tiling.simple_create a b
    | Pivot_out n -> Tiling.pivot_out n
    | Pivot_in (n, m) -> Tiling.pivot_in n m
    | Wall_slide (a, b) -> Tiling.wall_slide a b
    | T_flip _ -> fun _ -> None  (* applied via geometry, not tree ops *)
  in
  assert_invertible_fn name t apply

(* --- Test cases --- *)

(* n=2: simple flip at root *)
let t_h01 : Tiling.t = (true, Schrot.unit_frame (List2.Cons2 (Tile 0, Tile 1, [])))

(* n=3: all 6 tilings cover the basic operations *)
let t_h012 : Tiling.t = (true, Schrot.unit_frame (List2.Cons2 (Tile 0, Tile 1, [Tile 2])))
let t_h_v01_2 : Tiling.t = (true, Schrot.unit_frame (List2.Cons2 (
  Schrot.unit_frame (List2.Cons2 (Tile 0, Tile 1, [])), Tile 2, [])))

(* n=3: root boundary extraction targets *)
let t_v210 : Tiling.t = (false, Schrot.unit_frame (List2.Cons2 (Tile 2, Tile 1, [Tile 0])))
let t_v102 : Tiling.t = (false, Schrot.unit_frame (List2.Cons2 (Tile 1, Tile 0, [Tile 2])))

(* n=4: >=3-ary child extraction targets *)
let t_h31_v20 : Tiling.t = (true, Schrot.unit_frame (List2.Cons2 (
  Tile 3, Tile 1, [Schrot.unit_frame (List2.Cons2 (Tile 2, Tile 0, []))])))
let t_v3_h210 : Tiling.t = (false, Schrot.unit_frame (List2.Cons2 (
  Tile 3, Schrot.unit_frame (List2.Cons2 (Tile 2, Tile 1, [Tile 0])), [])))

(* n=4: existing 2-ary extraction (should keep working) *)
let t_h3_v2_h10 : Tiling.t = (true, Schrot.unit_frame (List2.Cons2 (
  Tile 3, Schrot.unit_frame (List2.Cons2 (
    Tile 2, Schrot.unit_frame (List2.Cons2 (Tile 1, Tile 0, [])), [])), [])))

(* n=4 counterexamples: pivot_in merge into ≥3-ary, pivot_out from ≥3-ary root *)
let t_h32_v10 : Tiling.t = (true, Schrot.unit_frame (List2.Cons2 (
  Tile 3, Tile 2, [Schrot.unit_frame (List2.Cons2 (Tile 1, Tile 0, []))])))
let t_h3_v21_0 : Tiling.t = (true, Schrot.unit_frame (List2.Cons2 (
  Tile 3, Schrot.unit_frame (List2.Cons2 (Tile 2, Tile 1, [])), [Tile 0])))

let () =
  (* Simple flip at root *)
  assert_invertible "dissolve_root" t_h01 (Simple_dissolve 0);
  (* Simple create/dissolve *)
  assert_invertible "create_01" t_h012 (Simple_create (0, 1));
  assert_invertible "create_12" t_h012 (Simple_create (1, 2));
  (* Wall slide *)
  assert_invertible "slide_01" t_h012 (Wall_slide (0, 1));
  assert_invertible "slide_12" t_h012 (Wall_slide (1, 2));
  (* Dissolve non-root *)
  assert_invertible "dissolve_v01" t_h_v01_2 (Simple_dissolve 0);
  (* Pivot out from 2-ary *)
  assert_invertible "pivot_out_0" t_h_v01_2 (Pivot_out 0);
  assert_invertible "pivot_out_1" t_h_v01_2 (Pivot_out 1);
  (* Root boundary extraction from >=3-ary root *)
  assert_invertible_fn "root_extract_first_B" t_v210 (Tiling.pivot_out_root 2 Before);
  assert_invertible_fn "root_extract_first_A" t_v210 (Tiling.pivot_out_root 2 After);
  assert_invertible_fn "root_extract_last_B" t_v102 (Tiling.pivot_out_root 2 Before);
  assert_invertible_fn "root_extract_last_A" t_v102 (Tiling.pivot_out_root 2 After);
  (* >=3-ary child Frame extraction *)
  assert_invertible "child3_extract_0" t_h31_v20 (Pivot_out 0);
  assert_invertible "child3_extract_first" t_v3_h210 (Pivot_out 2);
  (* Existing 2-ary extraction still works *)
  assert_invertible "2ary_extract_1" t_h3_v2_h10 (Pivot_out 1);
  assert_invertible "2ary_extract_0" t_h3_v2_h10 (Pivot_out 0);
  (* n=4 counterexamples *)
  assert_invertible "cx4_merge_21" t_h32_v10 (Pivot_in (2, 1));
  assert_invertible_fn "cx4_pout_3_B" t_h3_v21_0 (Tiling.pivot_out_root 3 Before);
  assert_invertible_fn "cx4_pout_3_A" t_h3_v21_0 (Tiling.pivot_out_root 3 After);
  assert_invertible_fn "cx4_pout_0_B" t_h3_v21_0 (Tiling.pivot_out_root 0 Before);
  assert_invertible_fn "cx4_pout_0_A" t_h3_v21_0 (Tiling.pivot_out_root 0 After);
  (* Summary *)
  if !failures = 0 then
    Printf.printf "\nAll unit tests passed.\n"
  else begin
    Printf.printf "\n%d unit test(s) FAILED.\n" !failures;
    exit 1
  end
