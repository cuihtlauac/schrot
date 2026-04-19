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
    | Wall_slide (a, b) -> Tiling.wall_slide a b
    | T_flip (stem, bar) -> fun t -> Geom.t_flip ~stem ~bar t
  in
  assert_invertible_fn name t apply

(* --- Test cases --- *)

(* n=2: simple flip at root *)
let t_h01 : Tiling.t = (true, Schrot.unit_frame (List2.Cons2 (Tile 0, Tile 1, [])))

(* n=3: all 6 tilings cover the basic operations *)
let t_h012 : Tiling.t = (true, Schrot.unit_frame (List2.Cons2 (Tile 0, Tile 1, [Tile 2])))
let t_h_v01_2 : Tiling.t = (true, Schrot.unit_frame (List2.Cons2 (
  Schrot.unit_frame (List2.Cons2 (Tile 0, Tile 1, [])), Tile 2, [])))

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
  (* T-flip via Geom.t_flip (Asinowski pivoting) *)
  assert_invertible "t_flip_0_2" t_h_v01_2 (T_flip (0, 2));
  assert_invertible "t_flip_1_2" t_h_v01_2 (T_flip (1, 2));
  (* Summary *)
  if !failures = 0 then
    Printf.printf "\nAll unit tests passed.\n"
  else begin
    Printf.printf "\n%d unit test(s) FAILED.\n" !failures;
    exit 1
  end
