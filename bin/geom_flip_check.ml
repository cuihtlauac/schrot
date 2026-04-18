(* Pure-geometric invertibility diagnostic.

   See /home/cuihtlauac/.claude/plans/dynamic-sparking-platypus.md for the
   framing.  For every labeled tree at n <= max_leaves, lower to
   rectangles (equal splits or irrational weights), enumerate every valid
   forward T-flip, apply it, search the post-flip geometry for a reverse
   T-flip that restores the original rects.

   Per failure we record:
   - whether pre/post geometry is generic (no 4-way vertices), via
     Geom.is_generic, to test whether forward flips create cross junctions;
   - whether the filter bypass (enumerate every (t_joint, side) without
     subwall_simplicity) finds a reverse, to isolate whether failures are
     caused by the simplicity filter. *)

let label_tiling (is_h, tree) =
  let counter = ref 0 in
  let rec go = function
    | Schrot.Tile () -> let n = !counter in incr counter; Schrot.Tile n
    | Schrot.Frame ch -> Schrot.Frame (List2.map (fun (w, c) -> (w, go c)) ch)
  in
  (is_h, go tree)

(* Weight every Frame child with 1 + k*(sqrt 2 - 1) where k is a global
   preorder counter.  Weights are in 1 + Z*(sqrt 2 - 1), which is a Z-basis
   of Z + Z*sqrt 2, so no partial-sum ratios coincide.  Guarantees
   [Geom.is_generic] on the lowered geometry. *)
let weight_tiling (is_h, tree) =
  let counter = ref 0 in
  let next_w () =
    let w = 1.0 +. float_of_int !counter *. (sqrt 2.0 -. 1.0) in
    incr counter; w
  in
  let rec go : (int, unit) Schrot.t -> (int, float) Schrot.t = function
    | Schrot.Tile n -> Schrot.Tile n
    | Schrot.Frame ch ->
      Schrot.Frame (List2.map (fun ((), c) -> (next_w (), go c)) ch)
  in
  (is_h, go tree)

let rects_equal ?(eps = 1e-7) rs1 rs2 =
  let sort = List.sort (fun (i, _) (j, _) -> compare i j) in
  let s1 = sort rs1 and s2 = sort rs2 in
  List.length s1 = List.length s2
  && List.for_all2 (fun (i, (r1 : Geom.rect)) (j, (r2 : Geom.rect)) ->
       i = j
       && abs_float (r1.x -. r2.x) < eps
       && abs_float (r1.y -. r2.y) < eps
       && abs_float (r1.w -. r2.w) < eps
       && abs_float (r1.h -. r2.h) < eps) s1 s2

(* Filter-passing candidates: (joint, side) with simple sub-wall. *)
let filter_candidates g =
  let sc = Geom.subwall_simplicity g in
  List.concat_map (fun (j, lo_simple, hi_simple) ->
    (if lo_simple then [(j, Geom.Lo)] else [])
    @ (if hi_simple then [(j, Geom.Hi)] else [])
  ) sc

(* All T-joint x side candidates, bypassing simplicity. *)
let all_candidates g =
  List.concat_map (fun j -> [(j, Geom.Lo); (j, Geom.Hi)]) (Geom.t_joints g)

(* Reuse pre-flip Geom.t with post-flip rects: tiling/adjacency are unused
   by t_joints / subwall_simplicity (both read only rects, verified in
   lib/geom.ml:153,192). *)
let geom_of_rects original rects = { original with Geom.rects }

let joint_str (j : Geom.t_joint) side =
  let side_str = match side with Geom.Lo -> "Lo" | Geom.Hi -> "Hi" in
  Printf.sprintf "joint@(%.4f,%.4f) bar=%d stems=(%d,%d) %s %s"
    j.jx j.jy j.bar_tile (fst j.stem_tiles) (snd j.stem_tiles)
    (if j.through_h then "H" else "V") side_str

let lower ~generic t =
  if generic then Geom.of_weighted (weight_tiling t) t
  else Geom.of_tiling t

let check ~generic max_n =
  let all_ok = ref true in
  Printf.printf "Mode: %s\n%!"
    (if generic then "generic (irrational weights)" else "equal splits");
  for n = 2 to max_n do
    let unit_tilings = Schrot.enum n in
    let all_tilings = List.map label_tiling unit_tilings in
    let ntil = List.length all_tilings in
    let orbit_tbl : (string, unit) Hashtbl.t = Hashtbl.create 64 in
    let orbit_reps = List.filter (fun t ->
      let key = Tiling.canonical_d4 t in
      if Hashtbl.mem orbit_tbl key then false
      else (Hashtbl.add orbit_tbl key (); true)
    ) all_tilings in
    let norbits = List.length orbit_reps in
    let flips_total = ref 0 in
    let fwd_none = ref 0 in
    let invertible = ref 0 in
    let failures = ref [] in
    let pre_non_generic = ref 0 in
    let post_non_generic_on_fail = ref 0 in
    let rescued_by_bypass = ref 0 in
    let genuine_failures = ref 0 in
    List.iter (fun t ->
      let g = lower ~generic t in
      let original = g.Geom.rects in
      if not (Geom.is_generic g) then incr pre_non_generic;
      let candidates = filter_candidates g in
      List.iter (fun (joint, side) ->
        incr flips_total;
        match Geom.apply_t_flip joint side g with
        | None -> incr fwd_none
        | Some post_rects ->
          let g' = geom_of_rects g post_rects in
          let rev_candidates = filter_candidates g' in
          let found = List.exists (fun (j', s') ->
            match Geom.apply_t_flip j' s' g' with
            | None -> false
            | Some back -> rects_equal back original
          ) rev_candidates in
          if found then incr invertible
          else begin
            let post_gen = Geom.is_generic g' in
            if not post_gen then incr post_non_generic_on_fail;
            let bypass = all_candidates g' in
            let rescued = List.exists (fun (j', s') ->
              match Geom.apply_t_flip j' s' g' with
              | None -> false
              | Some back -> rects_equal back original
            ) bypass in
            if rescued then incr rescued_by_bypass
            else incr genuine_failures;
            if List.length !failures < 5 then
              failures := (t, joint, side, post_gen, rescued) :: !failures
          end
      ) candidates
    ) orbit_reps;
    let nfail = !flips_total - !invertible - !fwd_none in
    Printf.printf
      "n=%d: %d tilings, %d D4 orbits, %d flips (%d fwd-invalid, %d pre-non-generic orbits)\n%!"
      n ntil norbits !flips_total !fwd_none !pre_non_generic;
    if nfail = 0 then
      Printf.printf "  invertible: %d/%d  OK\n%!" !invertible !invertible
    else begin
      all_ok := false;
      Printf.printf
        "  invertible: %d/%d  FAIL (%d failures: %d non-generic-post, %d rescued-by-bypass, %d genuine)\n%!"
        !invertible (!invertible + nfail) nfail
        !post_non_generic_on_fail !rescued_by_bypass !genuine_failures;
      List.iter (fun (t, j, s, post_gen, rescued) ->
        Printf.printf "    cex: %s  %s  [post_gen=%b rescued=%b]\n"
          (Tiling.to_string t) (joint_str j s) post_gen rescued
      ) (List.rev !failures)
    end
  done;
  if !all_ok then Printf.printf "\nAll geometric flips invertible.\n"
  else begin
    Printf.printf "\nSome geometric flips are NOT invertible.\n";
    exit 1
  end

let () =
  let max_n = ref 6 in
  let generic = ref false in
  Arg.parse [
    "--max-leaves", Arg.Set_int max_n, "Maximum number of leaves (default: 6)";
    "--generic", Arg.Set generic, "Use irrational weights so geometry is always generic";
  ] (fun _ -> ()) "geom_flip_check [--max-leaves N] [--generic]";
  check ~generic:!generic !max_n
