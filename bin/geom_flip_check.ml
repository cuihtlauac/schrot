(* Pure-geometric invertibility proof-of-concept.

   For every Schroder tree at n <= max_leaves, lower to rectangles
   (Geom.of_tiling), enumerate every valid T-flip (t_joint x side, filtered
   by subwall_simplicity per Merino-Mutze Lemma 3), apply it, then search
   the post-flip geometry for any valid T-flip whose application returns
   rectangles coordinate-equal to the pre-flip rectangles.

   No tree reconstruction is involved.  A pass here isolates invertibility
   as a property of the geometric flip primitive alone, independent of the
   tree <-> geometry round-trip that has known bugs (see
   FLIP_INVERTIBILITY.md). *)

let label_tiling (is_h, tree) =
  let counter = ref 0 in
  let rec go = function
    | Schrot.Tile () -> let n = !counter in incr counter; Schrot.Tile n
    | Schrot.Frame ch -> Schrot.Frame (List2.map (fun (w, c) -> (w, go c)) ch)
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

(* A candidate is (joint, side) where the sub-wall on [side] is simple. *)
let valid_candidates g =
  let sc = Geom.subwall_simplicity g in
  List.concat_map (fun (j, lo_simple, hi_simple) ->
    (if lo_simple then [(j, Geom.Lo)] else [])
    @ (if hi_simple then [(j, Geom.Hi)] else [])
  ) sc

(* Candidate enumeration on raw rects: reuse t_joints via a stub Geom.t.
   The [tiling] and [adjacency] fields are unused by [t_joints] and
   [subwall_simplicity]; only [rects] is read. *)
let geom_of_rects original rects = { original with Geom.rects }

let joint_str (j : Geom.t_joint) side =
  let side_str = match side with Geom.Lo -> "Lo" | Geom.Hi -> "Hi" in
  Printf.sprintf "joint@(%.4f,%.4f) bar=%d stems=(%d,%d) %s %s"
    j.jx j.jy j.bar_tile (fst j.stem_tiles) (snd j.stem_tiles)
    (if j.through_h then "H" else "V") side_str

let check max_n =
  let all_ok = ref true in
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
    List.iter (fun t ->
      let g = Geom.of_tiling t in
      let original = g.Geom.rects in
      let candidates = valid_candidates g in
      List.iter (fun (joint, side) ->
        incr flips_total;
        match Geom.apply_t_flip joint side g with
        | None -> incr fwd_none
        | Some post_rects ->
          let g' = geom_of_rects g post_rects in
          let rev_candidates = valid_candidates g' in
          let found = List.exists (fun (j', s') ->
            match Geom.apply_t_flip j' s' g' with
            | None -> false
            | Some back -> rects_equal back original
          ) rev_candidates in
          if found then incr invertible
          else if List.length !failures < 5 then
            failures := (t, joint, side) :: !failures
      ) candidates
    ) orbit_reps;
    let nfail = !flips_total - !invertible - !fwd_none in
    Printf.printf "n=%d: %d tilings, %d D4 orbits, %d flips checked (%d forward-invalid)\n%!"
      n ntil norbits !flips_total !fwd_none;
    if nfail = 0 then
      Printf.printf "  invertible: %d/%d  OK\n%!"
        !invertible (!invertible)
    else begin
      all_ok := false;
      Printf.printf "  invertible: %d/%d  FAIL (%d failures)\n%!"
        !invertible (!invertible + nfail) nfail;
      List.iter (fun (t, j, s) ->
        Printf.printf "    counterexample: %s  %s\n"
          (Tiling.to_string t) (joint_str j s)
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
  Arg.parse [
    "--max-leaves", Arg.Set_int max_n, "Maximum number of leaves (default: 6)";
  ] (fun _ -> ()) "geom_flip_check [--max-leaves N]";
  check !max_n
