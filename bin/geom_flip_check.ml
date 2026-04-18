(* Pure-geometric invertibility diagnostic.

   For every labeled tree at n <= max_leaves, lower to rectangles
   (equal splits or irrational weights via --generic), enumerate every
   valid forward T-flip (filtered by subwall_simplicity), apply it,
   search the post-flip geometry for a reverse T-flip that restores
   the original rects coordinate-exactly.

   Per failure we record whether pre/post geometry is generic, whether
   the filter bypass finds a reverse, and (for --report) enough detail
   for a future reader to reproduce the counterexample without
   re-running the check. *)

let label_tiling (is_h, tree) =
  let counter = ref 0 in
  let rec go = function
    | Schrot.Tile () -> let n = !counter in incr counter; Schrot.Tile n
    | Schrot.Frame ch -> Schrot.Frame (List2.map (fun (w, c) -> (w, go c)) ch)
  in
  (is_h, go tree)

(* Weights from 1 + Z*(sqrt 2 - 1) = Z-basis of Z + Z*sqrt 2; no
   rational partial-sum coincidences, so [Geom.is_generic] holds. *)
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

let rect_max_diff (a : Geom.rect) (b : Geom.rect) =
  max (abs_float (a.x -. b.x))
    (max (abs_float (a.y -. b.y))
       (max (abs_float (a.w -. b.w)) (abs_float (a.h -. b.h))))

let rects_max_diff rs1 rs2 =
  let sort = List.sort (fun (i, _) (j, _) -> compare i j) in
  let s1 = sort rs1 and s2 = sort rs2 in
  if List.length s1 <> List.length s2 then infinity
  else List.fold_left2 (fun acc (_, r1) (_, r2) ->
    max acc (rect_max_diff r1 r2)) 0.0 s1 s2

let filter_candidates g =
  let sc = Geom.subwall_simplicity g in
  List.concat_map (fun (j, lo_simple, hi_simple) ->
    (if lo_simple then [(j, Geom.Lo)] else [])
    @ (if hi_simple then [(j, Geom.Hi)] else [])) sc

let all_candidates g =
  List.concat_map (fun j -> [(j, Geom.Lo); (j, Geom.Hi)]) (Geom.t_joints g)

let geom_of_rects original rects = { original with Geom.rects }

let joint_str (j : Geom.t_joint) side =
  let side_str = match side with Geom.Lo -> "Lo" | Geom.Hi -> "Hi" in
  Printf.sprintf "joint@(%.4f,%.4f) bar=%d stems=(%d,%d) %s %s"
    j.jx j.jy j.bar_tile (fst j.stem_tiles) (snd j.stem_tiles)
    (if j.through_h then "H" else "V") side_str

let lower ~generic t =
  if generic then Geom.of_weighted (weight_tiling t) t
  else Geom.of_tiling t

(* --- Failure record (populated for the first K failures per run) --- *)

type failure = {
  tiling : Tiling.t;
  forward_joint : Geom.t_joint;
  forward_side : Geom.flip_side;
  pre_rects : (int * Geom.rect) list;
  post_rects : (int * Geom.rect) list;
  pre_filter : (Geom.t_joint * bool * bool) list;
  post_filter : (Geom.t_joint * bool * bool) list;
  post_generic : bool;
  rescued : bool;
  best_bypass : (float * Geom.t_joint * Geom.flip_side) option;
}

let best_bypass_match original g' =
  List.fold_left (fun acc (j', s') ->
    match Geom.apply_t_flip j' s' g' with
    | None -> acc
    | Some back ->
      let d = rects_max_diff back original in
      (match acc with
       | None -> Some (d, j', s')
       | Some (d0, _, _) when d < d0 -> Some (d, j', s')
       | _ -> acc)) None (all_candidates g')

(* --- Report writers --- *)

let side_str = function Geom.Lo -> "Lo" | Geom.Hi -> "Hi"
let dir_str h = if h then "H" else "V"

let write_rects_table oc rects =
  let rects = List.sort (fun (i, _) (j, _) -> compare i j) rects in
  Printf.fprintf oc "| tile | x | y | w | h |\n";
  Printf.fprintf oc "|---|---|---|---|---|\n";
  List.iter (fun (i, (r : Geom.rect)) ->
    Printf.fprintf oc "| %d | %.9g | %.9g | %.9g | %.9g |\n" i r.x r.y r.w r.h) rects

let write_joints_table oc filter =
  Printf.fprintf oc "| pos | wc | through_h | bar | stems | lo_simple | hi_simple |\n";
  Printf.fprintf oc "|---|---|---|---|---|---|---|\n";
  List.iter (fun ((j : Geom.t_joint), lo, hi) ->
    let wc, pos = if j.through_h then (j.jy, j.jx) else (j.jx, j.jy) in
    Printf.fprintf oc "| %.9g | %.9g | %b | %d | (%d,%d) | %b | %b |\n"
      pos wc j.through_h j.bar_tile (fst j.stem_tiles) (snd j.stem_tiles) lo hi) filter

let write_failure oc k (f : failure) =
  Printf.fprintf oc "\n#### Counterexample %d\n\n" (k + 1);
  Printf.fprintf oc "- Tiling: `%s`\n" (Tiling.to_string f.tiling);
  Printf.fprintf oc "- Forward: joint@(%.9g, %.9g) bar=%d stems=(%d,%d) %s %s\n"
    f.forward_joint.jx f.forward_joint.jy
    f.forward_joint.bar_tile
    (fst f.forward_joint.stem_tiles) (snd f.forward_joint.stem_tiles)
    (dir_str f.forward_joint.through_h) (side_str f.forward_side);
  Printf.fprintf oc "- post_generic=%b  rescued=%b\n" f.post_generic f.rescued;
  (match f.best_bypass with
   | Some (d, j, s) ->
     Printf.fprintf oc "- Best bypass match: joint@(%.9g, %.9g) %s %s  (diff=%.3e)\n"
       j.jx j.jy (dir_str j.through_h) (side_str s) d
   | None -> Printf.fprintf oc "- Best bypass match: none\n");
  Printf.fprintf oc "\nPre-flip rects:\n\n";
  write_rects_table oc f.pre_rects;
  Printf.fprintf oc "\nPost-flip rects:\n\n";
  write_rects_table oc f.post_rects;
  Printf.fprintf oc "\nPre-flip T-joints + filter:\n\n";
  write_joints_table oc f.pre_filter;
  Printf.fprintf oc "\nPost-flip T-joints + filter:\n\n";
  write_joints_table oc f.post_filter

(* --- Core check --- *)

type run_stats = {
  n : int;
  mode : string;
  tilings : int;
  orbits : int;
  flips : int;
  fwd_none : int;
  invertible : int;
  failures : int;
  non_generic_post : int;
  rescued_by_bypass : int;
  genuine : int;
  pre_non_generic : int;
  seconds : float;
  failure_details : failure list;  (* capped *)
}

let run_check ~generic ~max_failures n =
  let t_start = Unix.gettimeofday () in
  let unit_tilings = Schrot.enum n in
  let all_tilings = List.map label_tiling unit_tilings in
  let orbit_tbl : (string, unit) Hashtbl.t = Hashtbl.create 64 in
  let orbit_reps = List.filter (fun t ->
    let key = Tiling.canonical_d4 t in
    if Hashtbl.mem orbit_tbl key then false
    else (Hashtbl.add orbit_tbl key (); true)) all_tilings in
  let flips_total = ref 0 in
  let fwd_none = ref 0 in
  let invertible = ref 0 in
  let failures = ref [] in
  let pre_non_generic = ref 0 in
  let post_non_generic_on_fail = ref 0 in
  let rescued_by_bypass = ref 0 in
  let genuine_failures = ref 0 in
  let failure_details = ref [] in
  let detail_count = ref 0 in
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
          | Some back -> rects_equal back original) rev_candidates in
        if found then incr invertible
        else begin
          let post_gen = Geom.is_generic g' in
          if not post_gen then incr post_non_generic_on_fail;
          let bypass = all_candidates g' in
          let rescued = List.exists (fun (j', s') ->
            match Geom.apply_t_flip j' s' g' with
            | None -> false
            | Some back -> rects_equal back original) bypass in
          if rescued then incr rescued_by_bypass
          else incr genuine_failures;
          if List.length !failures < 5 then
            failures := (t, joint, side, post_gen, rescued) :: !failures;
          if !detail_count < max_failures then begin
            incr detail_count;
            let pre_filter = Geom.subwall_simplicity g in
            let post_filter = Geom.subwall_simplicity g' in
            let best = best_bypass_match original g' in
            failure_details := {
              tiling = t;
              forward_joint = joint;
              forward_side = side;
              pre_rects = original;
              post_rects;
              pre_filter;
              post_filter;
              post_generic = post_gen;
              rescued;
              best_bypass = best;
            } :: !failure_details
          end
        end
    ) candidates) orbit_reps;
  let flips = !flips_total in
  let nfail = flips - !invertible - !fwd_none in
  {
    n;
    mode = if generic then "generic" else "equal";
    tilings = List.length all_tilings;
    orbits = List.length orbit_reps;
    flips;
    fwd_none = !fwd_none;
    invertible = !invertible;
    failures = nfail;
    non_generic_post = !post_non_generic_on_fail;
    rescued_by_bypass = !rescued_by_bypass;
    genuine = !genuine_failures;
    pre_non_generic = !pre_non_generic;
    seconds = Unix.gettimeofday () -. t_start;
    failure_details = List.rev !failure_details;
  }

let print_stats s =
  Printf.printf
    "n=%d: %d tilings, %d D4 orbits, %d flips (%d fwd-invalid, %d pre-non-generic orbits) [%.2fs]\n%!"
    s.n s.tilings s.orbits s.flips s.fwd_none s.pre_non_generic s.seconds;
  if s.failures = 0 then
    Printf.printf "  invertible: %d/%d  OK\n%!" s.invertible s.invertible
  else
    Printf.printf
      "  invertible: %d/%d  FAIL (%d failures: %d non-generic-post, %d rescued-by-bypass, %d genuine)\n%!"
      s.invertible (s.invertible + s.failures) s.failures
      s.non_generic_post s.rescued_by_bypass s.genuine

let write_report_section oc s =
  Printf.fprintf oc "\n## n=%d mode=%s\n\n" s.n s.mode;
  Printf.fprintf oc "| n | mode | tilings | orbits | flips | invertible | failures | genuine | seconds |\n";
  Printf.fprintf oc "|---|---|---|---|---|---|---|---|---|\n";
  Printf.fprintf oc "| %d | %s | %d | %d | %d | %d | %d | %d | %.2f |\n"
    s.n s.mode s.tilings s.orbits s.flips s.invertible s.failures s.genuine s.seconds;
  Printf.fprintf oc "\n- fwd-invalid: %d\n" s.fwd_none;
  Printf.fprintf oc "- pre-non-generic orbits: %d\n" s.pre_non_generic;
  Printf.fprintf oc "- non-generic-post (among failures): %d\n" s.non_generic_post;
  Printf.fprintf oc "- rescued-by-bypass (among failures): %d\n" s.rescued_by_bypass;
  if s.failures = 0 then Printf.fprintf oc "\n**PASS** — all %d forward flips invertible.\n" s.invertible
  else begin
    Printf.fprintf oc "\n**FAIL** — %d non-invertible forward flips.\n" s.failures;
    if s.failure_details <> [] then begin
      Printf.fprintf oc "\n### Failure detail (first %d)\n" (List.length s.failure_details);
      List.iteri (write_failure oc) s.failure_details
    end
  end

let () =
  let max_n = ref 6 in
  let generic = ref false in
  let report_path = ref None in
  let max_failure_details = ref 5 in
  Arg.parse [
    "--max-leaves", Arg.Set_int max_n, "Maximum number of leaves (default: 6)";
    "--generic", Arg.Set generic, "Use irrational weights so geometry is always generic";
    "--report", Arg.String (fun s -> report_path := Some s),
      "PATH  Append a structured Markdown section to PATH";
    "--max-failure-details", Arg.Set_int max_failure_details,
      "K  Cap detailed failure records at K per run (default: 5)";
  ] (fun _ -> ()) "geom_flip_check [--max-leaves N] [--generic] [--report PATH]";
  Printf.printf "Mode: %s\n%!"
    (if !generic then "generic (irrational weights)" else "equal splits");
  let all_stats = ref [] in
  let all_ok = ref true in
  for n = 2 to !max_n do
    let s = run_check ~generic:!generic ~max_failures:!max_failure_details n in
    print_stats s;
    if s.failures > 0 then begin
      all_ok := false;
      List.iter (fun (t, j, side, post_gen, rescued) ->
        Printf.printf "    cex: %s  %s  [post_gen=%b rescued=%b]\n"
          (Tiling.to_string t) (joint_str j side) post_gen rescued
      ) (List.rev (List.filteri (fun i _ -> i < 5) s.failure_details
                  |> List.map (fun f ->
                       (f.tiling, f.forward_joint, f.forward_side,
                        f.post_generic, f.rescued))))
    end;
    all_stats := s :: !all_stats;
    match !report_path with
    | None -> ()
    | Some path ->
      let oc = open_out_gen [Open_wronly; Open_creat; Open_append] 0o644 path in
      write_report_section oc s;
      close_out oc
  done;
  (match !report_path with
   | None -> ()
   | Some path ->
     let oc = open_out_gen [Open_wronly; Open_creat; Open_append] 0o644 path in
     Printf.fprintf oc "\n## Run summary (mode=%s)\n\n"
       (if !generic then "generic" else "equal");
     Printf.fprintf oc "| n | flips | invertible | failures | seconds |\n";
     Printf.fprintf oc "|---|---|---|---|---|\n";
     List.iter (fun s ->
       Printf.fprintf oc "| %d | %d | %d | %d | %.2f |\n"
         s.n s.flips s.invertible s.failures s.seconds
     ) (List.rev !all_stats);
     close_out oc);
  if !all_ok then Printf.printf "\nAll geometric flips invertible.\n"
  else begin
    Printf.printf "\nSome geometric flips are NOT invertible.\n";
    exit 1
  end
