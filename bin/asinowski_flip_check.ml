(* Pure-geometric Asinowski-pivoting invertibility check.

   For every labeled tree at n <= max_leaves, lower to rectangles,
   enumerate every (T-joint, side) pair, and classify:

   Forward-flip outcome:
   - rejected_by_subwall   — Geom.subwall_simplicity rejects this side
   - apply_none            — apply_t_flip returns None (shouldn't happen
                              post-subwall, but logged if it does)
   - rejected_by_asinowski — M-M flip succeeds but post-geometry is not a
                              strong (guillotine-generic) rectangulation
                              (= exits the strong poset; Theorem 27)
   - admissible            — subwall-simple AND post is strong

   For each admissible forward flip, three-way reverse-invertibility:
   - invertible_in_asinowski         — a subwall-simple reverse in the
                                       post-geometry restores the pre-rects
                                       (such a reverse is itself Asinowski-
                                       admissible since pre was strong)
   - invertible_only_via_windmill    — no subwall-simple reverse matches,
                                       but some unfiltered reverse does.
                                       Round 5's subwall fix predicts zero
                                       hits here; any hit is a filter or
                                       theorem-reading bug.
   - genuine_failure                 — no reverse matches at all.

   Ground-truth cross-check at n<=7 generic mode (Round 6 oracle):
     n=5: rejected_by_asinowski=1, admissible=67
     n=6: rejected_by_asinowski=19, admissible=333
     n=7: rejected_by_asinowski=150, admissible=1632
   Any deviation from these counts means the Asinowski filter diverges
   from the tflip_oracle/tflip_sym_check path. *)

let label_tiling (is_h, tree) =
  let counter = ref 0 in
  let rec go = function
    | Schrot.Tile () -> let n = !counter in incr counter; Schrot.Tile n
    | Schrot.Frame ch -> Schrot.Frame (List2.map (fun (w, c) -> (w, go c)) ch)
  in
  (is_h, go tree)

(* Irrational weights so Geom.is_generic always holds on the pre-geometry. *)
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

let lower ~generic t =
  if generic then Geom.of_weighted (weight_tiling t) t
  else Geom.of_tiling t

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

let filter_candidates g =
  let sc = Geom.subwall_simplicity g in
  List.concat_map (fun (j, lo_simple, hi_simple) ->
    (if lo_simple then [(j, Geom.Lo)] else [])
    @ (if hi_simple then [(j, Geom.Hi)] else [])) sc

let all_candidates g =
  List.concat_map (fun j -> [(j, Geom.Lo); (j, Geom.Hi)]) (Geom.t_joints g)

let geom_of_rects (original : Geom.t) rects = { original with Geom.rects }

let side_str = function Geom.Lo -> "Lo" | Geom.Hi -> "Hi"
let dir_str h = if h then "H" else "V"

(* --- Forward-flip classification --- *)

type forward_class =
  | Rejected_by_subwall
  | Apply_none
  | Rejected_by_asinowski
  | Admissible of (int * Geom.rect) list  (* post-flip rects *)

let classify_forward joint side lo_simple hi_simple g =
  let simple = match side with Geom.Lo -> lo_simple | Geom.Hi -> hi_simple in
  if not simple then Rejected_by_subwall
  else
    match Geom.apply_t_flip joint side g with
    | None -> Apply_none
    | Some rects ->
      match Geom.tree_of_rects rects with
      | Ok [t'] ->
        let g' = { Geom.tiling = t'; rects; adjacency = [] } in
        if Geom.is_generic g' then Admissible rects
        else Rejected_by_asinowski
      | Ok _ | Error _ -> Rejected_by_asinowski

(* --- Reverse-flip classification (only for admissible forward) --- *)

type reverse_class =
  | Invertible_in_asinowski
  | Invertible_only_via_windmill of (Geom.t_joint * Geom.flip_side)
  | Genuine_failure

(* Find a reverse (j', s') in g' whose apply_t_flip matches original.
   Search: first among subwall-simple candidates, then among all. *)
let classify_reverse original g' =
  let filtered = filter_candidates g' in
  let try_candidate (j', s') =
    match Geom.apply_t_flip j' s' g' with
    | None -> false
    | Some back -> rects_equal back original
  in
  if List.exists try_candidate filtered then Invertible_in_asinowski
  else
    let bypass = all_candidates g' in
    match List.find_opt try_candidate bypass with
    | Some (j', s') -> Invertible_only_via_windmill (j', s')
    | None -> Genuine_failure

(* --- Run stats --- *)

type failure = {
  tiling : Tiling.t;
  forward_joint : Geom.t_joint;
  forward_side : Geom.flip_side;
  pre_rects : (int * Geom.rect) list;
  post_rects : (int * Geom.rect) list;
  reverse : reverse_class;
}

type run_stats = {
  n : int;
  mode : string;
  tilings : int;
  orbits : int;
  joint_sides_total : int;
  rejected_by_subwall : int;
  apply_none : int;
  rejected_by_asinowski : int;
  admissible : int;
  invertible_in_asinowski : int;
  invertible_only_via_windmill : int;
  genuine_failure : int;
  seconds : float;
  failure_details : failure list;  (* invertible_only_via_windmill + genuine *)
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
  let joint_sides_total = ref 0 in
  let rejected_by_subwall = ref 0 in
  let apply_none = ref 0 in
  let rejected_by_asinowski = ref 0 in
  let admissible = ref 0 in
  let invertible_in_asinowski = ref 0 in
  let invertible_only_via_windmill = ref 0 in
  let genuine_failure = ref 0 in
  let failure_details = ref [] in
  let detail_count = ref 0 in
  List.iter (fun t ->
    let g = lower ~generic t in
    let original = g.Geom.rects in
    let joints_with_simple = Geom.subwall_simplicity g in
    List.iter (fun (joint, lo_simple, hi_simple) ->
      List.iter (fun side ->
        incr joint_sides_total;
        match classify_forward joint side lo_simple hi_simple g with
        | Rejected_by_subwall -> incr rejected_by_subwall
        | Apply_none -> incr apply_none
        | Rejected_by_asinowski -> incr rejected_by_asinowski
        | Admissible post_rects ->
          incr admissible;
          let g' = geom_of_rects g post_rects in
          let rc = classify_reverse original g' in
          (match rc with
           | Invertible_in_asinowski -> incr invertible_in_asinowski
           | Invertible_only_via_windmill _ ->
             incr invertible_only_via_windmill;
             if !detail_count < max_failures then begin
               incr detail_count;
               failure_details := {
                 tiling = t;
                 forward_joint = joint;
                 forward_side = side;
                 pre_rects = original;
                 post_rects;
                 reverse = rc;
               } :: !failure_details
             end
           | Genuine_failure ->
             incr genuine_failure;
             if !detail_count < max_failures then begin
               incr detail_count;
               failure_details := {
                 tiling = t;
                 forward_joint = joint;
                 forward_side = side;
                 pre_rects = original;
                 post_rects;
                 reverse = rc;
               } :: !failure_details
             end)
      ) [Geom.Lo; Geom.Hi]
    ) joints_with_simple
  ) orbit_reps;
  {
    n;
    mode = if generic then "generic" else "equal";
    tilings = List.length all_tilings;
    orbits = List.length orbit_reps;
    joint_sides_total = !joint_sides_total;
    rejected_by_subwall = !rejected_by_subwall;
    apply_none = !apply_none;
    rejected_by_asinowski = !rejected_by_asinowski;
    admissible = !admissible;
    invertible_in_asinowski = !invertible_in_asinowski;
    invertible_only_via_windmill = !invertible_only_via_windmill;
    genuine_failure = !genuine_failure;
    seconds = Unix.gettimeofday () -. t_start;
    failure_details = List.rev !failure_details;
  }

(* --- Ground-truth cross-check (Round 6 oracle data, generic mode) --- *)

let expected_rejected_by_asinowski_generic = function
  | 5 -> Some 1
  | 6 -> Some 19
  | 7 -> Some 150
  | _ -> None

let expected_admissible_generic = function
  | 3 -> Some 2
  | 4 -> Some 14
  | 5 -> Some 67
  | 6 -> Some 333
  | 7 -> Some 1632
  | _ -> None

(* --- Printing --- *)

let print_stats s =
  Printf.printf
    "n=%d: %d tilings, %d D4 orbits, %d joint-sides [%.2fs]\n%!"
    s.n s.tilings s.orbits s.joint_sides_total s.seconds;
  Printf.printf
    "  forward: %d rejected_by_subwall, %d apply_none, %d rejected_by_asinowski, %d admissible\n%!"
    s.rejected_by_subwall s.apply_none s.rejected_by_asinowski s.admissible;
  Printf.printf
    "  reverse: %d invertible_in_asinowski, %d invertible_only_via_windmill, %d genuine_failure\n%!"
    s.invertible_in_asinowski s.invertible_only_via_windmill s.genuine_failure;
  if s.mode = "generic" then begin
    (match expected_rejected_by_asinowski_generic s.n with
     | Some exp when exp = s.rejected_by_asinowski ->
       Printf.printf "  oracle: rejected_by_asinowski = %d OK\n%!" exp
     | Some exp ->
       Printf.printf "  oracle: rejected_by_asinowski = %d MISMATCH (expected %d)\n%!"
         s.rejected_by_asinowski exp
     | None -> ());
    (match expected_admissible_generic s.n with
     | Some exp when exp = s.admissible ->
       Printf.printf "  oracle: admissible = %d OK\n%!" exp
     | Some exp ->
       Printf.printf "  oracle: admissible = %d MISMATCH (expected %d)\n%!"
         s.admissible exp
     | None -> ())
  end;
  if s.invertible_only_via_windmill > 0 then
    Printf.printf
      "  ** %d invertible_only_via_windmill — Theorem 27 predicts zero; investigate **\n%!"
      s.invertible_only_via_windmill;
  if s.genuine_failure > 0 then
    Printf.printf "  ** %d genuine_failure **\n%!" s.genuine_failure

(* --- Report writing --- *)

let write_rects_table oc rects =
  let rects = List.sort (fun (i, _) (j, _) -> compare i j) rects in
  Printf.fprintf oc "| tile | x | y | w | h |\n";
  Printf.fprintf oc "|---|---|---|---|---|\n";
  List.iter (fun (i, (r : Geom.rect)) ->
    Printf.fprintf oc "| %d | %.9g | %.9g | %.9g | %.9g |\n"
      i r.x r.y r.w r.h) rects

let write_failure oc k (f : failure) =
  Printf.fprintf oc "\n#### Detail %d\n\n" (k + 1);
  Printf.fprintf oc "- Tiling: `%s`\n" (Tiling.to_string f.tiling);
  Printf.fprintf oc
    "- Forward: joint@(%.9g, %.9g) bar=%d stems=(%d,%d) through=%s side=%s\n"
    f.forward_joint.jx f.forward_joint.jy
    f.forward_joint.bar_tile
    (fst f.forward_joint.stem_tiles) (snd f.forward_joint.stem_tiles)
    (dir_str f.forward_joint.through_h) (side_str f.forward_side);
  (match f.reverse with
   | Invertible_in_asinowski ->
     Printf.fprintf oc "- Reverse: invertible_in_asinowski (should not appear)\n"
   | Invertible_only_via_windmill (j', s') ->
     Printf.fprintf oc
       "- Reverse: invertible_only_via_windmill — bypass joint@(%.9g, %.9g) %s %s\n"
       j'.jx j'.jy (dir_str j'.through_h) (side_str s')
   | Genuine_failure ->
     Printf.fprintf oc "- Reverse: genuine_failure — no reverse matches\n");
  Printf.fprintf oc "\nPre-flip rects:\n\n";
  write_rects_table oc f.pre_rects;
  Printf.fprintf oc "\nPost-flip rects:\n\n";
  write_rects_table oc f.post_rects

let write_report_section oc s =
  Printf.fprintf oc "\n## n=%d mode=%s\n\n" s.n s.mode;
  Printf.fprintf oc
    "| n | mode | orbits | joint-sides | admissible | rejected_by_asinowski | invertible | only-via-windmill | genuine | seconds |\n";
  Printf.fprintf oc "|---|---|---|---|---|---|---|---|---|---|\n";
  Printf.fprintf oc "| %d | %s | %d | %d | %d | %d | %d | %d | %d | %.2f |\n"
    s.n s.mode s.orbits s.joint_sides_total s.admissible
    s.rejected_by_asinowski s.invertible_in_asinowski
    s.invertible_only_via_windmill s.genuine_failure s.seconds;
  Printf.fprintf oc "\n- rejected_by_subwall: %d\n" s.rejected_by_subwall;
  Printf.fprintf oc "- apply_none: %d\n" s.apply_none;
  if s.mode = "generic" then begin
    (match expected_rejected_by_asinowski_generic s.n with
     | Some exp ->
       let tag = if exp = s.rejected_by_asinowski then "OK" else "MISMATCH" in
       Printf.fprintf oc
         "- oracle cross-check: rejected_by_asinowski=%d (expected %d) %s\n"
         s.rejected_by_asinowski exp tag
     | None -> ());
    (match expected_admissible_generic s.n with
     | Some exp ->
       let tag = if exp = s.admissible then "OK" else "MISMATCH" in
       Printf.fprintf oc
         "- oracle cross-check: admissible=%d (expected %d) %s\n"
         s.admissible exp tag
     | None -> ())
  end;
  let all_pass =
    s.apply_none = 0
    && s.invertible_only_via_windmill = 0
    && s.genuine_failure = 0
  in
  if all_pass then
    Printf.fprintf oc
      "\n**PASS** — all %d admissible forward flips invertible_in_asinowski.\n"
      s.admissible
  else begin
    Printf.fprintf oc
      "\n**FAIL** — %d only-via-windmill, %d genuine, %d apply_none.\n"
      s.invertible_only_via_windmill s.genuine_failure s.apply_none;
    if s.failure_details <> [] then begin
      Printf.fprintf oc "\n### Detail (first %d)\n" (List.length s.failure_details);
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
  ] (fun _ -> ()) "asinowski_flip_check [--max-leaves N] [--generic] [--report PATH]";
  Printf.printf "Mode: %s\n%!"
    (if !generic then "generic (irrational weights)" else "equal splits");
  let all_stats = ref [] in
  let all_ok = ref true in
  for n = 2 to !max_n do
    let s = run_check ~generic:!generic ~max_failures:!max_failure_details n in
    print_stats s;
    if s.apply_none > 0
       || s.invertible_only_via_windmill > 0
       || s.genuine_failure > 0
    then all_ok := false;
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
     Printf.fprintf oc
       "| n | joint-sides | admissible | rejected_by_asinowski | invertible | only-via-windmill | genuine | seconds |\n";
     Printf.fprintf oc "|---|---|---|---|---|---|---|---|\n";
     List.iter (fun s ->
       Printf.fprintf oc "| %d | %d | %d | %d | %d | %d | %d | %.2f |\n"
         s.n s.joint_sides_total s.admissible s.rejected_by_asinowski
         s.invertible_in_asinowski s.invertible_only_via_windmill
         s.genuine_failure s.seconds
     ) (List.rev !all_stats);
     close_out oc);
  if !all_ok then
    Printf.printf "\nAll admissible flips invertible_in_asinowski; no windmill-reverse or genuine failures.\n"
  else begin
    Printf.printf "\nFAIL — see stats above.\n";
    exit 1
  end
