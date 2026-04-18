(* Symbolic T-flip candidate + equivalence check vs geometric oracle.

   Candidate rule derived from Phase B oracle classification.

   Let L = LCA of (stem, bar) in t.  bar and stem_branch (the child
   of L on stem's path) must be adjacent siblings in L.  Within
   stem_branch, the "promoted" subtree is the first child (Lo flip)
   or last child (Hi flip), and stem must be a descendant of that
   promoted subtree.  The T-flip rewrites L:

     flip L's direction;
     new L = <new-dir frame> with
       - Lo position = promoted  (if side=Lo)
       - Hi position = promoted  (if side=Hi)
       - the other position = <L's original-dir frame> of (bar,
         rest-of-stem_branch) in the order preserving bar's original
         Lo/Hi relation to stem_branch in L.
     substitute new L for old L in t; flip root is_h iff LCA was the
     root.  Same-orientation nesting is collapsed by the constructor. *)

(* ---------- helpers ---------- *)

let list2_of_list = function
  | a :: b :: rest -> List2.Cons2 (a, b, rest)
  | _ -> failwith "list2_of_list: need >= 2 elements"

let label_tiling (is_h, tree) =
  let counter = ref 0 in
  let rec go = function
    | Schrot.Tile () -> let n = !counter in incr counter; Schrot.Tile n
    | Schrot.Frame ch -> Schrot.Frame (List2.map (fun (w, c) -> (w, go c)) ch)
  in
  (is_h, go tree)

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

let rec path_to_leaf tr tgt =
  match tr with
  | Schrot.Tile m -> if m = tgt then Some [] else None
  | Schrot.Frame ch ->
    let rec scan i = function
      | [] -> None
      | (_, c) :: rest ->
        match path_to_leaf c tgt with
        | Some p -> Some (i :: p)
        | None -> scan (i + 1) rest
    in
    scan 0 (List2.to_list ch)

let rec lcp a b =
  match a, b with
  | x :: xs, y :: ys when x = y -> x :: lcp xs ys
  | _ -> []

(* Construct a Frame at direction [my_is_h] with given children list.
   Inline any child Frame whose root direction equals my_is_h
   (same-orientation collapse).  Each child is paired with its own
   is_h (direction if it's a Frame); Tile children carry the expected
   direction (from context). *)
let build_frame my_is_h (children : (bool * (int, unit) Schrot.t) list) =
  let expanded = List.concat_map (fun (child_is_h, c) ->
    if child_is_h = my_is_h then
      match c with
      | Schrot.Frame ch -> List2.to_list ch |> List.map (fun (_, gc) -> gc)
      | Schrot.Tile _ -> [c]
    else [c]) children in
  match expanded with
  | [] -> failwith "build_frame: empty"
  | [c] -> c
  | lst ->
    let weighted = List.map (fun c -> ((), c)) lst in
    Schrot.Frame (list2_of_list weighted)

let frame_children = function
  | Schrot.Frame ch -> List2.to_list ch
  | _ -> []

let _arity = function
  | Schrot.Frame ch -> List2.length ch
  | _ -> 0

(* ---------- the T-flip rule ---------- *)

type side = Lo | Hi

let lca_rewrite lca_tree lca_is_h ~bar_idx ~stem_idx side stem =
  let ch = frame_children lca_tree in
  let n = List.length ch in
  if bar_idx < 0 || bar_idx >= n || stem_idx < 0 || stem_idx >= n then None
  else if abs (bar_idx - stem_idx) <> 1 then None
  else
    let (_, bar) = List.nth ch bar_idx in
    let (_, stem_branch) = List.nth ch stem_idx in
    let bar_before_stem = bar_idx < stem_idx in
    let stem_branch_is_h = not lca_is_h in
    (* Choose promoted = first child (Lo) or last child (Hi) of stem_branch *)
    let promoted, promoted_is_h, rest_of_stem_branch =
      match stem_branch with
      | Schrot.Tile _ ->
        (* stem_branch is a single tile, which means promoted = tile and
           rest_of_stem_branch = empty.  But then "rest" has nothing to
           combine with bar — there's no valid flip in this
           configuration.  Actually: if stem_branch is a Tile and stem
           matches it, then the flip has no other stem at LCA level.  It
           might be a valid flip with a different LCA level structure;
           we'd need to go up.  For now, reject. *)
        Schrot.Tile (-1), stem_branch_is_h, []   (* unused *)
      | Schrot.Frame sb_ch ->
        let lst = List2.to_list sb_ch in
        let prom_idx = match side with Lo -> 0 | Hi -> List.length lst - 1 in
        let (_, prom) = List.nth lst prom_idx in
        let prom_is_h =
          (* prom's direction: if it's a Frame, the alternating
             invariant says flip(stem_branch_is_h). *)
          not stem_branch_is_h in
        let rest = List.mapi (fun i (w, c) -> (i, (w, c))) lst
                   |> List.filter_map (fun (i, pair) ->
                        if i = prom_idx then None else Some pair) in
        prom, prom_is_h, rest
    in
    (match stem_branch with Schrot.Tile _ -> None
     | _ ->
       if path_to_leaf promoted stem = None then None
       else
         let rest_children = rest_of_stem_branch in
         let rest_collapsed =
           match rest_children with
           | [(_, c)] -> c   (* single remaining: collapse stem_branch *)
           | _ ->
             let weighted = List.map (fun (w, c) -> (w, c)) rest_children in
             Schrot.Frame (list2_of_list weighted)
         in
         (* The direction of rest_collapsed: if >= 2, it's a Frame with
            direction = stem_branch_is_h.  If collapsed to single: it's
            that child, which had direction flip(stem_branch_is_h). *)
         let rest_is_h = match rest_children with
           | [_] ->
             (* single remaining child has direction flip(stem_branch_is_h) *)
             not stem_branch_is_h
           | _ -> stem_branch_is_h
         in
         (* rest_frame = an lca_is_h-direction frame containing bar and rest_collapsed. *)
         let bar_is_h = not lca_is_h in (* bar's direction if it's a Frame *)
         let rest_frame =
           if bar_before_stem then
             build_frame lca_is_h [(bar_is_h, bar); (rest_is_h, rest_collapsed)]
           else
             build_frame lca_is_h [(rest_is_h, rest_collapsed); (bar_is_h, bar)]
         in
         let rest_frame_is_h =
           (* rest_frame's root direction: if it's a Frame built at
              lca_is_h, that's its direction.  If collapsed to single
              child (only possible when build_frame would see 1 child
              after inlining), it's the child's direction.
              We play it safe: assume lca_is_h if it's a Frame, else
              the wrapped single tile's direction (bar or rest).
              In practice, rest_frame has 2 children (bar + rest), so
              it's a Frame at lca_is_h (unless one of them is itself
              an lca_is_h-frame that got inlined, which still keeps
              rest_frame as lca_is_h-frame). *)
           lca_is_h in
         let new_sub_is_h = not lca_is_h in
         let new_sub = match side with
           | Lo ->
             build_frame new_sub_is_h
               [(promoted_is_h, promoted); (rest_frame_is_h, rest_frame)]
           | Hi ->
             build_frame new_sub_is_h
               [(rest_frame_is_h, rest_frame); (promoted_is_h, promoted)]
         in
         (* Replace [min_idx..max_idx] in LCA children with new_sub. *)
         let (lo_idx, hi_idx) = if bar_before_stem
                                then (bar_idx, stem_idx)
                                else (stem_idx, bar_idx) in
         let new_sub_is_h_final = new_sub_is_h in
         let _ = new_sub_is_h_final in
         let merged_list =
           List.mapi (fun i pair -> (i, pair)) ch
           |> List.concat_map (fun (i, (_w, c)) ->
                if i = lo_idx then [(new_sub_is_h, new_sub)]
                else if i = hi_idx then []
                else [(not lca_is_h, c)])
         in
         if List.length merged_list = 1 then
           (* LCA now has a single child; the whole LCA collapses *)
           let (sole_is_h, sole) = List.hd merged_list in
           Some (sole_is_h, sole)
         else
           Some (lca_is_h, build_frame lca_is_h merged_list))

(* Rebuild tree by substituting (sub_is_h, sub) at path, rebuilding
   each frame along the way with build_frame so same-orient collapse
   happens at every level. *)
let rec rebuild_through is_h tree path (sub_is_h, sub) =
  match path with
  | [] -> (sub_is_h, sub)
  | i :: rest ->
    (match tree with
     | Schrot.Frame ch ->
       let lst = List2.to_list ch in
       let child_is_h = not is_h in
       let (_, child_tree) = List.nth lst i in
       let (new_child_is_h, new_child) =
         rebuild_through child_is_h child_tree rest (sub_is_h, sub) in
       let new_children = List.mapi (fun j (_, c) ->
         if j = i then (new_child_is_h, new_child)
         else (child_is_h, c)) lst in
       let new_frame = build_frame is_h new_children in
       let new_is_h = match new_frame with
         | Schrot.Frame _ -> is_h
         | Schrot.Tile _ -> is_h  (* a single-tile collapse keeps parent is_h nominally *)
       in
       (new_is_h, new_frame)
     | Schrot.Tile _ -> failwith "rebuild_through: path into tile")

let rec descend tr p =
  match p, tr with
  | [], _ -> Some tr
  | i :: rest, Schrot.Frame ch ->
    (match List.nth_opt (List2.to_list ch) i with
     | Some (_, c) -> descend c rest
     | None -> None)
  | _, Schrot.Tile _ -> None

let t_flip ~stem ~bar ~side (is_h, tree) =
  match path_to_leaf tree stem, path_to_leaf tree bar with
  | Some sp, Some bp ->
    let lca_path = lcp sp bp in
    let lca_depth = List.length lca_path in
    (match descend tree lca_path with
     | None -> None
     | Some lca_tree ->
       let lca_is_h =
         if lca_depth mod 2 = 0 then is_h else not is_h
       in
       if lca_depth >= List.length bp then None
       else if lca_depth >= List.length sp then None
       else
         let bar_idx = List.nth bp lca_depth in
         let stem_idx = List.nth sp lca_depth in
         (match lca_rewrite lca_tree lca_is_h ~bar_idx ~stem_idx side stem with
          | None -> None
          | Some (new_lca_is_h, new_lca) ->
            let (final_is_h, final_tree) =
              rebuild_through is_h tree lca_path (new_lca_is_h, new_lca) in
            Some (final_is_h, final_tree)))
  | _ -> None

(* ---------- oracle + comparison ---------- *)

let filter_candidates g =
  let sc = Geom.subwall_simplicity g in
  List.concat_map (fun (j, lo_simple, hi_simple) ->
    (if lo_simple then [(j, Geom.Lo)] else [])
    @ (if hi_simple then [(j, Geom.Hi)] else [])) sc

let stem_of (j : Geom.t_joint) side =
  match side with
  | Geom.Lo -> fst j.stem_tiles
  | Geom.Hi -> snd j.stem_tiles

let geom_to_sym_side = function Geom.Lo -> Lo | Geom.Hi -> Hi

let catalog_orbit_reps n =
  let unit_tilings = Schrot.enum n in
  let all_tilings = List.map label_tiling unit_tilings in
  let seen = Hashtbl.create 64 in
  List.filter (fun t ->
    let key = Tiling.canonical_d4 t in
    if Hashtbl.mem seen key then false
    else (Hashtbl.add seen key (); true)) all_tilings

let check_n n =
  let reps = catalog_orbit_reps n in
  let total = ref 0 in
  let match_count = ref 0 in
  let sym_none = ref 0 in
  let mismatch = ref 0 in
  let anomaly = ref 0 in
  let sample_mismatches = ref [] in
  List.iter (fun t ->
    let g = Geom.of_weighted (weight_tiling t) t in
    let candidates = filter_candidates g in
    List.iter (fun (joint, side) ->
      incr total;
      let stem = stem_of joint side in
      let bar = joint.Geom.bar_tile in
      let sym_side = geom_to_sym_side side in
      match Geom.apply_t_flip joint side g with
      | None -> incr anomaly
      | Some post_rects ->
        match Geom.tree_of_rects post_rects with
        | Ok [t'_oracle] ->
          (match t_flip ~stem ~bar ~side:sym_side t with
           | None ->
             incr sym_none;
             if List.length !sample_mismatches < 5 then
               sample_mismatches := (t, stem, bar, side, t'_oracle, None) :: !sample_mismatches
           | Some t'_sym ->
             let os = Tiling.to_string t'_oracle in
             let ss = Tiling.to_string t'_sym in
             if os = ss then incr match_count
             else begin
               incr mismatch;
               if List.length !sample_mismatches < 5 then
                 sample_mismatches := (t, stem, bar, side, t'_oracle, Some t'_sym) :: !sample_mismatches
             end)
        | _ -> incr anomaly) candidates
  ) reps;
  Printf.printf "n=%d: %d total, %d match, %d sym=None, %d mismatch, %d anomalies\n%!"
    n !total !match_count !sym_none !mismatch !anomaly;
  if !sample_mismatches <> [] then begin
    Printf.printf "  sample mismatches (first 5):\n";
    List.iter (fun (t, stem, bar, side, oracle, sym_opt) ->
      let side_s = match side with Geom.Lo -> "Lo" | Geom.Hi -> "Hi" in
      let sym_s = match sym_opt with
        | None -> "None"
        | Some t' -> Tiling.to_string t' in
      Printf.printf "    t=%s stem=%d bar=%d %s  oracle=%s  sym=%s\n"
        (Tiling.to_string t) stem bar side_s (Tiling.to_string oracle) sym_s
    ) (List.rev !sample_mismatches)
  end

let () =
  let max_n = ref 5 in
  Arg.parse [
    "--max-leaves", Arg.Set_int max_n, "Maximum number of leaves (default: 5)";
  ] (fun _ -> ()) "tflip_sym_check [--max-leaves N]";
  for n = 2 to !max_n do check_n n done
