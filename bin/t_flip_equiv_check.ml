(* Equivalence check: Geom.t_flip (symbolic + Asinowski filter) vs the
   M-M geometric oracle (apply_t_flip + tree_of_rects).

   For every labeled orbit representative at n <= max_leaves, every
   pair of distinct leaves (stem, bar):

   - symbolic = Geom.t_flip ~stem ~bar t
   - oracle   = lower t with irrational weights, find the joint where
                bar_tile = bar and stem ∈ stem_tiles, check Asinowski
                admissibility, if admissible reconstruct via
                tree_of_rects.

   Both should agree (equal trees or both None) on every pair.  At
   n=7 in generic mode we expect 1632 Some results matching exactly;
   all other (stem, bar) pairs (including the 150 windmill cases)
   return None from both paths. *)

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

let orbit_reps n =
  let unit_tilings = Schrot.enum n in
  let all_tilings = List.map label_tiling unit_tilings in
  let seen = Hashtbl.create 64 in
  List.filter (fun t ->
    let key = Tiling.canonical_d4 t in
    if Hashtbl.mem seen key then false
    else (Hashtbl.add seen key (); true)) all_tilings

(* Oracle: find the joint for (stem, bar), apply M-M, reconstruct. *)
let oracle ~stem ~bar t =
  let g = Geom.of_weighted (weight_tiling t) t in
  let joints = Geom.t_joints g in
  let matched =
    List.find_opt (fun (j : Geom.t_joint) ->
      j.bar_tile = bar
      && (stem = fst j.stem_tiles || stem = snd j.stem_tiles)) joints
  in
  match matched with
  | None -> None
  | Some joint ->
    let side =
      if stem = fst joint.stem_tiles then Geom.Lo else Geom.Hi
    in
    if not (Geom.is_asinowski_admissible g joint side) then None
    else
      match Geom.apply_t_flip joint side g with
      | None -> None
      | Some rects ->
        match Geom.tree_of_rects rects with
        | Ok [t'] -> Some t'
        | _ -> None

let check_n n =
  let reps = orbit_reps n in
  let total_pairs = ref 0 in
  let both_some_match = ref 0 in
  let both_none = ref 0 in
  let only_sym = ref 0 in
  let only_oracle = ref 0 in
  let mismatch = ref 0 in
  let samples = ref [] in
  List.iter (fun t ->
    let lvs = Tiling.leaves t in
    List.iter (fun stem ->
      List.iter (fun bar ->
        if stem <> bar then begin
          incr total_pairs;
          let sym = Geom.t_flip ~stem ~bar t in
          let orc = oracle ~stem ~bar t in
          match sym, orc with
          | None, None -> incr both_none
          | Some ts, Some to_ ->
            if Tiling.to_string ts = Tiling.to_string to_ then
              incr both_some_match
            else begin
              incr mismatch;
              if List.length !samples < 5 then
                samples := (t, stem, bar, Some ts, Some to_) :: !samples
            end
          | Some ts, None ->
            incr only_sym;
            if List.length !samples < 5 then
              samples := (t, stem, bar, Some ts, None) :: !samples
          | None, Some to_ ->
            incr only_oracle;
            if List.length !samples < 5 then
              samples := (t, stem, bar, None, Some to_) :: !samples
        end
      ) lvs
    ) lvs
  ) reps;
  Printf.printf
    "n=%d: %d orbit reps, %d pairs\n\
    \  both_some_match=%d  both_none=%d  only_sym=%d  only_oracle=%d  mismatch=%d\n%!"
    n (List.length reps) !total_pairs
    !both_some_match !both_none !only_sym !only_oracle !mismatch;
  let ok = !only_sym = 0 && !only_oracle = 0 && !mismatch = 0 in
  if not ok && !samples <> [] then begin
    Printf.printf "  sample divergences (first 5):\n";
    List.iter (fun (t, stem, bar, s, o) ->
      let str = function None -> "None" | Some t' -> Tiling.to_string t' in
      Printf.printf "    t=%s  stem=%d bar=%d  sym=%s  oracle=%s\n"
        (Tiling.to_string t) stem bar (str s) (str o)
    ) (List.rev !samples)
  end;
  ok

let () =
  let max_n = ref 6 in
  Arg.parse [
    "--max-leaves", Arg.Set_int max_n, "Maximum number of leaves (default: 6)";
  ] (fun _ -> ()) "t_flip_equiv_check [--max-leaves N]";
  let all_ok = ref true in
  for n = 2 to !max_n do
    if not (check_n n) then all_ok := false
  done;
  if !all_ok then
    Printf.printf "\nGeom.t_flip matches the oracle on every (stem, bar) pair.\n"
  else begin
    Printf.printf "\nFAIL: Geom.t_flip diverges from oracle on some pairs.\n";
    exit 1
  end
