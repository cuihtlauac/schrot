(* Oracle for tree-level T-flip: enumerate every valid (t, stem, bar, side)
   at small n, apply the known-correct geometric T-flip, reconstruct the
   post-flip tree.  Output is the ground-truth map

     (t, stem, bar) -> t'

   that the symbolic implementation (future Tiling.t_flip) must match.

   This run also computes structural features for each row to cluster
   (t, stem, bar) -> t' by tree-level shape, which drives Phase C rule
   synthesis. *)

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

let filter_candidates g =
  let sc = Geom.subwall_simplicity g in
  List.concat_map (fun (j, lo_simple, hi_simple) ->
    (if lo_simple then [(j, Geom.Lo)] else [])
    @ (if hi_simple then [(j, Geom.Hi)] else [])) sc

let stem_of (j : Geom.t_joint) side =
  match side with
  | Geom.Lo -> fst j.stem_tiles
  | Geom.Hi -> snd j.stem_tiles

(* --- Path utilities --- *)

(* Path to leaf n in the Schrot tree: list of child indices from root.
   Returns None if n is not present. *)
let rec path_to : (int, unit) Schrot.t -> int -> int list option =
  fun tr tgt ->
  match tr with
  | Schrot.Tile m -> if m = tgt then Some [] else None
  | Schrot.Frame ch ->
    let rec scan i = function
      | [] -> None
      | (_, c) :: rest ->
        match path_to c tgt with
        | Some p -> Some (i :: p)
        | None -> scan (i + 1) rest
    in
    scan 0 (List2.to_list ch)

let tiling_path (_, tree) n = path_to tree n

(* Descend a path into the tree, returning the subtree.  Indexes are
   into the Frame's children; a path of [] returns the whole tree. *)
let rec descend tr path =
  match path, tr with
  | [], _ -> Some tr
  | _, Schrot.Tile _ -> None
  | i :: rest, Schrot.Frame ch ->
    match List.nth_opt (List2.to_list ch) i with
    | None -> None
    | Some (_, c) -> descend c rest

(* Longest common prefix of two paths. *)
let rec lcp a b =
  match a, b with
  | x :: xs, y :: ys when x = y -> x :: lcp xs ys
  | _ -> []

(* Orientation of the frame at a given path from the root of t.
   The root's orientation is is_h(t); children alternate. *)
let dir_at_path t path =
  let (is_h, _) = t in
  let d = List.length path in
  (* Root at depth 0 has is_h; each level down flips. *)
  if d mod 2 = 0 then is_h else not is_h

(* Arity of the frame at the given path (must be a Frame, not a Tile). *)
let arity_at_path t path =
  let (_, tree) = t in
  match descend tree path with
  | Some (Schrot.Frame ch) -> List2.length ch
  | _ -> 0

(* --- Row record and feature computation --- *)

type row = {
  t : Tiling.t;
  stem : int;
  bar : int;
  side : Geom.flip_side;
  jx : float;
  jy : float;
  through_h : bool;
  t_prime : Tiling.t;
  (* features computed from t, t_prime *)
  stem_path : int list;
  bar_path : int list;
  lca_path : int list;
  bar_parent_dir : bool;       (* H / V of the frame containing bar *)
  bar_parent_arity : int;
  stem_parent_dir : bool;
  stem_parent_arity : int;
  root_flipped : bool;         (* is_h(t) <> is_h(t') *)
}

type reconstruction_anomaly =
  | Multi of Tiling.t list
  | Non_guillotine
  | Empty

type outcome = Row of row | Anomaly_row of row * reconstruction_anomaly

let make_features t stem bar side jx jy through_h t_prime =
  let sp = tiling_path t stem |> Option.value ~default:[] in
  let bp = tiling_path t bar |> Option.value ~default:[] in
  let lp = lcp sp bp in
  let parent p = match List.rev p with _ :: rest -> List.rev rest | [] -> [] in
  let bar_parent_path = parent bp in
  let stem_parent_path = parent sp in
  {
    t; stem; bar; side; jx; jy; through_h; t_prime;
    stem_path = sp;
    bar_path = bp;
    lca_path = lp;
    bar_parent_dir = dir_at_path t bar_parent_path;
    bar_parent_arity = arity_at_path t bar_parent_path;
    stem_parent_dir = dir_at_path t stem_parent_path;
    stem_parent_arity = arity_at_path t stem_parent_path;
    root_flipped = fst t <> fst t_prime;
  }

let catalog_orbit_reps n =
  let unit_tilings = Schrot.enum n in
  let all_tilings = List.map label_tiling unit_tilings in
  let seen = Hashtbl.create 64 in
  List.filter (fun t ->
    let key = Tiling.canonical_d4 t in
    if Hashtbl.mem seen key then false
    else (Hashtbl.add seen key (); true)) all_tilings

let catalog_for_n n =
  let reps = catalog_orbit_reps n in
  List.concat_map (fun t ->
    let g = Geom.of_weighted (weight_tiling t) t in
    let candidates = filter_candidates g in
    List.filter_map (fun (joint, side) ->
      match Geom.apply_t_flip joint side g with
      | None -> None
      | Some post_rects ->
        let stem = stem_of joint side in
        let bar = joint.Geom.bar_tile in
        let jx = joint.jx and jy = joint.jy in
        let through_h = joint.through_h in
        match Geom.tree_of_rects post_rects with
        | Ok [t'] ->
          let r = make_features t stem bar side jx jy through_h t' in
          Some (Row r)
        | Ok [] ->
          let r = make_features t stem bar side jx jy through_h t in
          Some (Anomaly_row (r, Empty))
        | Ok ts ->
          let r = make_features t stem bar side jx jy through_h t in
          Some (Anomaly_row (r, Multi ts))
        | Error _ ->
          let r = make_features t stem bar side jx jy through_h t in
          Some (Anomaly_row (r, Non_guillotine))) candidates
  ) reps

(* --- Report --- *)

let side_str = function Geom.Lo -> "Lo" | Geom.Hi -> "Hi"
let dir_str h = if h then "H" else "V"
let path_str p = String.concat "." (List.map string_of_int p)

let write_row_table oc rows =
  Printf.fprintf oc "| t | stem | bar | side | dir | jpos | bP.dir | bP.arity | sP.dir | sP.arity | lca | stemP | barP | flip? | t' |\n";
  Printf.fprintf oc "|---|------|-----|------|-----|------|--------|----------|--------|----------|-----|-------|------|-------|-----|\n";
  List.iter (fun r ->
    Printf.fprintf oc
      "| `%s` | %d | %d | %s | %s | (%.3f,%.3f) | %s | %d | %s | %d | `%s` | `%s` | `%s` | %s | `%s` |\n"
      (Tiling.to_string r.t) r.stem r.bar (side_str r.side) (dir_str r.through_h)
      r.jx r.jy
      (dir_str r.bar_parent_dir) r.bar_parent_arity
      (dir_str r.stem_parent_dir) r.stem_parent_arity
      (path_str r.lca_path) (path_str r.stem_path) (path_str r.bar_path)
      (if r.root_flipped then "yes" else "no")
      (Tiling.to_string r.t_prime)) rows

let anomaly_str = function
  | Multi ts -> Printf.sprintf "multi (%d trees)" (List.length ts)
  | Non_guillotine -> "non-guillotine"
  | Empty -> "empty Ok []"

let write_anomaly_table oc anomalies =
  Printf.fprintf oc "| t | stem | bar | side | dir | bP.dir | bP.arity | lca | anomaly |\n";
  Printf.fprintf oc "|---|------|-----|------|-----|--------|----------|-----|---------|\n";
  List.iter (fun (r, a) ->
    Printf.fprintf oc "| `%s` | %d | %d | %s | %s | %s | %d | `%s` | %s |\n"
      (Tiling.to_string r.t) r.stem r.bar (side_str r.side)
      (dir_str r.through_h)
      (dir_str r.bar_parent_dir) r.bar_parent_arity
      (path_str r.lca_path) (anomaly_str a)) anomalies

let partition_outcomes os =
  List.partition_map (function
    | Row r -> Left r
    | Anomaly_row (r, a) -> Right (r, a)) os

(* Cluster rows by (bar_parent_arity, bar_parent_dir_vs_stem_parent_dir,
   stem_parent_arity, lca_depth_vs_bar_parent_depth).  These are the
   structural features most likely to govern the symbolic rule. *)
type cluster_key = {
  k_bar_arity : int;
  k_stem_arity : int;
  k_same_parent_dir : bool;
  k_lca_is_bar_parent : bool;    (* stem is in the same frame as bar vs deeper *)
  k_root_flipped : bool;
}

let cluster_of r =
  let bar_parent_path = match List.rev r.bar_path with _ :: rest -> List.rev rest | [] -> [] in
  {
    k_bar_arity = r.bar_parent_arity;
    k_stem_arity = r.stem_parent_arity;
    k_same_parent_dir = r.bar_parent_dir = r.stem_parent_dir;
    k_lca_is_bar_parent = r.lca_path = bar_parent_path;
    k_root_flipped = r.root_flipped;
  }

let cluster_label k =
  Printf.sprintf
    "barP_arity=%d stemP_arity=%d same_parent_dir=%b lca_is_bar_parent=%b root_flip=%b"
    k.k_bar_arity k.k_stem_arity k.k_same_parent_dir k.k_lca_is_bar_parent k.k_root_flipped

let write_clusters oc rows =
  let by_cluster = Hashtbl.create 32 in
  List.iter (fun r ->
    let k = cluster_of r in
    let cur = try Hashtbl.find by_cluster k with Not_found -> [] in
    Hashtbl.replace by_cluster k (r :: cur)) rows;
  let clusters = Hashtbl.fold (fun k rs acc -> (k, List.rev rs) :: acc) by_cluster [] in
  let sorted = List.sort (fun (_, a) (_, b) -> compare (List.length b) (List.length a)) clusters in
  Printf.fprintf oc "\n### Cluster summary (by structural features)\n\n";
  Printf.fprintf oc "| count | key |\n";
  Printf.fprintf oc "|-------|-----|\n";
  List.iter (fun (k, rs) ->
    Printf.fprintf oc "| %d | %s |\n" (List.length rs) (cluster_label k)) sorted;
  Printf.fprintf oc "\n### Per-cluster samples (up to 3 rows each)\n";
  List.iter (fun (k, rs) ->
    Printf.fprintf oc "\n#### %s  (%d rows)\n\n" (cluster_label k) (List.length rs);
    let sample = List.filteri (fun i _ -> i < 3) rs in
    write_row_table oc sample) sorted

let write_header oc ~max_n =
  Printf.fprintf oc "# T-flip oracle catalog + structural classification\n\n";
  Printf.fprintf oc "Authoritative mapping (t, stem, bar, side) -> t' for every valid\n";
  Printf.fprintf oc "T-flip up to n=%d, derived by lowering each orbit representative\n" max_n;
  Printf.fprintf oc "to generic geometry (irrational weights), applying\n";
  Printf.fprintf oc "Geom.apply_t_flip, and reconstructing the tree via\n";
  Printf.fprintf oc "Geom.tree_of_rects.\n\n";
  Printf.fprintf oc "Feature legend:\n\n";
  Printf.fprintf oc "- **bP.dir / bP.arity**: direction (H/V) and arity of bar's parent frame in t.\n";
  Printf.fprintf oc "- **sP.dir / sP.arity**: same for stem's parent frame.\n";
  Printf.fprintf oc "- **lca**: path (dotted child-indices) of lowest common ancestor.\n";
  Printf.fprintf oc "- **stemP / barP**: full paths of stem and bar.\n";
  Printf.fprintf oc "- **flip?**: does the root orientation change between t and t'?\n\n"

let write_section oc n outcomes =
  let rows, anomalies = partition_outcomes outcomes in
  Printf.fprintf oc "\n## n=%d  (%d rows, %d anomalies)\n\n"
    n (List.length rows) (List.length anomalies);
  if anomalies <> [] then begin
    Printf.fprintf oc "### Anomalies\n\n";
    write_anomaly_table oc anomalies
  end;
  if rows <> [] then begin
    Printf.fprintf oc "\n### Rows\n\n";
    write_row_table oc rows
  end

let () =
  let max_n = ref 6 in
  let report_path = ref None in
  let include_clusters = ref true in
  Arg.parse [
    "--max-leaves", Arg.Set_int max_n, "Maximum number of leaves (default: 6)";
    "--report", Arg.String (fun s -> report_path := Some s),
      "PATH  Write a Markdown catalog to PATH";
    "--no-clusters", Arg.Clear include_clusters, "Skip the cluster summary section";
  ] (fun _ -> ()) "tflip_oracle [--max-leaves N] [--report PATH]";
  let oc = match !report_path with
    | None -> stdout
    | Some path -> open_out path in
  write_header oc ~max_n:!max_n;
  let total_rows = ref 0 in
  let total_anom = ref 0 in
  let all_rows = ref [] in
  for n = 2 to !max_n do
    let outcomes = catalog_for_n n in
    let rows, anoms = partition_outcomes outcomes in
    total_rows := !total_rows + List.length rows;
    total_anom := !total_anom + List.length anoms;
    all_rows := List.rev_append rows !all_rows;
    write_section oc n outcomes;
    Printf.printf "n=%d: %d rows, %d anomalies\n%!"
      n (List.length rows) (List.length anoms)
  done;
  Printf.fprintf oc "\n## Totals\n\n";
  Printf.fprintf oc "- rows: %d\n" !total_rows;
  Printf.fprintf oc "- anomalies: %d\n" !total_anom;
  if !include_clusters then write_clusters oc (List.rev !all_rows);
  if !report_path <> None then close_out oc;
  Printf.printf "Total: %d rows, %d anomalies\n%!" !total_rows !total_anom
