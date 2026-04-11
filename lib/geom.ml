(* Tiling geometry in normalized coordinates on [0,1]².
   All positions and dimensions are relative to the unit square;
   SVG renderers scale these to pixel coordinates at render time.
   Adjacency excludes point contact (Eppstein et al. 2009). *)

type rect = { x: float; y: float; w: float; h: float }

type t = {
  tiling: Tiling.t;
  rects: (int * rect) list;
  adjacency: (int * int) list;  (* sorted pairs (a, b) with a < b *)
}

(* Compute tile rectangles on [0,1)² using resolved splits *)
let compute_rects tiling =
  let wt = Tiling.resolve_splits tiling in
  let raw = Tiling.rects_of_weighted wt in
  List.map (fun (n, (r : Tiling.rect)) ->
    (n, { x = r.rx; y = r.ry; w = r.rw; h = r.rh })
  ) raw

(* Compute adjacency from tile rects: shared boundary segment (not just a point).
   min_overlap is the minimum shared boundary length to count as adjacent.
   Default 1e-9 excludes exact point contact only.  A larger value (e.g. 1e-2)
   also excludes near-point-contact at almost-aligned cross junctions. *)
let compute_adjacency ?(min_overlap = 1e-9) rects =
  let eps = 1e-9 in
  let edges = ref [] in
  List.iter (fun (a, ar) ->
    List.iter (fun (b, br) ->
      if a < b then begin
        let shared_v =
          (abs_float ((ar.x +. ar.w) -. br.x) < eps ||
           abs_float ((br.x +. br.w) -. ar.x) < eps)
          && (min (ar.y +. ar.h) (br.y +. br.h) -. max ar.y br.y > min_overlap)
        in
        let shared_h =
          (abs_float ((ar.y +. ar.h) -. br.y) < eps ||
           abs_float ((br.y +. br.h) -. ar.y) < eps)
          && (min (ar.x +. ar.w) (br.x +. br.w) -. max ar.x br.x > min_overlap)
        in
        if shared_v || shared_h then
          edges := (a, b) :: !edges
      end
    ) rects
  ) rects;
  !edges

let of_tiling tiling =
  let rects = compute_rects tiling in
  let adjacency = compute_adjacency rects in
  { tiling; rects; adjacency }

let of_weighted ?(min_overlap = 1e-9) wt tiling =
  let raw = Tiling.rects_of_weighted wt in
  let rects = List.map (fun (n, (r : Tiling.rect)) ->
    (n, { x = r.rx; y = r.ry; w = r.rw; h = r.rh })
  ) raw in
  let adjacency = compute_adjacency ~min_overlap rects in
  { tiling; rects; adjacency }

(* Equal-split geometry: no junction resolution.
   Cross junctions have point contact only (excluded from adjacency),
   producing diamonds in the adjacency poset. *)
let of_tiling_equal tiling =
  let equal_wt = Schrot.map_weights (fun () -> 1.0) (snd tiling) in
  let raw = Tiling.rects_of_weighted (fst tiling, equal_wt) in
  let rects = List.map (fun (n, (r : Tiling.rect)) ->
    (n, { x = r.rx; y = r.ry; w = r.rw; h = r.rh })
  ) raw in
  let adjacency = compute_adjacency rects in
  { tiling; rects; adjacency }

(* --- Queries --- *)

let rect_of g n =
  List.assoc n g.rects

let center_of g n =
  let r = rect_of g n in
  (r.x +. r.w /. 2., r.y +. r.h /. 2.)

let edges g = g.adjacency

let neighbors g n =
  List.filter_map (fun (a, b) ->
    if a = n then Some b
    else if b = n then Some a
    else None
  ) g.adjacency
