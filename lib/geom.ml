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

(* Compute adjacency from tile rects: shared boundary segment (not just a point) *)
let compute_adjacency rects =
  let eps = 1e-9 in
  let edges = ref [] in
  List.iter (fun (a, ar) ->
    List.iter (fun (b, br) ->
      if a < b then begin
        let shared_v =
          (abs_float ((ar.x +. ar.w) -. br.x) < eps ||
           abs_float ((br.x +. br.w) -. ar.x) < eps)
          && (min (ar.y +. ar.h) (br.y +. br.h) -. max ar.y br.y > eps)
        in
        let shared_h =
          (abs_float ((ar.y +. ar.h) -. br.y) < eps ||
           abs_float ((br.y +. br.h) -. ar.y) < eps)
          && (min (ar.x +. ar.w) (br.x +. br.w) -. max ar.x br.x > eps)
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
