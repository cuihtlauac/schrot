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

(* --- T-joint enumeration --- *)

type corner = NW | NE | SW | SE

type t_joint = {
  jx : float;
  jy : float;
  bar_tile : int;
  stem_tiles : int * int;
  stem_corners : corner * corner;
  through_h : bool;
}

type touch = At_corner of corner | At_edge_h | At_edge_v

let classify_touch ~eps px py r =
  let at_left = abs_float (r.x -. px) < eps in
  let at_right = abs_float (r.x +. r.w -. px) < eps in
  let at_top = abs_float (r.y -. py) < eps in
  let at_bottom = abs_float (r.y +. r.h -. py) < eps in
  if (at_left || at_right) && (at_top || at_bottom) then
    Some (At_corner (match at_left, at_top with
      | true, true -> NW | false, true -> NE
      | true, false -> SW | false, false -> SE))
  else if at_left || at_right then Some At_edge_v
  else if at_top || at_bottom then Some At_edge_h
  else None

let t_joints ?(eps = 1e-9) g =
  let rects = g.rects in
  (* 1. Collect interior corner candidates *)
  let candidates = ref [] in
  List.iter (fun (_, r) ->
    List.iter (fun (x, y) ->
      if x > eps && x < 1.0 -. eps && y > eps && y < 1.0 -. eps then
        candidates := (x, y) :: !candidates
    ) [(r.x, r.y); (r.x +. r.w, r.y);
       (r.x, r.y +. r.h); (r.x +. r.w, r.y +. r.h)]
  ) rects;
  (* 2. Deduplicate nearby points (pairwise, not just consecutive) *)
  let unique = List.fold_left (fun acc (x, y) ->
    if List.exists (fun (px, py) ->
      abs_float (x -. px) < eps && abs_float (y -. py) < eps) acc
    then acc
    else (x, y) :: acc
  ) [] !candidates |> List.rev in
  (* 3. For each point, find touching tiles and classify *)
  List.filter_map (fun (px, py) ->
    let touching = List.filter_map (fun (n, r) ->
      let x_in = px >= r.x -. eps && px <= r.x +. r.w +. eps in
      let y_in = py >= r.y -. eps && py <= r.y +. r.h +. eps in
      if x_in && y_in then
        match classify_touch ~eps px py r with
        | Some t -> Some (n, t) | None -> None
      else None
    ) rects in
    if List.length touching <> 3 then None
    else
      let corners = List.filter_map (fun (n, t) ->
        match t with At_corner c -> Some (n, c) | _ -> None) touching in
      let edges = List.filter_map (fun (n, t) ->
        match t with
        | At_edge_h -> Some (n, true)
        | At_edge_v -> Some (n, false)
        | _ -> None) touching in
      match corners, edges with
      | [(a, ca); (b, cb)], [(e, edge_is_h)] ->
        let through_h = edge_is_h in
        (* Order stem tiles: first closer to origin along through-wall *)
        let sa, sca, sb, scb =
          if through_h then
            (* through-wall horizontal, stem vertical: order by x.
               NE/SE corner → tile is left of stem; NW/SW → right *)
            let a_left = (ca = NE || ca = SE) in
            if a_left then (a, ca, b, cb) else (b, cb, a, ca)
          else
            (* through-wall vertical, stem horizontal: order by y.
               SW/SE corner → tile is above stem; NW/NE → below *)
            let a_above = (ca = SW || ca = SE) in
            if a_above then (a, ca, b, cb) else (b, cb, a, ca)
        in
        Some { jx = px; jy = py; bar_tile = e;
               stem_tiles = (sa, sb);
               stem_corners = (sca, scb);
               through_h }
      | _ -> None
  ) unique

let subwall_simplicity ?(eps = 1e-9) g =
  let joints = t_joints ~eps g in
  let annotated = List.mapi (fun i j ->
    let wc = if j.through_h then j.jy else j.jx in
    let pos = if j.through_h then j.jx else j.jy in
    (i, j, j.through_h, wc, pos)
  ) joints in
  let sorted = List.sort (fun (_, _, h1, c1, p1) (_, _, h2, c2, p2) ->
    let r = compare h1 h2 in if r <> 0 then r else
    let r = compare c1 c2 in if r <> 0 then r else
    compare p1 p2
  ) annotated in
  let rec group acc cur = function
    | [] -> List.rev (if cur = [] then acc else List.rev cur :: acc)
    | ((_, _, h, c, _) as x) :: rest ->
      match cur with
      | [] -> group acc [x] rest
      | (_, _, h', c', _) :: _ when h = h' && abs_float (c -. c') < eps ->
        group acc (x :: cur) rest
      | _ -> group (List.rev cur :: acc) [x] rest
  in
  let groups = group [] [] sorted in
  let tagged = List.concat_map (fun grp ->
    let n = List.length grp in
    List.mapi (fun k (i, j, _, _, _) -> (i, j, k = 0, k = n - 1)) grp
  ) groups in
  let restored = List.sort (fun (i1, _, _, _) (i2, _, _, _) -> compare i1 i2) tagged in
  List.map (fun (_, j, lo, hi) -> (j, lo, hi)) restored

(* --- Geometry to tree reconstruction --- *)

type block = {
  block_tiles : (int * rect) list;
  block_bbox : rect;
}

type partial_tree =
  | PTile of int
  | PBlock of block
  | PFrame of bool * partial_tree list

type non_guillotine = {
  decomposition : partial_tree;
  blocks : block list;
}

(* Deduplicate sorted floats within eps *)
let dedup_floats ~eps xs =
  List.fold_left (fun acc x ->
    match acc with
    | prev :: _ when abs_float (x -. prev) < eps -> acc
    | _ -> x :: acc
  ) [] xs |> List.rev

(* Find wall-to-wall cut coordinates along one axis.
   For horizontal cuts: lo/hi extract y-boundaries, span extracts x-range.
   A coordinate c is a cut iff every tile lies entirely on one side. *)
let find_cuts ~eps ~lo ~hi ~span_lo ~span_hi bbox tiles =
  (* Collect candidate coordinates from tile boundaries, strictly interior *)
  let bbox_lo = lo bbox and bbox_hi = hi bbox in
  let candidates = List.fold_left (fun acc (_, r) ->
    let a = lo r and b = hi r in
    let acc = if a > bbox_lo +. eps && a < bbox_hi -. eps then a :: acc else acc in
    if b > bbox_lo +. eps && b < bbox_hi -. eps then b :: acc else acc
  ) [] tiles in
  let candidates = List.sort_uniq compare candidates |> dedup_floats ~eps in
  (* Test each candidate: is it a cut? *)
  List.filter (fun c ->
    (* Every tile must lie entirely on one side *)
    List.for_all (fun (_, r) ->
      hi r <= c +. eps || c <= lo r +. eps
    ) tiles
    (* The cut must span the full perpendicular extent *)
    && (let min_span = List.fold_left (fun m (_, r) ->
          if hi r <= c +. eps || c <= lo r +. eps then
            (* tile touches the cut line — check its span *)
            if abs_float (hi r -. c) < eps || abs_float (lo r -. c) < eps
            then min m (span_lo r)
            else m
          else m) infinity tiles in
        let max_span = List.fold_left (fun m (_, r) ->
          if abs_float (hi r -. c) < eps || abs_float (lo r -. c) < eps
          then max m (span_hi r)
          else m) neg_infinity tiles in
        min_span <= span_lo bbox +. eps && max_span >= span_hi bbox -. eps)
  ) candidates

let h_cuts ~eps bbox tiles =
  find_cuts ~eps
    ~lo:(fun r -> r.y) ~hi:(fun r -> r.y +. r.h)
    ~span_lo:(fun r -> r.x) ~span_hi:(fun r -> r.x +. r.w)
    bbox tiles

let v_cuts ~eps bbox tiles =
  find_cuts ~eps
    ~lo:(fun r -> r.x) ~hi:(fun r -> r.x +. r.w)
    ~span_lo:(fun r -> r.y) ~span_hi:(fun r -> r.y +. r.h)
    bbox tiles

(* Partition tiles into strips between consecutive cut boundaries *)
let form_strips ~eps ~lo ~hi ~set_lo_hi bbox cuts tiles =
  let boundaries =
    lo bbox :: (List.sort compare cuts) @ [hi bbox] in
  let rec pairs = function
    | a :: (b :: _ as rest) -> (a, b) :: pairs rest
    | _ -> []
  in
  List.map (fun (strip_lo, strip_hi) ->
    let strip_bbox = set_lo_hi bbox strip_lo (strip_hi -. strip_lo) in
    let strip_tiles = List.filter (fun (_, r) ->
      lo r >= strip_lo -. eps && hi r <= strip_hi +. eps
    ) tiles in
    (strip_bbox, strip_tiles)
  ) (pairs boundaries)

let h_strips ~eps bbox cuts tiles =
  form_strips ~eps
    ~lo:(fun r -> r.y) ~hi:(fun r -> r.y +. r.h)
    ~set_lo_hi:(fun bb y h -> { bb with y; h })
    bbox cuts tiles

let v_strips ~eps bbox cuts tiles =
  form_strips ~eps
    ~lo:(fun r -> r.x) ~hi:(fun r -> r.x +. r.w)
    ~set_lo_hi:(fun bb x w -> { bb with x; w })
    bbox cuts tiles

(* Cartesian product of a list of lists *)
let cartesian_product lists =
  List.fold_right (fun options acc ->
    List.concat_map (fun opt ->
      List.map (fun rest -> opt :: rest) acc
    ) options
  ) lists [[]]

(* Core recursive decomposition *)
let rec decompose ~eps bbox tiles =
  match tiles with
  | [(label, _)] -> [PTile label]
  | [] -> assert false
  | _ ->
    let hc = h_cuts ~eps bbox tiles in
    let vc = v_cuts ~eps bbox tiles in
    let results = ref [] in
    if hc <> [] then begin
      let strips = h_strips ~eps bbox hc tiles in
      let child_options = List.map (fun (sb, st) ->
        decompose ~eps sb st
      ) strips in
      let combos = cartesian_product child_options in
      List.iter (fun children ->
        results := PFrame (true, children) :: !results
      ) combos
    end;
    if vc <> [] then begin
      let strips = v_strips ~eps bbox vc tiles in
      let child_options = List.map (fun (sb, st) ->
        decompose ~eps sb st
      ) strips in
      let combos = cartesian_product child_options in
      List.iter (fun children ->
        results := PFrame (false, children) :: !results
      ) combos
    end;
    if !results = [] then
      [PBlock { block_tiles = tiles; block_bbox = bbox }]
    else
      List.rev !results

let rec is_complete = function
  | PTile _ -> true
  | PBlock _ -> false
  | PFrame (_, children) -> List.for_all is_complete children

let extract_blocks pt =
  let blocks = ref [] in
  let rec go = function
    | PTile _ -> ()
    | PBlock b -> blocks := b :: !blocks
    | PFrame (_, children) -> List.iter go children
  in
  go pt;
  List.rev !blocks

let rec to_schrot = function
  | PTile n -> Schrot.Tile n
  | PBlock _ -> assert false
  | PFrame (_, children) ->
    let weighted = List.map (fun c -> ((), to_schrot c)) children in
    match weighted with
    | a :: b :: rest -> Schrot.Frame (List2.Cons2 (a, b, rest))
    | _ -> assert false

let to_tiling = function
  | PTile n -> (false, Schrot.Tile n)
  | PBlock _ -> assert false
  | PFrame (is_h, children) ->
    let weighted = List.map (fun c -> ((), to_schrot c)) children in
    match weighted with
    | a :: b :: rest -> (is_h, Schrot.Frame (List2.Cons2 (a, b, rest)))
    | _ -> assert false

let tree_of_rects ?(eps = 1e-9) rects =
  match rects with
  | [] -> invalid_arg "tree_of_rects: empty tile list"
  | [(n, _)] -> Ok [(false, Schrot.Tile n)]
  | _ ->
    let bbox = { x = 0.; y = 0.; w = 1.; h = 1. } in
    let all = decompose ~eps bbox rects in
    let complete = List.filter is_complete all in
    if complete <> [] then
      Ok (List.map to_tiling complete)
    else
      let pt = List.hd all in
      Error { decomposition = pt; blocks = extract_blocks pt }

let tree_of_geom ?eps g =
  tree_of_rects ?eps g.rects
