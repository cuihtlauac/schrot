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

(* Interior vertex enumeration: for each deduplicated interior vertex,
   returns (px, py, touching) where touching = [(tile_id, touch_kind); ...]. *)
let interior_vertices ~eps rects =
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
    if touching = [] then None
    else Some (px, py, touching)
  ) unique

let t_joints ?(eps = 1e-9) g =
  let verts = interior_vertices ~eps g.rects in
  List.filter_map (fun (px, py, touching) ->
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
  ) verts

let is_generic ?(eps = 1e-9) g =
  let verts = interior_vertices ~eps g.rects in
  List.for_all (fun (_, _, touching) -> List.length touching <= 3) verts

(* --- Geometric T-flips --- *)

type flip_side = Lo | Hi

let subwall_simplicity ?(eps = 1e-9) g =
  (* A T-flip at (j, side) is valid iff the chosen stem sits flush
     against the bar's edge on that side: the stem grows into the bar
     and the bar shrinks back to flush with the stem's other edge.
     If the stem's far edge does NOT coincide with the bar's far edge,
     then either the stem is in the middle of the bar (bar would
     split) or the stem extends past the bar (bar residue + stem
     overlap).  Both are invalid.  This criterion subsumes the
     Merino-Mutze minimality condition without needing to enumerate
     intermediate T-joints on the through-wall: any such intermediate
     T-joint would force the stem to end at its position, breaking
     the edge-match. *)
  let joints = t_joints ~eps g in
  let edges_match j side =
    let stem_id = match side with
      | Lo -> fst j.stem_tiles
      | Hi -> snd j.stem_tiles in
    let stem = List.assoc stem_id g.rects in
    let bar = List.assoc j.bar_tile g.rects in
    let stem_edge, bar_edge =
      match side with
      | Lo ->
        if j.through_h then (stem.x, bar.x)
        else (stem.y, bar.y)
      | Hi ->
        if j.through_h then (stem.x +. stem.w, bar.x +. bar.w)
        else (stem.y +. stem.h, bar.y +. bar.h)
    in
    abs_float (stem_edge -. bar_edge) < eps
  in
  List.map (fun j -> (j, edges_match j Lo, edges_match j Hi)) joints

let apply_t_flip ?(eps = 1e-9) joint side g =
  ignore eps;
  (* Pick the stem tile for this side.
     stem_tiles are ordered: fst = closer to origin (smaller coordinate),
     snd = farther.  Lo = toward origin, Hi = away. *)
  let stem_id = match side with
    | Lo -> fst joint.stem_tiles
    | Hi -> snd joint.stem_tiles
  in
  let a = List.assoc stem_id g.rects in
  let b = List.assoc joint.bar_tile g.rects in
  let new_a, new_b =
    if joint.through_h then
      (* Horizontal through-wall: A extends vertically, B shrinks horizontally *)
      let na = { x = a.x; y = min a.y b.y; w = a.w; h = a.h +. b.h } in
      let nb = match side with
        | Lo -> { x = a.x +. a.w; y = b.y; w = b.w -. a.w; h = b.h }
        | Hi -> { x = b.x;        y = b.y; w = b.w -. a.w; h = b.h }
      in
      (na, nb)
    else
      (* Vertical through-wall: A extends horizontally, B shrinks vertically *)
      let na = { x = min a.x b.x; y = a.y; w = a.w +. b.w; h = a.h } in
      let nb = match side with
        | Lo -> { x = b.x; y = a.y +. a.h; w = b.w; h = b.h -. a.h }
        | Hi -> { x = b.x; y = b.y;        w = b.w; h = b.h -. a.h }
      in
      (na, nb)
  in
  if new_b.w < eps || new_b.h < eps then None
  else
    let new_rects = List.map (fun (id, r) ->
      if id = stem_id then (id, new_a)
      else if id = joint.bar_tile then (id, new_b)
      else (id, r)
    ) g.rects in
    Some new_rects

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
  | [] -> [PBlock { block_tiles = []; block_bbox = bbox }]
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

(* --- Equivalence predicates --- *)

let weak_equivalent ?(eps = 1e-9) g1 g2 =
  match tree_of_geom ~eps g1, tree_of_geom ~eps g2 with
  | Ok trees1, Ok trees2 ->
    let shape t =
      let (is_h, tree) = Tiling.erase t in
      Tiling.unit_tree_to_string is_h tree
    in
    let s1 = List.map shape trees1 in
    let s2 = List.map shape trees2 in
    List.exists (fun s -> List.mem s s2) s1
  | _ -> false

let strong_equivalent g1 g2 =
  let n1 = List.length g1.rects and n2 = List.length g2.rects in
  if n1 <> n2 then false
  else
    let normalize rects adjacency =
      let ids = List.mapi (fun i (id, _) -> (id, i)) rects in
      let map_id id = List.assoc id ids in
      List.map (fun (a, b) ->
        let a' = map_id a and b' = map_id b in
        if a' < b' then (a', b') else (b', a')
      ) adjacency
    in
    Tiling.graphs_isomorphic n1
      (normalize g1.rects g1.adjacency)
      (normalize g2.rects g2.adjacency)

(* --- Enumerate geometric T-flips --- *)

(* Core: enumerate T-flips from a rect list, using a dummy Geom.t for
   t_joints/subwall_simplicity which only need g.rects. *)
let t_flips_of_rects ?(eps = 1e-9) rects =
  let g = { tiling = (false, Schrot.Tile 0);  (* dummy, unused *)
            rects; adjacency = [] } in
  let joints = subwall_simplicity ~eps g in
  let results = ref [] in
  List.iter (fun (joint, lo_simple, hi_simple) ->
    let try_side side =
      match apply_t_flip ~eps joint side g with
      | None -> ()
      | Some new_rects ->
        (match tree_of_rects ~eps new_rects with
         | Ok [t] ->
           let stem_id = match side with
             | Lo -> fst joint.stem_tiles
             | Hi -> snd joint.stem_tiles
           in
           results := (Tiling.T_flip (stem_id, joint.bar_tile), t, new_rects) :: !results
         | Ok _ | Error _ -> ())
    in
    if lo_simple then try_side Lo;
    if hi_simple then try_side Hi
  ) joints;
  !results

let enumerate_t_flips ?(eps = 1e-9) g =
  let results = t_flips_of_rects ~eps g.rects in
  (* Deduplicate by result *)
  let seen = Hashtbl.create 16 in
  List.filter (fun (_, t', _) ->
    let key = Tiling.to_string t' in
    if Hashtbl.mem seen key then false
    else (Hashtbl.add seen key (); true)
  ) (List.rev results)

let enumerate_t_flips_from_rects ?(eps = 1e-9) rects =
  (* Like enumerate_t_flips but also accepts multi-tree results from
     tree_of_rects (cross junction ambiguity).  Used for reverse-flip
     checks where the reverse geometry may recreate a cross junction. *)
  let g = { tiling = (false, Schrot.Tile 0); rects; adjacency = [] } in
  let joints = subwall_simplicity ~eps g in
  let results = ref [] in
  List.iter (fun (joint, lo_simple, hi_simple) ->
    let try_side side =
      match apply_t_flip ~eps joint side g with
      | None -> ()
      | Some new_rects ->
        (match tree_of_rects ~eps new_rects with
         | Ok trees ->
           let stem_id = match side with
             | Lo -> fst joint.stem_tiles
             | Hi -> snd joint.stem_tiles
           in
           List.iter (fun t ->
             results := (Tiling.T_flip (stem_id, joint.bar_tile), t) :: !results
           ) trees
         | Error _ -> ())
    in
    if lo_simple then try_side Lo;
    if hi_simple then try_side Hi
  ) joints;
  let seen = Hashtbl.create 16 in
  List.filter_map (fun (f, t) ->
    let key = Tiling.to_string t in
    if Hashtbl.mem seen key then None
    else (Hashtbl.add seen key (); Some (f, t))
  ) (List.rev !results)

(* --- Asinowski-admissible T-flips --- *)

(* A T-flip is Asinowski-admissible iff its post-flip geometry is a
   strong (guillotine and generic) rectangulation.  Asinowski's
   pivoting flips (§4.5, Theorem 22) are exactly the Merino-Mütze
   T-flips restricted to cases preserving guillotine — same rotation
   as [apply_t_flip], stricter admissibility.  See THEORY.md Layer 2. *)
let is_asinowski_admissible ?(eps = 1e-9) g joint side =
  match apply_t_flip ~eps joint side g with
  | None -> false
  | Some rects ->
    match tree_of_rects ~eps rects with
    | Ok [t'] ->
      let g' = { tiling = t'; rects; adjacency = [] } in
      is_generic ~eps g'
    | Ok _ | Error _ -> false
