type dir = H | V
type side = Before | After

type t = int Schrot.tiling

let is_h (h, _) = h
let tree (_, t) = t

let size (_, t) = Schrot.size t

let leaves (_, t) = Schrot.leaves t

let max_leaf t =
  List.fold_left max (-1) (leaves t)

let rec tree_to_string is_h = function
  | Schrot.Tile n -> string_of_int n
  | Schrot.Frame children ->
    let tag = if is_h then "h" else "v" in
    let ch = List2.to_list children
      |> List.map (tree_to_string (not is_h))
      |> String.concat ", " in
    tag ^ "(" ^ ch ^ ")"

let to_string (is_h, t) = tree_to_string is_h t

(* Split tile [n] in direction [dir].
   [~side] controls whether the fresh tile goes Before or After [n].
   If the parent frame has the same orientation as [dir], insert next to [n].
   Otherwise, replace [Tile n] with [Frame [Tile n; Tile fresh]]. *)
let split ?(side = After) n dir t =
  let fresh = max_leaf t + 1 in
  let dir_is_h = (dir = H) in
  let pair a b = match side with
    | After -> [a; b]
    | Before -> [b; a]
  in
  let rec go_frame frame_is_h children =
    match go_list frame_is_h (List2.to_list children) with
    | None -> None
    | Some new_list ->
      match List2.of_list_opt new_list with
      | Some ch -> Some (Schrot.Frame ch)
      | None -> assert false
  and go_list frame_is_h = function
    | [] -> None
    | child :: rest ->
      match child with
      | Schrot.Tile k when k = n ->
        if frame_is_h = dir_is_h then
          Some (pair (Schrot.Tile n) (Schrot.Tile fresh) @ rest)
        else
          let children = pair (Schrot.Tile n) (Schrot.Tile fresh) in
          let wrapped = Schrot.Frame (List2.Cons2 (List.hd children, List.nth children 1, [])) in
          Some (wrapped :: rest)
      | Schrot.Tile _ ->
        (match go_list frame_is_h rest with
         | None -> None
         | Some rest' -> Some (child :: rest'))
      | Schrot.Frame sub ->
        (match go_frame (not frame_is_h) sub with
         | Some child' ->
           Some (child' :: rest)
         | None ->
           match go_list frame_is_h rest with
           | None -> None
           | Some rest' -> Some (Schrot.Frame sub :: rest'))
  in
  let is_h, tree = t in
  match tree with
  | Schrot.Tile k when k = n ->
    let children = pair (Schrot.Tile n) (Schrot.Tile fresh) in
    (dir_is_h, Schrot.Frame (List2.Cons2 (List.hd children, List.nth children 1, [])))
  | Schrot.Tile _ -> t
  | Schrot.Frame children ->
    match go_frame is_h children with
    | Some tree' -> (is_h, tree')
    | None -> t

(* Close tile [n]: remove it from the tiling.
   When a frame collapses to a single child, that child moves up one depth
   level, so the orientation at that position flips. *)
let close n t =
  (* Remove tile [n] from a list of siblings. Returns None if not found. *)
  let rec remove_from_list = function
    | [] -> None
    | child :: rest ->
      match child with
      | Schrot.Tile k when k = n -> Some rest
      | _ ->
        match remove_from_tree child with
        | Some child' -> Some (child' :: rest)
        | None ->
          match remove_from_list rest with
          | Some rest' -> Some (child :: rest')
          | None -> None
  (* Remove tile [n] from a tree. Returns None if not found. *)
  and remove_from_tree = function
    | Schrot.Tile _ -> None
    | Schrot.Frame children ->
      match remove_from_list (List2.to_list children) with
      | None -> None
      | Some new_list ->
        match new_list with
        | [] -> assert false
        | [single] -> Some single (* collapse *)
        | a :: b :: rest -> Some (Schrot.Frame (List2.Cons2 (a, b, rest)))
  in
  let is_h, tree = t in
  match tree with
  | Schrot.Tile k when k = n ->
    invalid_arg "Tiling.close: cannot close the only tile"
  | Schrot.Tile _ -> t
  | Schrot.Frame children ->
    match remove_from_list (List2.to_list children) with
    | None -> t
    | Some [] -> assert false
    | Some [single] -> (not is_h, single) (* root collapse: flip orientation *)
    | Some (a :: b :: rest) -> (is_h, Schrot.Frame (List2.Cons2 (a, b, rest)))

type arrow = Left | Right | Up | Down

(* First leaf in tree order (topmost for H, leftmost for V). *)
let rec first_leaf = function
  | Schrot.Tile n -> n
  | Schrot.Frame children -> first_leaf (List2.hd children)

(* Last leaf in tree order. *)
let rec last_leaf = function
  | Schrot.Tile n -> n
  | Schrot.Frame children ->
    last_leaf (List2.nth children (List2.length children - 1))

(* Find the neighbor of tile [n] in direction [arrow].
   Walk up from [n] to the nearest ancestor frame whose orientation matches
   the arrow axis (H for up/down, V for left/right), move to the
   previous/next sibling, then descend picking the first or last leaf. *)
let neighbor n arrow tiling =
  let axis_is_h = match arrow with Up | Down -> true | Left | Right -> false in
  let go_prev = match arrow with Left | Up -> true | Right | Down -> false in
  (* Find path from root to tile [n]: list of (children, index, frame_is_h) *)
  let rec find_path is_h = function
    | Schrot.Tile k -> if k = n then Some [] else None
    | Schrot.Frame children ->
      let child_is_h = not is_h in
      let rec try_children i = function
        | [] -> None
        | child :: rest ->
          (match find_path child_is_h child with
           | Some path -> Some ((children, i, is_h) :: path)
           | None -> try_children (i + 1) rest)
      in
      try_children 0 (List2.to_list children)
  in
  let is_h = is_h tiling in
  match find_path is_h (tree tiling) with
  | None -> None
  | Some path ->
    (* Walk path from leaf toward root, find first frame with matching axis *)
    let rec scan = function
      | [] -> None
      | (children, idx, frame_is_h) :: upper ->
        if frame_is_h = axis_is_h then
          let k = List2.length children in
          let target_idx = if go_prev then idx - 1 else idx + 1 in
          if target_idx >= 0 && target_idx < k then
            let sibling = List2.nth children target_idx in
            (* Enter sibling: first leaf when going prev, last when going next
               — no, user says topmost/leftmost, which is always first leaf *)
            Some (first_leaf sibling)
          else
            scan upper (* at edge of this frame, try further up *)
        else
          scan upper
    in
    scan (List.rev path)

(* --- Rational intervals --- *)


let rec gcd a b = if b = 0 then a else gcd b (a mod b)

(* --- Dynamic junction resolution via repulsion --- *)

(* Mutable split tree: mirrors a Schrot.t but each Frame carries
   a mutable float array of k+1 cumulative positions in [0, 1]. *)
type split_tree =
  | SLeaf of int
  | SFrame of { mutable pos: float array; children: split_tree List2.t }

let init_splits tree =
  let rec go = function
    | Schrot.Tile n -> SLeaf n
    | Schrot.Frame children ->
      let k = List2.length children in
      let pos = Array.init (k + 1) (fun i ->
        float_of_int i /. float_of_int k) in
      SFrame { pos; children = List2.map go children }
  in
  go tree


(* Collect tile rectangles and cut segments from a split tree.
   Each cut records its owning frame's pos array and index, so repulsion
   can adjust it in place. *)
type rect = { rx: float; ry: float; rw: float; rh: float }

type cut_seg = {
  cut_is_h: bool;      (* true = horizontal cut at constant y *)
  abs_pos: float;       (* position on the cut's own axis *)
  span_lo: float;       (* span start on perpendicular axis *)
  span_hi: float;       (* span end on perpendicular axis *)
  owner: float array;   (* mutable ref to owning frame's positions *)
  cut_idx: int;         (* index into owner: cut between children [i-1] and [i] *)
  frame_span: float;    (* owning frame's span on the cut's axis *)
  frame_origin: float;  (* absolute position of frame's start on the cut's axis *)
  frame_id: int;        (* unique id for the owning frame, stable across mutations *)
}

let collect_geometry is_h_root st =
  let rects = ref [] in
  let cuts = ref [] in
  let next_id = ref 0 in
  let fresh_id () = let id = !next_id in incr next_id; id in
  let rec go x y w h is_h = function
    | SLeaf n ->
      rects := (n, { rx = x; ry = y; rw = w; rh = h }) :: !rects
    | SFrame { pos; children } ->
      let k = List2.length children in
      let fid = fresh_id () in
      for i = 1 to k - 1 do
        if is_h then
          let cy = y +. pos.(i) *. h in
          cuts := { cut_is_h = true; abs_pos = cy;
                    span_lo = x; span_hi = x +. w;
                    owner = pos; cut_idx = i;
                    frame_span = h; frame_origin = y;
                    frame_id = fid } :: !cuts
        else
          let cx = x +. pos.(i) *. w in
          cuts := { cut_is_h = false; abs_pos = cx;
                    span_lo = y; span_hi = y +. h;
                    owner = pos; cut_idx = i;
                    frame_span = w; frame_origin = x;
                    frame_id = fid } :: !cuts
      done;
      List2.iteri (fun i child ->
        if is_h then
          let cy = y +. pos.(i) *. h in
          let ch = (pos.(i + 1) -. pos.(i)) *. h in
          go x cy w ch (not is_h) child
        else
          let cx = x +. pos.(i) *. w in
          let cw = (pos.(i + 1) -. pos.(i)) *. w in
          go cx y cw h (not is_h) child
      ) children
  in
  go 0. 0. 1. 1. is_h_root st;
  (!rects, !cuts)

(* Find boundary groups: for each cut, find all perpendicular cuts that
   terminate at it.  A group needs resolution only if it contains at
   least one coincident pair (same abs_pos, different frame_id) — a
   cross junction.  When resolution is needed, ALL cuts in the group
   are spread evenly (not just the coincident ones). *)
let find_boundary_groups cuts eps =
  let h_cuts = List.filter (fun c -> c.cut_is_h) cuts in
  let v_cuts = List.filter (fun c -> not c.cut_is_h) cuts in
  let groups = ref [] in
  (* Does a group contain at least one coincident pair? *)
  let has_coincidence terms =
    let rec check = function
      | [] -> false
      | c :: rest ->
        List.exists (fun d ->
          c.frame_id <> d.frame_id &&
          abs_float (c.abs_pos -. d.abs_pos) < eps
        ) rest || check rest
    in
    check terms
  in
  (* V-cuts terminating at each H-cut (parent_is_h = true) *)
  List.iter (fun hc ->
    let terms = List.filter (fun vc ->
      (abs_float (vc.span_lo -. hc.abs_pos) < eps ||
       abs_float (vc.span_hi -. hc.abs_pos) < eps) &&
      vc.abs_pos > hc.span_lo +. eps &&
      vc.abs_pos < hc.span_hi -. eps
    ) v_cuts in
    if List.length terms >= 2 && has_coincidence terms then
      groups := (true, hc.span_lo, hc.span_hi, terms) :: !groups
  ) h_cuts;
  (* H-cuts terminating at each V-cut (parent_is_h = false) *)
  List.iter (fun vc ->
    let terms = List.filter (fun hc ->
      (abs_float (hc.span_lo -. vc.abs_pos) < eps ||
       abs_float (hc.span_hi -. vc.abs_pos) < eps) &&
      hc.abs_pos > vc.span_lo +. eps &&
      hc.abs_pos < vc.span_hi -. eps
    ) h_cuts in
    if List.length terms >= 2 && has_coincidence terms then
      groups := (false, vc.span_lo, vc.span_hi, terms) :: !groups
  ) v_cuts;
  !groups

(* Resolve junctions by iterative relaxation toward evenly spaced targets.

   D4-covariant diagonal bias: at H-boundaries (where V-cuts terminate),
   "before" children (above) get smaller target slots → NW-SE diagonal.
   At V-boundaries (where H-cuts terminate), the sort is reversed so the
   bias rotates with the tiling under D4 symmetry.  This ensures
   D4-equivalent tilings produce D4-equivalent rendered layouts.

   References:
   - Eppstein et al. 2009, "Area-Universal Rectangular Layouts": adjacency
     in non-one-sided layouts depends on split ratios; point contact is
     excluded from adjacency.
   - Zeidler et al. 2017, "Tiling Algebra": tabstops as shared constraint
     variables; adjacency = shared tabstop with positive-length boundary.
   - Baez 2022, "Guillotine Partitions and the Hipparchus Operad":
     bijection between guillotine partitions and Schroeder trees. *)
let resolve_splits ?(max_iter = 40) tiling =
  let st = init_splits (tree tiling) in
  let is_h_root = is_h tiling in
  let (_, cuts_eq) = collect_geometry is_h_root st in
  let groups = find_boundary_groups cuts_eq 1e-9 in
  if groups = [] then st
  else begin
    (* For each boundary group, determine the boundary position and sort
       cuts into before-children (span_hi = boundary) then after-children
       (span_lo = boundary), each sub-sorted by equal-split abs_pos.
       This deterministically picks the NW-SE diagonal. *)
    let all_targets : (int * int, float list) Hashtbl.t = Hashtbl.create 16 in
    let add_target key target =
      let prev = try Hashtbl.find all_targets key with Not_found -> [] in
      Hashtbl.replace all_targets key (target :: prev)
    in
    let owner_of : (int * int, float array * int) Hashtbl.t = Hashtbl.create 8 in
    List.iter (fun (parent_is_h, span_lo, span_hi, group) ->
      let n = List.length group in
      (* Find the boundary position: it's the span endpoint shared by the
         terminating cuts (span_hi for before-children, span_lo for after) *)
      let boundary = match group with
        | c :: _ ->
          let mid = (c.span_lo +. c.span_hi) /. 2. in
          if mid < (span_lo +. span_hi) /. 2. then c.span_hi else c.span_lo
        | [] -> 0.
      in
      (* D4-covariant sort:
         - H-boundary: before-first (NW-SE diagonal)
         - V-boundary: after-first (rotated NW-SE = same visual pattern
           under 90° rotation) *)
      let eps = 1e-9 in
      let is_before c = abs_float (c.span_hi -. boundary) < eps in
      let sorted = List.sort (fun a b ->
        let ba = is_before a and bb = is_before b in
        let order = if parent_is_h then
          (* H-boundary: before < after *)
          if ba && not bb then -1
          else if not ba && bb then 1
          else 0
        else
          (* V-boundary: after < before (reversed) *)
          if ba && not bb then 1
          else if not ba && bb then -1
          else 0
        in
        if order <> 0 then order else compare a.abs_pos b.abs_pos
      ) group in
      (* Assign evenly spaced targets *)
      List.iteri (fun i c ->
        let j = i + 1 in
        let target_abs = span_lo +. float_of_int j *. (span_hi -. span_lo)
                         /. float_of_int (n + 1) in
        let target_rel = (target_abs -. c.frame_origin) /. c.frame_span in
        let key = (c.frame_id, c.cut_idx) in
        add_target key target_rel;
        if not (Hashtbl.mem owner_of key) then
          Hashtbl.replace owner_of key (c.owner, c.cut_idx)
      ) sorted
    ) groups;
    (* Merge multiple targets per cut into their average *)
    let final_targets : (int * int, float) Hashtbl.t =
      Hashtbl.create (Hashtbl.length all_targets) in
    Hashtbl.iter (fun key targets ->
      let n = List.length targets in
      let avg = List.fold_left ( +. ) 0. targets /. float_of_int n in
      Hashtbl.replace final_targets key avg
    ) all_targets;
    (* Apply a small fixed offset toward each target to seed relaxation *)
    Hashtbl.iter (fun key target ->
      match Hashtbl.find_opt owner_of key with
      | Some (owner, idx) ->
        let offset = 1e-4 *. (if target > owner.(idx) then 1. else -1.) in
        owner.(idx) <- owner.(idx) +. offset
      | None -> ()
    ) final_targets;
    (* Iterate relaxation *)
    let alpha = 0.3 in
    for _ = 1 to max_iter do
      Hashtbl.iter (fun key target ->
        match Hashtbl.find_opt owner_of key with
        | Some (owner, idx) ->
          owner.(idx) <- owner.(idx) +. alpha *. (target -. owner.(idx))
        | None -> ()
      ) final_targets;
      (* Clamp monotonicity *)
      let clamped = Hashtbl.create 8 in
      Hashtbl.iter (fun _ (owner, _) ->
        let id = Hashtbl.hash owner in
        if not (Hashtbl.mem clamped id) then begin
          Hashtbl.replace clamped id ();
          let k = Array.length owner - 1 in
          let tiny = 1e-6 in
          for i = 1 to k - 1 do
            owner.(i) <- max (owner.(i - 1) +. tiny)
              (min (owner.(i + 1) -. tiny) owner.(i))
          done
        end
      ) owner_of
    done;
    st
  end

(* Compute tile rectangles from a resolved split tree *)
let rects_of_split_tree is_h_root st =
  let (rects, _) = collect_geometry is_h_root st in
  rects

(* --- Degenerate vertex detection --- *)

(* Exact rational interval for a tile on a given axis under equal splits.
   Used only for degenerate vertex detection (cross junctions). *)
type interval = { num: int; den: int }

let tile_interval ~axis n tiling =
  let axis_is_h = (axis = H) in
  let rec go num den is_h = function
    | Schrot.Tile k ->
      if k = n then Some { num; den }
      else None
    | Schrot.Frame children ->
      let arity = List2.length children in
      let rec try_children i = function
        | [] -> None
        | child :: rest ->
          let num', den' =
            if is_h = axis_is_h then (num * arity + i, den * arity)
            else (num, den)
          in
          (match go num' den' (not is_h) child with
           | Some _ as r -> r
           | None -> try_children (i + 1) rest)
      in
      try_children 0 (List2.to_list children)
  in
  match go 0 1 (is_h tiling) (tree tiling) with
  | Some iv -> iv
  | None -> invalid_arg "Tiling.tile_interval: tile not found"

(* A rational point (px/pd, qx/qd) in the unit square *)
type rational_point = { px: int; pd: int; qx: int; qd: int }

let compare_rat (a, b) (c, d) =
  compare (a * d) (c * b)

let compare_rpoint p1 p2 =
  let c = compare_rat (p1.px, p1.pd) (p2.px, p2.pd) in
  if c <> 0 then c
  else compare_rat (p1.qx, p1.qd) (p2.qx, p2.qd)

module RPointMap = Map.Make(struct
  type t = rational_point
  let compare = compare_rpoint
end)

let rat_is_interior num den = num > 0 && num < den

let normalize_rpoint p =
  let g1 = gcd (abs p.px) (abs p.pd) in
  let g2 = gcd (abs p.qx) (abs p.qd) in
  { px = p.px / g1; pd = p.pd / g1;
    qx = p.qx / g2; qd = p.qd / g2 }

(* Does rational a/b lie in [lo/d, hi/d]? *)
let rat_in_closed (a, b) (lo, hi, d) =
  a * d >= lo * b && a * d <= hi * b

(* Approach 1: for each interior point where tile boundaries cross,
   count how many tile closures contain it.  Multiplicity >= 3 = degenerate.
   Candidate points are tile corners (every degenerate vertex is a corner
   of at least one tile).  But a tile can border the point via an edge
   without having a corner there, so we check all tiles for containment. *)
let degenerate_corners tiling =
  let tiles = leaves tiling in
  (* Collect intervals for all tiles *)
  let tile_ivs = List.map (fun n ->
    let iv_h = tile_interval ~axis:H n tiling in
    let iv_v = tile_interval ~axis:V n tiling in
    (iv_h, iv_v)
  ) tiles in
  (* Collect candidate interior points from tile corners *)
  let candidates = ref RPointMap.empty in
  List.iter (fun (iv_h, iv_v) ->
    let xs = [(iv_v.num, iv_v.den); (iv_v.num + 1, iv_v.den)] in
    let ys = [(iv_h.num, iv_h.den); (iv_h.num + 1, iv_h.den)] in
    List.iter (fun (xn, xd) ->
      List.iter (fun (yn, yd) ->
        if rat_is_interior xn xd && rat_is_interior yn yd then
          let p = { px = xn; pd = xd; qx = yn; qd = yd } in
          candidates := RPointMap.add p () !candidates
      ) ys
    ) xs
  ) tile_ivs;
  (* For each candidate, count tiles whose closure contains it *)
  RPointMap.fold (fun pt () acc ->
    let mult = List.fold_left (fun count (iv_h, iv_v) ->
      let x_in = rat_in_closed (pt.px, pt.pd)
        (iv_v.num, iv_v.num + 1, iv_v.den) in
      let y_in = rat_in_closed (pt.qx, pt.qd)
        (iv_h.num, iv_h.num + 1, iv_h.den) in
      if x_in && y_in then count + 1 else count
    ) 0 tile_ivs in
    if mult >= 3 then (pt, mult) :: acc else acc
  ) !candidates []
  |> List.sort (fun (a, _) (b, _) -> compare_rpoint a b)

(* Approach 3: enumerate all cuts, find (H-cut, V-cut) pairs that intersect.
   An H-cut is a horizontal segment (constant y); a V-cut is vertical (constant x). *)
type cut = {
  pos_num: int; pos_den: int;
  span_lo: int; span_hi: int; span_den: int;
}

let collect_cuts tiling =
  let h_cuts = ref [] and v_cuts = ref [] in
  let rec walk is_h h_lo h_hi h_den v_lo v_hi v_den = function
    | Schrot.Tile _ -> ()
    | Schrot.Frame children ->
      let k = List2.length children in
      for i = 1 to k - 1 do
        if is_h then
          (* H-frame: horizontal cut at y = (h_lo*k + i) / (h_den*k),
             spanning x = [v_lo/v_den, v_hi/v_den] *)
          h_cuts := { pos_num = h_lo * k + i; pos_den = h_den * k;
                      span_lo = v_lo; span_hi = v_hi; span_den = v_den } :: !h_cuts
        else
          (* V-frame: vertical cut at x = (v_lo*k + i) / (v_den*k),
             spanning y = [h_lo/h_den, h_hi/h_den] *)
          v_cuts := { pos_num = v_lo * k + i; pos_den = v_den * k;
                      span_lo = h_lo; span_hi = h_hi; span_den = h_den } :: !v_cuts
      done;
      List.iteri (fun i child ->
        let h_lo', h_hi', h_den', v_lo', v_hi', v_den' =
          if is_h then
            (h_lo * k + i, h_lo * k + i + 1, h_den * k, v_lo, v_hi, v_den)
          else
            (h_lo, h_hi, h_den, v_lo * k + i, v_lo * k + i + 1, v_den * k)
        in
        walk (not is_h) h_lo' h_hi' h_den' v_lo' v_hi' v_den' child
      ) (List2.to_list children)
  in
  walk (is_h tiling) 0 1 1 0 1 1 (tree tiling);
  (!h_cuts, !v_cuts)

(* Rational comparison: a/b < c/d iff a*d < c*b (assuming positive denominators) *)
let rat_lt (a, b) (c, d) = a * d < c * b
let rat_le (a, b) (c, d) = a * d <= c * b

(* Find interior intersection points of H-cuts and V-cuts.
   Cross (+): V-cut x strictly inside H-cut x-span AND H-cut y strictly inside V-cut y-span.
   T-junction: one strict, one at boundary. *)
let degenerate_cuts tiling =
  let (h_cuts, v_cuts) = collect_cuts tiling in
  let points = ref [] in
  List.iter (fun hc ->
    (* hc: y = hc.pos_num/hc.pos_den, x in [hc.span_lo/hc.span_den, hc.span_hi/hc.span_den] *)
    List.iter (fun vc ->
      (* vc: x = vc.pos_num/vc.pos_den, y in [vc.span_lo/vc.span_den, vc.span_hi/vc.span_den] *)
      let x_in_h_lo = rat_le (hc.span_lo, hc.span_den) (vc.pos_num, vc.pos_den) in
      let x_in_h_hi = rat_le (vc.pos_num, vc.pos_den) (hc.span_hi, hc.span_den) in
      let x_strict_lo = rat_lt (hc.span_lo, hc.span_den) (vc.pos_num, vc.pos_den) in
      let x_strict_hi = rat_lt (vc.pos_num, vc.pos_den) (hc.span_hi, hc.span_den) in
      let y_in_v_lo = rat_le (vc.span_lo, vc.span_den) (hc.pos_num, hc.pos_den) in
      let y_in_v_hi = rat_le (hc.pos_num, hc.pos_den) (vc.span_hi, vc.span_den) in
      let y_strict_lo = rat_lt (vc.span_lo, vc.span_den) (hc.pos_num, hc.pos_den) in
      let y_strict_hi = rat_lt (hc.pos_num, hc.pos_den) (vc.span_hi, vc.span_den) in
      let x_inside = x_in_h_lo && x_in_h_hi in
      let y_inside = y_in_v_lo && y_in_v_hi in
      let x_strict = x_strict_lo && x_strict_hi in
      let y_strict = y_strict_lo && y_strict_hi in
      (* At least one containment must be strict (otherwise it's a regular corner) *)
      if x_inside && y_inside && (x_strict || y_strict) then
        let pt = { px = vc.pos_num; pd = vc.pos_den;
                   qx = hc.pos_num; qd = hc.pos_den } in
        points := pt :: !points
    ) v_cuts
  ) h_cuts;
  List.sort_uniq compare_rpoint !points

(* Depth of the lowest common ancestor of two tiles in the tree.
   This is the length of the common path prefix from root to each tile. *)
let lca_depth a b tiling =
  let find_path n =
    let rec go is_h = function
      | Schrot.Tile k -> if k = n then Some [] else None
      | Schrot.Frame children ->
        let rec try_ch i = function
          | [] -> None
          | child :: rest ->
            match go (not is_h) child with
            | Some path -> Some (i :: path)
            | None -> try_ch (i + 1) rest
        in
        try_ch 0 (List2.to_list children)
    in
    go (is_h tiling) (tree tiling)
  in
  match find_path a, find_path b with
  | Some pa, Some pb ->
    let rec walk depth pa pb =
      match pa, pb with
      | ia :: ra, ib :: rb when ia = ib -> walk (depth + 1) ra rb
      | _ -> depth
    in
    walk 0 pa pb
  | _ -> 0

(* --- Tabstop-based adjacency --- *)

type adjacency = North | South | East | West

type tab = int
type bounds = { left: tab; right: tab; top: tab; bottom: tab }

let tabstop_extract tiling =
  let next_id = ref 0 in
  let fresh () = let id = !next_id in incr next_id; id in
  let x_left = fresh () in
  let x_right = fresh () in
  let y_top = fresh () in
  let y_bottom = fresh () in
  let tiles = ref [] in
  let rec go left right top bottom is_h = function
    | Schrot.Tile n ->
      tiles := (n, { left; right; top; bottom }) :: !tiles
    | Schrot.Frame children ->
      let k = List2.length children in
      if is_h then begin
        (* H frame: introduce k-1 y-tabstops between consecutive children *)
        let tabs = Array.init (k + 1) (fun i ->
          if i = 0 then top
          else if i = k then bottom
          else fresh ()
        ) in
        List2.iteri (fun i child ->
          go left right tabs.(i) tabs.(i + 1) (not is_h) child
        ) children
      end else begin
        (* V frame: introduce k-1 x-tabstops *)
        let tabs = Array.init (k + 1) (fun i ->
          if i = 0 then left
          else if i = k then right
          else fresh ()
        ) in
        List2.iteri (fun i child ->
          go tabs.(i) tabs.(i + 1) top bottom (not is_h) child
        ) children
      end
  in
  go x_left x_right y_top y_bottom (is_h tiling) (tree tiling);
  List.rev !tiles

(* Two tiles are potentially adjacent if they share a tabstop on opposing
   sides.  This is the maximal potential adjacency set — every pair that is
   adjacent in some concrete layout of this fragment.  No perpendicular
   overlap check: that depends on split ratios (Eppstein et al. 2009). *)
let tabstop_adjacent a_id b_id bounds_list =
  match List.assoc_opt a_id bounds_list, List.assoc_opt b_id bounds_list with
  | Some a, Some b ->
    if a.right = b.left then Some East
    else if a.left = b.right then Some West
    else if a.bottom = b.top then Some South
    else if a.top = b.bottom then Some North
    else None
  | _ -> None

let tabstop_neighbors n bounds_list =
  List.filter_map (fun (m, _) ->
    if m = n then None
    else match tabstop_adjacent n m bounds_list with
      | Some dir -> Some (dir, m)
      | None -> None
  ) bounds_list

(* All potential adjacency edges: sorted (a, b) pairs with a < b.
   This is the maximal set — includes both diagonals at cross junctions. *)
let tabstop_all_adjacencies tiling =
  let bounds = tabstop_extract tiling in
  let tiles = List.map fst bounds in
  let edges = ref [] in
  List.iter (fun a ->
    List.iter (fun b ->
      if a < b then
        match tabstop_adjacent a b bounds with
        | Some _ -> edges := (a, b) :: !edges
        | None -> ()
    ) tiles
  ) tiles;
  List.sort compare !edges

(* --- D4 symmetry group --- *)

(* 90° CW rotation of the unit square:
   H(c1,...,ck) [top→bottom] becomes V(ck',...,c1') [right→left = reversed]
   V(c1,...,ck) [left→right] becomes H(c1',...,ck') [top→bottom = same order] *)
let rot90 (is_h, tree) =
  let rec go parent_is_h = function
    | Schrot.Tile n -> Schrot.Tile n
    | Schrot.Frame children ->
      let children' = List2.map (go (not parent_is_h)) children in
      if parent_is_h then Schrot.Frame (List2.rev children')
      else Schrot.Frame children'
  in
  (not is_h, go is_h tree)

(* 180° rotation: reverse children at every node *)
let rot180 (is_h, tree) =
  let rec go = function
    | Schrot.Tile n -> Schrot.Tile n
    | Schrot.Frame children -> Schrot.Frame (List2.rev (List2.map go children))
  in
  (is_h, go tree)

(* Horizontal reflection (flip top/bottom):
   reverse children at H-nodes only *)
let flip_h (is_h, tree) =
  let rec go cur_is_h = function
    | Schrot.Tile n -> Schrot.Tile n
    | Schrot.Frame children ->
      let children' = List2.map (go (not cur_is_h)) children in
      if cur_is_h then Schrot.Frame (List2.rev children')
      else Schrot.Frame children'
  in
  (is_h, go is_h tree)

(* All 8 D4 elements applied to a tiling *)
let d4_orbit t =
  let r1 = rot90 t in
  let r2 = rot90 r1 in
  let r3 = rot90 r2 in
  let f = flip_h t in
  let fr1 = rot90 f in
  let fr2 = rot90 fr1 in
  let fr3 = rot90 fr2 in
  [t; r1; r2; r3; f; fr1; fr2; fr3]

(* Klein four-group V4 = {id, rot180, flip_h, flip_v}.
   Preserves cut orientations (H stays H, V stays V). *)
let v4_orbit t =
  let r2 = rot180 t in
  let fh = flip_h t in
  let fv = flip_h r2 in (* flip_v = rot180 . flip_h *)
  [t; r2; fh; fv]

(* Erase leaf labels *)
let erase (is_h, tree) = (is_h, Schrot.map (fun _ -> ()) tree)

(* String representation of a unit tiling (erased labels) *)
let rec unit_tree_to_string is_h = function
  | Schrot.Tile () -> "*"
  | Schrot.Frame children ->
    let tag = if is_h then "h" else "v" in
    let ch = List2.to_list children
      |> List.map (unit_tree_to_string (not is_h))
      |> String.concat ", " in
    tag ^ "(" ^ ch ^ ")"

(* Canonical D4 representative: erase labels, apply all 8 D4 elements,
   pick the lexicographically smallest string representation. *)
let canonical_d4 t =
  let erased = erase t in
  let images = d4_orbit erased in
  let strs = List.map (fun (is_h, tree) ->
    unit_tree_to_string is_h tree
  ) images in
  let sorted = List.sort String.compare strs in
  List.hd sorted

(* Canonical V4 representative: erase labels, apply 4 V4 elements,
   pick the lexicographically smallest. Preserves cut orientation. *)
let canonical_v4 t =
  let erased = erase t in
  let images = v4_orbit erased in
  let strs = List.map (fun (is_h, tree) ->
    unit_tree_to_string is_h tree
  ) images in
  let sorted = List.sort String.compare strs in
  List.hd sorted

(* --- Canonical forms for equivalence classes --- *)

let rec compare_unit_tree t1 t2 =
  match t1, t2 with
  | Schrot.Tile (), Schrot.Tile () -> 0
  | Schrot.Tile _, Schrot.Frame _ -> -1
  | Schrot.Frame _, Schrot.Tile _ -> 1
  | Schrot.Frame ch1, Schrot.Frame ch2 ->
    let c = compare (List2.length ch1) (List2.length ch2) in
    if c <> 0 then c
    else
      let rec lex a b = match a, b with
        | [], [] -> 0
        | [], _ -> -1 | _, [] -> 1
        | x :: xs, y :: ys ->
          let c = compare_unit_tree x y in
          if c <> 0 then c else lex xs ys
      in
      lex (List2.to_list ch1) (List2.to_list ch2)

(* Canonical form under child reversal (planar topology).
   Erases leaf labels, at each Frame picks the lexicographically smaller
   of children vs reversed children. *)
let rec canonical_planar t =
  match t with
  | Schrot.Tile _ -> Schrot.Tile ()
  | Schrot.Frame children ->
    let ch = List2.map canonical_planar children in
    let rev_ch = List2.rev ch in
    let fwd = List2.to_list ch in
    let bwd = List2.to_list rev_ch in
    let rec lex a b = match a, b with
      | [], [] -> 0
      | [], _ -> -1 | _, [] -> 1
      | x :: xs, y :: ys ->
        let c = compare_unit_tree x y in
        if c <> 0 then c else lex xs ys
    in
    if lex fwd bwd <= 0 then Schrot.Frame ch
    else Schrot.Frame rev_ch

(* Canonical form under full child permutation (unordered tree shape).
   Erases leaf labels, sorts children at each node. *)
let rec canonical_unordered t =
  match t with
  | Schrot.Tile _ -> Schrot.Tile ()
  | Schrot.Frame children ->
    let ch = List2.map canonical_unordered children in
    let sorted = List2.to_list ch |> List.sort compare_unit_tree in
    match sorted with
    | a :: b :: rest -> Schrot.Frame (List2.Cons2 (a, b, rest))
    | _ -> assert false

(* Check if two adjacency edge lists (on the same number of vertices)
   are isomorphic, using backtracking permutation search with
   degree-based pruning. *)
let graphs_isomorphic n edges1 edges2 =
  if n <= 1 then true
  else
    let adj1 = Array.make n [] and adj2 = Array.make n [] in
    List.iter (fun (a, b) -> adj1.(a) <- b :: adj1.(a); adj1.(b) <- a :: adj1.(b)) edges1;
    List.iter (fun (a, b) -> adj2.(a) <- b :: adj2.(a); adj2.(b) <- a :: adj2.(b)) edges2;
    let deg1 = Array.map (fun l -> List.length l) adj1 in
    let deg2 = Array.map (fun l -> List.length l) adj2 in
    (* perm.(i) = j means vertex i in graph1 maps to vertex j in graph2 *)
    let perm = Array.make n (-1) in
    let used = Array.make n false in
    let rec go k =
      if k = n then true
      else
        (* Try mapping vertex k of graph1 to each unused vertex of graph2 *)
        let d = deg1.(k) in
        let found = ref false in
        for j = 0 to n - 1 do
          if not !found && not used.(j) && deg2.(j) = d then begin
            (* Check consistency: for all already-mapped neighbors of k,
               the image must be a neighbor of j *)
            let consistent = List.for_all (fun nb ->
              if perm.(nb) >= 0 then List.mem perm.(nb) adj2.(j)
              else true
            ) adj1.(k) in
            if consistent then begin
              perm.(k) <- j;
              used.(j) <- true;
              if go (k + 1) then found := true
              else begin perm.(k) <- -1; used.(j) <- false end
            end
          end
        done;
        !found
    in
    go 0

(* Graph fingerprint: sorted multiset of (degree, sorted_neighbor_degrees).
   Two graphs with different fingerprints are guaranteed non-isomorphic. *)
let adjacency_fingerprint n edges =
  if n <= 1 then []
  else
    let adj = Array.make n [] in
    List.iter (fun (a, b) ->
      adj.(a) <- b :: adj.(a);
      adj.(b) <- a :: adj.(b)
    ) edges;
    let degree = Array.map List.length adj in
    let fp = Array.init n (fun v ->
      let nd = List.map (fun u -> degree.(u)) adj.(v) |> List.sort compare in
      (degree.(v), nd)
    ) in
    let result = Array.to_list fp in
    List.sort compare result

(* Relabel leaves 0..n-1 in left-to-right order *)
let relabel (is_h, tree) =
  let counter = ref 0 in
  let rec go = function
    | Schrot.Tile _ ->
      let n = !counter in
      incr counter;
      Schrot.Tile n
    | Schrot.Frame children ->
      Schrot.Frame (List2.map go children)
  in
  (is_h, go tree)
