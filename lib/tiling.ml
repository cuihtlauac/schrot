type dir = H | V
type side = Before | After

type t = (int, unit) Schrot.tiling

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
      |> List.map (fun (_, c) -> tree_to_string (not is_h) c)
      |> String.concat ", " in
    tag ^ "(" ^ ch ^ ")"

let to_string (is_h, t) = tree_to_string is_h t

(* Split tile [n] in direction [dir].  (Layer 1: operad composition,
   maps SR_n -> SR_{n+1}.)
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
    | (w, child) :: rest ->
      match child with
      | Schrot.Tile k when k = n ->
        let u t = ((), t) in
        if frame_is_h = dir_is_h then
          Some (List.map u (pair (Schrot.Tile n) (Schrot.Tile fresh)) @ rest)
        else
          let children = pair (Schrot.Tile n) (Schrot.Tile fresh) in
          let wrapped = Schrot.unit_frame (List2.Cons2 (List.hd children, List.nth children 1, [])) in
          Some (((), wrapped) :: rest)
      | Schrot.Tile _ ->
        (match go_list frame_is_h rest with
         | None -> None
         | Some rest' -> Some ((w, child) :: rest'))
      | Schrot.Frame sub ->
        (match go_frame (not frame_is_h) sub with
         | Some child' ->
           Some ((w, child') :: rest)
         | None ->
           match go_list frame_is_h rest with
           | None -> None
           | Some rest' -> Some ((w, Schrot.Frame sub) :: rest'))
  in
  let is_h, tree = t in
  match tree with
  | Schrot.Tile k when k = n ->
    let children = pair (Schrot.Tile n) (Schrot.Tile fresh) in
    (dir_is_h, Schrot.unit_frame (List2.Cons2 (List.hd children, List.nth children 1, [])))
  | Schrot.Tile _ -> t
  | Schrot.Frame children ->
    match go_frame is_h children with
    | Some tree' -> (is_h, tree')
    | None -> t

(* Close tile [n]: remove it from the tiling.  (Layer 1: operad inverse,
   maps SR_{n+1} -> SR_n.)
   When a frame collapses to a single child, that child moves up one depth
   level, so the orientation at that position flips. *)
let close n t =
  (* Remove tile [n] from a list of siblings. Returns None if not found. *)
  let rec remove_from_list = function
    | [] -> None
    | (w, child) :: rest ->
      match child with
      | Schrot.Tile k when k = n -> Some rest
      | _ ->
        match remove_from_tree child with
        | Some child' -> Some ((w, child') :: rest)
        | None ->
          match remove_from_list rest with
          | Some rest' -> Some ((w, child) :: rest')
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
        | [(_w, single)] -> Some single (* collapse *)
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
    | Some [(_w, single)] -> (not is_h, single) (* root collapse: flip orientation *)
    | Some (a :: b :: rest) -> (is_h, Schrot.Frame (List2.Cons2 (a, b, rest)))

type arrow = Left | Right | Up | Down

(* First leaf in tree order (topmost for H, leftmost for V). *)
let rec first_leaf = function
  | Schrot.Tile n -> n
  | Schrot.Frame children -> first_leaf (snd (List2.hd children))

(* Last leaf in tree order. *)
let rec last_leaf = function
  | Schrot.Tile n -> n
  | Schrot.Frame children ->
    last_leaf (snd (List2.nth children (List2.length children - 1)))

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
        | (_, child) :: rest ->
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
            let sibling = snd (List2.nth children target_idx) in
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
      SFrame { pos; children = List2.map (fun (_, c) -> go c) children }
  in
  go tree


(* Collect tile rectangles and cut segments from a split tree.
   Each cut records its owning frame's pos array and index, so repulsion
   can adjust it in place. *)
type rect = { rx: float; ry: float; rw: float; rh: float }

let rec split_tree_height = function
  | SLeaf _ -> 0
  | SFrame { children; _ } ->
    1 + List2.fold_left (fun acc c -> max acc (split_tree_height c)) 0 children

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
  before_height: int;   (* height of child subtree before this cut (child cut_idx-1) *)
  after_height: int;    (* height of child subtree after this cut (child cut_idx) *)
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
      let ch_arr = Array.of_list (List2.to_list children) in
      for i = 1 to k - 1 do
        let bh = split_tree_height ch_arr.(i - 1) in
        let ah = split_tree_height ch_arr.(i) in
        if is_h then
          let cy = y +. pos.(i) *. h in
          cuts := { cut_is_h = true; abs_pos = cy;
                    span_lo = x; span_hi = x +. w;
                    owner = pos; cut_idx = i;
                    frame_span = h; frame_origin = y;
                    frame_id = fid;
                    before_height = bh; after_height = ah } :: !cuts
        else
          let cx = x +. pos.(i) *. w in
          cuts := { cut_is_h = false; abs_pos = cx;
                    span_lo = y; span_hi = y +. h;
                    owner = pos; cut_idx = i;
                    frame_span = w; frame_origin = x;
                    frame_id = fid;
                    before_height = bh; after_height = ah } :: !cuts
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
  (* V-cuts terminating at each H-cut *)
  List.iter (fun hc ->
    let terms = List.filter (fun vc ->
      (abs_float (vc.span_lo -. hc.abs_pos) < eps ||
       abs_float (vc.span_hi -. hc.abs_pos) < eps) &&
      vc.abs_pos > hc.span_lo +. eps &&
      vc.abs_pos < hc.span_hi -. eps
    ) v_cuts in
    if List.length terms >= 2 && has_coincidence terms then
      groups := (hc.span_lo, hc.span_hi, terms) :: !groups
  ) h_cuts;
  (* H-cuts terminating at each V-cut *)
  List.iter (fun vc ->
    let terms = List.filter (fun hc ->
      (abs_float (hc.span_lo -. vc.abs_pos) < eps ||
       abs_float (hc.span_hi -. vc.abs_pos) < eps) &&
      hc.abs_pos > vc.span_lo +. eps &&
      hc.abs_pos < vc.span_hi -. eps
    ) h_cuts in
    if List.length terms >= 2 && has_coincidence terms then
      groups := (vc.span_lo, vc.span_hi, terms) :: !groups
  ) v_cuts;
  !groups

(* Resolve junctions by iterative relaxation toward evenly spaced targets.

   D4-covariant structural bias: at each boundary group, cuts from
   shallower subtrees get smaller target slots, giving deeper subtrees
   more room.  This is D4-covariant because subtree height is a tree
   property, invariant under geometric transformations.

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
  let to_weighted st =
    let rec go = function
      | SLeaf n -> Schrot.Tile n
      | SFrame { pos; children } ->
        Schrot.Frame (List2.mapi (fun i child ->
          (pos.(i + 1) -. pos.(i), go child)
        ) children)
    in
    (is_h_root, go st)
  in
  if groups = [] then to_weighted st
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
    List.iter (fun (span_lo, span_hi, group) ->
      let n = List.length group in
      (* D4-covariant structural sort: each cut moves toward its shallower
         side, giving the deeper side more room.  Sort by
         (before_height - after_height): negative = before is shallower,
         cut gets a smaller slot; positive = after is shallower, larger slot. *)
      let sorted = List.sort (fun a b ->
        let da = a.before_height - a.after_height in
        let db = b.before_height - b.after_height in
        let c = compare da db in
        if c <> 0 then c else compare a.abs_pos b.abs_pos
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
    (* Apply a small fixed offset toward each target to seed the direction *)
    Hashtbl.iter (fun key target ->
      match Hashtbl.find_opt owner_of key with
      | Some (owner, idx) ->
        let offset = 1e-4 *. (if target > owner.(idx) then 1. else -1.) in
        owner.(idx) <- owner.(idx) +. offset
      | None -> ()
    ) final_targets;
    (* Relax toward targets.  Each step: pos += alpha * (target - pos).
       Converges geometrically; stop when max displacement < eps. *)
    let alpha = 0.3 in
    let eps = 1e-6 in
    let converged = ref false in
    let iter = ref 0 in
    while not !converged && !iter < max_iter do
      incr iter;
      let max_disp = ref 0. in
      Hashtbl.iter (fun key target ->
        match Hashtbl.find_opt owner_of key with
        | Some (owner, idx) ->
          let disp = alpha *. (target -. owner.(idx)) in
          owner.(idx) <- owner.(idx) +. disp;
          max_disp := max !max_disp (abs_float disp)
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
      ) owner_of;
      if !max_disp < eps then converged := true
    done;
    to_weighted st
  end

(* Compute tile rectangles from a weighted tiling on [0,1]^2.
   Reconstructs cumulative positions from weights. *)
let rects_of_weighted (is_h_root, tree) =
  let rects = ref [] in
  let rec go x y w h is_h = function
    | Schrot.Tile n ->
      rects := (n, { rx = x; ry = y; rw = w; rh = h }) :: !rects
    | Schrot.Frame children ->
      let children_list = List2.to_list children in
      let k = List.length children_list in
      let total = List.fold_left (fun acc (wt, _) -> acc +. wt) 0. children_list in
      let cum = Array.make (k + 1) 0. in
      List.iteri (fun i (wt, _) ->
        cum.(i + 1) <- cum.(i) +. wt /. total
      ) children_list;
      List2.iteri (fun i (_, child) ->
        if is_h then
          let cy = y +. cum.(i) *. h in
          let ch = (cum.(i + 1) -. cum.(i)) *. h in
          go x cy w ch (not is_h) child
        else
          let cx = x +. cum.(i) *. w in
          let cw = (cum.(i + 1) -. cum.(i)) *. w in
          go cx y cw h (not is_h) child
      ) children
  in
  go 0. 0. 1. 1. is_h_root tree;
  !rects

(* Collect cut line segments grouped by depth from a weighted tiling.
   Returns an array indexed by depth; each element is a list of
   (x1, y1, x2, y2) segments in unit-square coordinates. *)
let cuts_of_weighted (is_h_root, tree) =
  let max_depth = Schrot.height tree in
  let lines = Array.make (max_depth + 1) [] in
  let rec go x y w h is_h depth = function
    | Schrot.Tile _ -> ()
    | Schrot.Frame children ->
      let children_list = List2.to_list children in
      let k = List.length children_list in
      let total = List.fold_left (fun acc (wt, _) -> acc +. wt) 0. children_list in
      let cum = Array.make (k + 1) 0. in
      List.iteri (fun i (wt, _) ->
        cum.(i + 1) <- cum.(i) +. wt /. total
      ) children_list;
      for i = 1 to k - 1 do
        if is_h then
          let cy = y +. cum.(i) *. h in
          lines.(depth) <- (x, cy, x +. w, cy) :: lines.(depth)
        else
          let cx = x +. cum.(i) *. w in
          lines.(depth) <- (cx, y, cx, y +. h) :: lines.(depth)
      done;
      List2.iteri (fun i (_, child) ->
        if is_h then
          let cy = y +. cum.(i) *. h in
          let ch = (cum.(i + 1) -. cum.(i)) *. h in
          go x cy w ch (not is_h) (depth + 1) child
        else
          let cx = x +. cum.(i) *. w in
          let cw = (cum.(i + 1) -. cum.(i)) *. w in
          go cx y cw h (not is_h) (depth + 1) child
      ) children
  in
  go 0. 0. 1. 1. is_h_root 0 tree;
  lines

(* --- Segment enumeration and weight adjustment --- *)

type segment = {
  seg_id : int;
  seg_path : int list;
  seg_cut : int;
  seg_is_h : bool;
  seg_depth : int;
  seg_x1 : float; seg_y1 : float;
  seg_x2 : float; seg_y2 : float;
}

let enumerate_segments (is_h_root, tree) =
  let segments = ref [] in
  let next_id = ref 0 in
  let rec go rev_path x y w h is_h depth = function
    | Schrot.Tile _ -> ()
    | Schrot.Frame children ->
      let children_list = List2.to_list children in
      let k = List.length children_list in
      let total = List.fold_left (fun acc (wt, _) -> acc +. wt) 0. children_list in
      let cum = Array.make (k + 1) 0. in
      List.iteri (fun i (wt, _) ->
        cum.(i + 1) <- cum.(i) +. wt /. total
      ) children_list;
      for cut = 0 to k - 2 do
        let ci = cut + 1 in
        let id = !next_id in
        incr next_id;
        let (x1, y1, x2, y2) =
          if is_h then
            let cy = y +. cum.(ci) *. h in
            (x, cy, x +. w, cy)
          else
            let cx = x +. cum.(ci) *. w in
            (cx, y, cx, y +. h)
        in
        segments := {
          seg_id = id;
          seg_path = List.rev rev_path;
          seg_cut = cut;
          seg_is_h = is_h;
          seg_depth = depth;
          seg_x1 = x1; seg_y1 = y1;
          seg_x2 = x2; seg_y2 = y2;
        } :: !segments
      done;
      List2.iteri (fun i (_, child) ->
        if is_h then
          let cy = y +. cum.(i) *. h in
          let ch = (cum.(i + 1) -. cum.(i)) *. h in
          go (i :: rev_path) x cy w ch (not is_h) (depth + 1) child
        else
          let cx = x +. cum.(i) *. w in
          let cw = (cum.(i + 1) -. cum.(i)) *. w in
          go (i :: rev_path) cx y cw h (not is_h) (depth + 1) child
      ) children
  in
  go [] 0. 0. 1. 1. is_h_root 0 tree;
  List.rev !segments

let adjust_weight path cut_idx delta (is_h, tree) =
  let min_w = 0.02 in
  let rec go path tree =
    match path, tree with
    | [], Schrot.Frame children ->
      let children_list = List2.to_list children in
      let w_before = fst (List.nth children_list cut_idx) in
      let w_after = fst (List.nth children_list (cut_idx + 1)) in
      let new_before = w_before +. delta in
      let new_after = w_after -. delta in
      if new_before < min_w || new_after < min_w then None
      else
        let new_list = List.mapi (fun i (w, c) ->
          if i = cut_idx then (new_before, c)
          else if i = cut_idx + 1 then (new_after, c)
          else (w, c)
        ) children_list in
        (match new_list with
         | a :: b :: rest ->
           Some (Schrot.Frame (List2.Cons2 (a, b, rest)))
         | _ -> None)
    | i :: rest, Schrot.Frame children ->
      let children_list = List2.to_list children in
      (match List.nth_opt children_list i with
       | None -> None
       | Some (_, child) ->
         match go rest child with
         | None -> None
         | Some child' ->
           let new_list = List.mapi (fun j (wj, cj) ->
             if j = i then (wj, child') else (wj, cj)
           ) children_list in
           (match new_list with
            | a :: b :: rest ->
              Some (Schrot.Frame (List2.Cons2 (a, b, rest)))
            | _ -> None))
    | _, _ -> None
  in
  match go path tree with
  | None -> None
  | Some tree' -> Some (is_h, tree')

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
        | (_, child) :: rest ->
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

module EdgeSet = Set.Make(struct
  type t = int * int
  let compare = compare
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
      List.iteri (fun i (_w, child) ->
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

(* Cross junction tiles: for each degenerate point of multiplicity 4,
   return the point and the 4 tile IDs whose closures contain it.
   Same logic as degenerate_corners but collecting tile IDs. *)
let cross_junction_tiles tiling =
  let tiles = leaves tiling in
  let tile_ivs = List.map (fun n ->
    let iv_h = tile_interval ~axis:H n tiling in
    let iv_v = tile_interval ~axis:V n tiling in
    (n, iv_h, iv_v)
  ) tiles in
  (* Candidate interior points from tile corners *)
  let candidates = ref RPointMap.empty in
  List.iter (fun (_, iv_h, iv_v) ->
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
  RPointMap.fold (fun pt () acc ->
    let touching = List.fold_left (fun ids (n, iv_h, iv_v) ->
      let x_in = rat_in_closed (pt.px, pt.pd)
        (iv_v.num, iv_v.num + 1, iv_v.den) in
      let y_in = rat_in_closed (pt.qx, pt.qd)
        (iv_h.num, iv_h.num + 1, iv_h.den) in
      if x_in && y_in then n :: ids else ids
    ) [] tile_ivs in
    if List.length touching = 4 then
      (pt, List.sort compare touching) :: acc
    else acc
  ) !candidates []
  |> List.sort (fun (a, _) (b, _) -> compare_rpoint a b)

let count_cross_junctions tiling =
  List.length (cross_junction_tiles tiling)

(* Depth of the lowest common ancestor of two tiles in the tree.
   This is the length of the common path prefix from root to each tile. *)
let cut_depth a b tiling =
  let find_path n =
    let rec go is_h = function
      | Schrot.Tile k -> if k = n then Some [] else None
      | Schrot.Frame children ->
        let rec try_ch i = function
          | [] -> None
          | (_w, child) :: rest ->
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
        List2.iteri (fun i (_, child) ->
          go left right tabs.(i) tabs.(i + 1) (not is_h) child
        ) children
      end else begin
        (* V frame: introduce k-1 x-tabstops *)
        let tabs = Array.init (k + 1) (fun i ->
          if i = 0 then left
          else if i = k then right
          else fresh ()
        ) in
        List2.iteri (fun i (_, child) ->
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

(* --- Strong guillotine multiplicity --- *)

let binom n k =
  if k < 0 || k > n then 0
  else
    let k = min k (n - k) in
    let rec go i acc =
      if i > k then acc
      else go (i + 1) (acc * (n - k + i) / i)
    in
    go 1 1

(* Delannoy number D(a,b) = number of lattice paths (0,0)->(a,b)
   using steps (1,0), (0,1), (1,1).
   D(a,b) = Σ_k C(a,k) C(b,k) 2^k.
   Counts non-generic interleavings (allowing coincident positions). *)
let delannoy a b =
  let rec go k acc =
    if k > min a b then acc
    else go (k + 1) (acc + binom a k * binom b k * (1 lsl k))
  in
  go 0 0

type border = Top | Bottom | Left | Right

(* Number of perpendicular cuts terminating at a node from a given side.
   In a Schroder tree, children alternate orientation, so a child of an
   H-frame is either Tile (0 cuts) or V-frame (arity-1 cuts). *)
let perp_cuts = function
  | Schrot.Tile _ -> 0
  | Schrot.Frame children -> List2.length children - 1

(* Ordered list of tile IDs touching a given border of a node's region. *)
let rec boundary_tiles_ordered is_h bdr = function
  | Schrot.Tile n -> [n]
  | Schrot.Frame children ->
    let ch = List2.to_list children in
    let k = List.length ch in
    if is_h then
      (match bdr with
      | Top -> boundary_tiles_ordered (not is_h) Top (snd (List.hd ch))
      | Bottom -> boundary_tiles_ordered (not is_h) Bottom (snd (List.nth ch (k - 1)))
      | Left -> List.concat_map (fun (_, c) -> boundary_tiles_ordered (not is_h) Left c) ch
      | Right -> List.concat_map (fun (_, c) -> boundary_tiles_ordered (not is_h) Right c) ch)
    else
      (match bdr with
      | Left -> boundary_tiles_ordered (not is_h) Left (snd (List.hd ch))
      | Right -> boundary_tiles_ordered (not is_h) Right (snd (List.nth ch (k - 1)))
      | Top -> List.concat_map (fun (_, c) -> boundary_tiles_ordered (not is_h) Top c) ch
      | Bottom -> List.concat_map (fun (_, c) -> boundary_tiles_ordered (not is_h) Bottom c) ch)

(* Multiplicity of a weak guillotine tiling = product over all internal
   boundaries of C(a+b, a), where a,b = number of perpendicular segments
   terminating at the boundary from each side.  Perpendicular segments
   include those from nested levels that transitively reach the boundary.
   Count = len(boundary_tiles_ordered(child, facing_side)) - 1.
   (Asinowski et al. 2024, §5.3). *)
let multiplicity tiling =
  let rec go is_h = function
    | Schrot.Tile _ -> 1
    | Schrot.Frame children ->
      let ch = List2.to_list children in
      let child_prod = List.fold_left (fun acc (_, c) ->
        acc * go (not is_h) c) 1 ch in
      let boundary_prod = ref 1 in
      let rec pairs = function
        | [] | [_] -> ()
        | (_, c_i) :: (((_, c_next) :: _) as rest) ->
          let a, b =
            if is_h then
              (List.length (boundary_tiles_ordered (not is_h) Bottom c_i) - 1,
               List.length (boundary_tiles_ordered (not is_h) Top c_next) - 1)
            else
              (List.length (boundary_tiles_ordered (not is_h) Right c_i) - 1,
               List.length (boundary_tiles_ordered (not is_h) Left c_next) - 1)
          in
          boundary_prod := !boundary_prod * binom (a + b) a;
          pairs rest
      in
      pairs ch;
      child_prod * !boundary_prod
  in
  go (is_h tiling) (tree tiling)

(* Non-generic multiplicity: same structure but uses Delannoy numbers
   D(a,b) instead of C(a+b, a).  Counts strong classes including those
   where perpendicular segments coincide (4-way junctions allowed). *)
let multiplicity_nongeneric tiling =
  let rec go is_h = function
    | Schrot.Tile _ -> 1
    | Schrot.Frame children ->
      let ch = List2.to_list children in
      let child_prod = List.fold_left (fun acc (_, c) ->
        acc * go (not is_h) c) 1 ch in
      let boundary_prod = ref 1 in
      let rec pairs = function
        | [] | [_] -> ()
        | (_, c_i) :: (((_, c_next) :: _) as rest) ->
          let a, b =
            if is_h then
              (List.length (boundary_tiles_ordered (not is_h) Bottom c_i) - 1,
               List.length (boundary_tiles_ordered (not is_h) Top c_next) - 1)
            else
              (List.length (boundary_tiles_ordered (not is_h) Right c_i) - 1,
               List.length (boundary_tiles_ordered (not is_h) Left c_next) - 1)
          in
          boundary_prod := !boundary_prod * delannoy a b;
          pairs rest
      in
      pairs ch;
      child_prod * !boundary_prod
  in
  go (is_h tiling) (tree tiling)

(* Non-generic interleaving enumeration.  Like enumerate_interleavings
   but also generates merges with coincident positions (1,1)-steps.
   A merge step is: `T (advance top), `B (advance bot), `C (both).
   When two cuts coincide, the facing tiles from BOTH sides change
   simultaneously — the 4 tiles at the junction share a single point. *)
let enumerate_interleavings_nongeneric top_tiles bot_tiles =
  let a = List.length top_tiles - 1 in
  let b = List.length bot_tiles - 1 in
  let top_arr = Array.of_list top_tiles in
  let bot_arr = Array.of_list bot_tiles in
  let mk x y = if x < y then (x, y) else (y, x) in
  let rec gen_merges n_t n_b =
    if n_t = 0 && n_b = 0 then [[]]
    else
      let take_t = if n_t > 0 then
        List.map (fun m -> `T :: m) (gen_merges (n_t - 1) n_b)
      else [] in
      let take_b = if n_b > 0 then
        List.map (fun m -> `B :: m) (gen_merges n_t (n_b - 1))
      else [] in
      let take_c = if n_t > 0 && n_b > 0 then
        List.map (fun m -> `C :: m) (gen_merges (n_t - 1) (n_b - 1))
      else [] in
      take_t @ take_b @ take_c
  in
  let edges_of_merge merge =
    let edges = ref [] in
    let ti = ref 0 and bi = ref 0 in
    edges := mk top_arr.(!ti) bot_arr.(!bi) :: !edges;
    List.iter (fun step ->
      (match step with
       | `T -> incr ti
       | `B -> incr bi
       | `C -> incr ti; incr bi);
      edges := mk top_arr.(!ti) bot_arr.(!bi) :: !edges
    ) merge;
    List.sort_uniq compare !edges
  in
  List.map edges_of_merge (gen_merges a b)

(* Enumerate all non-generic strong classes of a tiling. *)
let enumerate_all_strong_adjacencies_nongeneric tiling =
  let boundaries = ref [] in
  let rec collect is_h = function
    | Schrot.Tile _ -> ()
    | Schrot.Frame children ->
      let ch = List2.to_list children in
      List.iter (fun (_, c) -> collect (not is_h) c) ch;
      let rec pairs = function
        | [] | [_] -> ()
        | (_, c_i) :: (((_, c_next) :: _) as rest) ->
          let top_tiles, bot_tiles =
            if is_h then
              (boundary_tiles_ordered (not is_h) Bottom c_i,
               boundary_tiles_ordered (not is_h) Top c_next)
            else
              (boundary_tiles_ordered (not is_h) Right c_i,
               boundary_tiles_ordered (not is_h) Left c_next)
          in
          boundaries := (top_tiles, bot_tiles) :: !boundaries;
          pairs rest
      in
      pairs ch
  in
  collect (is_h tiling) (tree tiling);
  let all_boundaries = !boundaries in
  let all_interleavings = List.map (fun (top_tiles, bot_tiles) ->
    enumerate_interleavings_nongeneric top_tiles bot_tiles
  ) all_boundaries in
  let rec cartesian = function
    | [] -> [EdgeSet.empty]
    | choices :: rest ->
      let rest_products = cartesian rest in
      List.concat_map (fun edge_list ->
        let edge_set = List.fold_left (fun s e ->
          EdgeSet.add e s) EdgeSet.empty edge_list in
        List.map (fun rp -> EdgeSet.union edge_set rp) rest_products
      ) choices
  in
  let all_sets = cartesian all_interleavings in
  List.map EdgeSet.elements all_sets

(* Enumerate all C(a+b, a) interleavings of a top-cuts and b bot-cuts
   along a boundary.  [top_tiles] has a+1 tiles, [bot_tiles] has b+1.
   Each interleaving merges the a+b cuts into a sequence; in each
   resulting interval, the facing top-tile and bot-tile are adjacent.
   Returns a list of (adjacency edge list), one per interleaving. *)
let enumerate_interleavings top_tiles bot_tiles =
  let a = List.length top_tiles - 1 in
  let b = List.length bot_tiles - 1 in
  let top_arr = Array.of_list top_tiles in
  let bot_arr = Array.of_list bot_tiles in
  let mk x y = if x < y then (x, y) else (y, x) in
  let rec gen_merges n_t n_b =
    if n_t = 0 && n_b = 0 then [[]]
    else
      let take_t = if n_t > 0 then
        List.map (fun m -> true :: m) (gen_merges (n_t - 1) n_b)
      else [] in
      let take_b = if n_b > 0 then
        List.map (fun m -> false :: m) (gen_merges n_t (n_b - 1))
      else [] in
      take_t @ take_b
  in
  let edges_of_merge merge =
    let edges = ref [] in
    let ti = ref 0 and bi = ref 0 in
    edges := mk top_arr.(!ti) bot_arr.(!bi) :: !edges;
    List.iter (fun is_top_cut ->
      if is_top_cut then incr ti else incr bi;
      edges := mk top_arr.(!ti) bot_arr.(!bi) :: !edges
    ) merge;
    List.sort_uniq compare !edges
  in
  List.map edges_of_merge (gen_merges a b)

(* Enumerate ALL strong equivalence classes of a tiling by generating
   all interleaving choices at every internal boundary.  Returns a list
   of adjacency edge sets. *)
let enumerate_all_strong_adjacencies tiling =
  let boundaries = ref [] in
  let rec collect is_h = function
    | Schrot.Tile _ -> ()
    | Schrot.Frame children ->
      let ch = List2.to_list children in
      List.iter (fun (_, c) -> collect (not is_h) c) ch;
      let rec pairs = function
        | [] | [_] -> ()
        | (_, c_i) :: (((_, c_next) :: _) as rest) ->
          let top_tiles, bot_tiles =
            if is_h then
              (boundary_tiles_ordered (not is_h) Bottom c_i,
               boundary_tiles_ordered (not is_h) Top c_next)
            else
              (boundary_tiles_ordered (not is_h) Right c_i,
               boundary_tiles_ordered (not is_h) Left c_next)
          in
          boundaries := (top_tiles, bot_tiles) :: !boundaries;
          pairs rest
      in
      pairs ch
  in
  collect (is_h tiling) (tree tiling);
  let all_boundaries = !boundaries in
  let all_interleavings = List.map (fun (top_tiles, bot_tiles) ->
    enumerate_interleavings top_tiles bot_tiles
  ) all_boundaries in
  let rec cartesian = function
    | [] -> [EdgeSet.empty]
    | choices :: rest ->
      let rest_products = cartesian rest in
      List.concat_map (fun edge_list ->
        let edge_set = List.fold_left (fun s e ->
          EdgeSet.add e s) EdgeSet.empty edge_list in
        List.map (fun rp -> EdgeSet.union edge_set rp) rest_products
      ) choices
  in
  let all_sets = cartesian all_interleavings in
  List.map EdgeSet.elements all_sets

(* --- Cross junction diagonal pairs --- *)

(* For each cross junction, identify the two diagonal tile pairs.
   At point P = (px/pd, qx/qd), classify each of the 4 tiles by
   which corner touches P:
     NW: P at tile's SE corner (x_right = px, y_bottom = qx)
     NE: P at tile's SW corner (x_left = px, y_bottom = qx)
     SW: P at tile's NE corner (x_right = px, y_top = qx)
     SE: P at tile's NW corner (x_left = px, y_top = qx)
   Diagonals: (NW, SE) and (NE, SW). *)
let diagonal_pairs tiling =
  let junctions = cross_junction_tiles tiling in
  List.map (fun (pt, tile_ids) ->
    let nw = ref (-1) and ne = ref (-1)
    and sw = ref (-1) and se = ref (-1) in
    List.iter (fun n ->
      let iv_h = tile_interval ~axis:H n tiling in
      let iv_v = tile_interval ~axis:V n tiling in
      (* x_left = iv_v.num/iv_v.den, x_right = (iv_v.num+1)/iv_v.den *)
      (* y_top = iv_h.num/iv_h.den, y_bottom = (iv_h.num+1)/iv_h.den *)
      let x_at_left = compare_rat (iv_v.num, iv_v.den) (pt.px, pt.pd) = 0 in
      let x_at_right = compare_rat (iv_v.num + 1, iv_v.den) (pt.px, pt.pd) = 0 in
      let y_at_top = compare_rat (iv_h.num, iv_h.den) (pt.qx, pt.qd) = 0 in
      let y_at_bottom = compare_rat (iv_h.num + 1, iv_h.den) (pt.qx, pt.qd) = 0 in
      match x_at_left, x_at_right, y_at_top, y_at_bottom with
      | _, true, _, true -> nw := n   (* P at SE corner *)
      | true, _, _, true -> ne := n   (* P at SW corner *)
      | _, true, true, _ -> sw := n   (* P at NE corner *)
      | true, _, true, _ -> se := n   (* P at NW corner *)
      | _ -> failwith (Printf.sprintf
          "diagonal_pairs: tile %d has no corner at junction" n)
    ) tile_ids;
    let mk a b = if a < b then (a, b) else (b, a) in
    (mk !nw !se, mk !ne !sw)
  ) junctions

(* Enumerate all 2^k strong adjacency edge sets for a tiling with k
   cross junctions.  [geo_edges] is the default geometric adjacency
   (from Geom.edges).  For k=0, returns [geo_edges] unchanged.

   At each cross junction, one of three cases:
   - d1 in geo, d2 not: resolved, d1 is the default diagonal
   - d2 in geo, d1 not: resolved, d2 is the default diagonal
   - neither in geo: unresolved (nested junction, resolve_splits averaged
     targets); treat as needing both choices added to the base set *)
let enumerate_strong_adjacencies ~geo_edges tiling =
  let diags = diagonal_pairs tiling in
  let k = List.length diags in
  if k = 0 then
    [geo_edges]
  else
    let geo_set = List.fold_left (fun s e -> EdgeSet.add e s)
      EdgeSet.empty geo_edges in
    (* Classify each junction: Resolved(chosen, unchosen) | Unresolved(d1, d2) *)
    let classified = List.map (fun (d1, d2) ->
      let d1_in = EdgeSet.mem d1 geo_set in
      let d2_in = EdgeSet.mem d2 geo_set in
      match d1_in, d2_in with
      | true, false -> `Resolved (d1, d2)
      | false, true -> `Resolved (d2, d1)
      | false, false -> `Unresolved (d1, d2)
      | true, true -> `Both_present (d1, d2)
    ) diags in
    let n_choices = 1 lsl k in
    List.init n_choices (fun mask ->
      let edges = List.fold_left (fun s (i, cls) ->
        match cls with
        | `Resolved (chosen, unchosen) ->
          if mask land (1 lsl i) = 0 then s
          else s |> EdgeSet.remove chosen |> EdgeSet.add unchosen
        | `Unresolved (d1, d2) ->
          (* Neither present: add one or the other *)
          if mask land (1 lsl i) = 0 then EdgeSet.add d1 s
          else EdgeSet.add d2 s
        | `Both_present (d1, _d2) ->
          (* Both already present (shouldn't happen normally) *)
          if mask land (1 lsl i) = 0 then s
          else EdgeSet.remove d1 s
      ) geo_set (List.mapi (fun i x -> (i, x)) classified) in
      EdgeSet.elements edges
    )

(* --- D4 symmetry group --- *)

(* 90° CW rotation of the unit square:
   H(c1,...,ck) [top→bottom] becomes V(ck',...,c1') [right→left = reversed]
   V(c1,...,ck) [left→right] becomes H(c1',...,ck') [top→bottom = same order] *)
let rot90 (is_h, tree) =
  let rec go parent_is_h = function
    | Schrot.Tile n -> Schrot.Tile n
    | Schrot.Frame children ->
      let children' = List2.map (fun (w, c) -> (w, go (not parent_is_h) c)) children in
      if parent_is_h then Schrot.Frame (List2.rev children')
      else Schrot.Frame children'
  in
  (not is_h, go is_h tree)

(* 180° rotation: reverse children at every node *)
let rot180 (is_h, tree) =
  let rec go = function
    | Schrot.Tile n -> Schrot.Tile n
    | Schrot.Frame children -> Schrot.Frame (List2.rev (List2.map (fun (w, c) -> (w, go c)) children))
  in
  (is_h, go tree)

(* Horizontal reflection (flip top/bottom):
   reverse children at H-nodes only *)
let flip_h (is_h, tree) =
  let rec go cur_is_h = function
    | Schrot.Tile n -> Schrot.Tile n
    | Schrot.Frame children ->
      let children' = List2.map (fun (w, c) -> (w, go (not cur_is_h) c)) children in
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

(* Named D4 actions for Burnside decomposition.
   Order: id, rot90, rot180, rot270, flip_h, flip_v, diag_NE, diag_NW. *)
let d4_actions : (string * (t -> t)) array =
  let rot270 t = rot90 (rot90 (rot90 t)) in
  let flip_v t = rot180 (flip_h t) in
  let diag_ne t = rot90 (flip_h t) in
  let diag_nw t = rot270 (flip_h t) in
  [| ("id", Fun.id); ("rot90", rot90); ("rot180", rot180); ("rot270", rot270);
     ("flip_h", flip_h); ("flip_v", flip_v); ("diag_ne", diag_ne); ("diag_nw", diag_nw) |]

(* Klein four-group V4 = {id, rot180, flip_h, flip_v}.
   Preserves cut orientations (H stays H, V stays V). *)
let v4_orbit t =
  let r2 = rot180 t in
  let fh = flip_h t in
  let fv = flip_h r2 in (* flip_v = rot180 . flip_h *)
  [t; r2; fh; fv]

(* Erase leaf labels *)
let erase (is_h, tree) = (is_h, Schrot.map (fun _ -> ()) tree)

(* Separable permutation corresponding to a tiling.
   V (direct sum): children get ascending value ranges.
   H (skew sum): children get descending value ranges.
   Positions = DFS leaf order (0..n-1). *)
let to_perm (is_h_root, tree) =
  let n = Schrot.size tree in
  let perm = Array.make n 0 in
  let rec go pos lo hi is_h = function
    | Schrot.Tile _ ->
      assert (lo = hi);
      perm.(pos) <- lo
    | Schrot.Frame children ->
      let children_list = List2.to_list children in
      let sizes = List.map (fun (_, c) -> Schrot.size c) children_list in
      let k = List.length sizes in
      let pos_off = Array.make (k + 1) pos in
      List.iteri (fun i s -> pos_off.(i + 1) <- pos_off.(i) + s) sizes;
      let val_ranges = Array.make k (0, 0) in
      if is_h then begin
        let v = ref hi in
        List.iteri (fun i s ->
          val_ranges.(i) <- (!v - s + 1, !v);
          v := !v - s
        ) sizes
      end else begin
        let v = ref lo in
        List.iteri (fun i s ->
          val_ranges.(i) <- (!v, !v + s - 1);
          v := !v + s
        ) sizes
      end;
      List.iteri (fun i (_, child) ->
        let (vlo, vhi) = val_ranges.(i) in
        go pos_off.(i) vlo vhi (not is_h) child
      ) children_list
  in
  go 0 0 (n - 1) is_h_root tree;
  perm

(* String representation of a unit tiling (erased labels) *)
let rec unit_tree_to_string is_h = function
  | Schrot.Tile () -> "*"
  | Schrot.Frame children ->
    let tag = if is_h then "h" else "v" in
    let ch = List2.to_list children
      |> List.map (fun (_, c) -> unit_tree_to_string (not is_h) c)
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
        | (_, x) :: xs, (_, y) :: ys ->
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
    let ch = List2.map (fun (w, c) -> (w, canonical_planar c)) children in
    let rev_ch = List2.rev ch in
    let fwd = List2.to_list ch in
    let bwd = List2.to_list rev_ch in
    let rec lex a b = match a, b with
      | [], [] -> 0
      | [], _ -> -1 | _, [] -> 1
      | (_, x) :: xs, (_, y) :: ys ->
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
    let ch = List2.map (fun (w, c) -> (w, canonical_unordered c)) children in
    let sorted = List2.to_list ch |> List.sort (fun (_, x) (_, y) -> compare_unit_tree x y) in
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
      Schrot.Frame (List2.map (fun (w, c) -> (w, go c)) children)
  in
  (is_h, go tree)

(* --- Layer 2: Quotientope flips --- *)

(* Two distinct flip families coexist in this type, from two different papers:

   (a) Asinowski et al. 2024 Theorem 22 / Figure 19 defines 5 flip types on
       the *strong* poset (guillotine-only, windmill-avoiding per Theorem 27):
       L-B pivoting, R-T pivoting, simple flip, V-slide, H-slide.  These are
       cover relations of a lattice, closed on guillotine by construction.
       Under D4 they reduce to 3 orbits: simple, pivot, wall slide.  The
       `Simple_*`, `Pivot_*`, and `Wall_slide` variants mirror those 3 orbits.

   (b) Merino-Mutze 2021 §2.2 defines a T-flip on *generic* rectangulations
       (which include windmills).  Theorem 19 guarantees invertibility in the
       generic space, but an M-M T-flip applied to a guillotine input can
       produce a windmill output — at n=7, 150 of 1782 flips do so.  The
       `T_flip` variant is populated by `Geom.enumerate_t_flips` and is
       therefore the M-M operation, not Asinowski's pivoting.

   The two coincide exactly when the M-M T-flip's output stays guillotine;
   otherwise they diverge.  A tree-level T-flip that preserves the Schroder
   representation must be the Asinowski restriction (M-M ∩ guillotine).  See
   FLIP_INVERTIBILITY.md Round 6 and backlog.md Round 7. *)

type flip =
  | Simple_dissolve of int
  | Simple_create of int * int
  | Pivot_out of int
  | Pivot_in of int * int  (* (tile to push, tile to pair with) *)
  | Wall_slide of int * int
  | T_flip of int * int  (* (stem_tile, bar_tile) *)

let rec contains_leaf n = function
  | Schrot.Tile k -> k = n
  | Schrot.Frame children ->
    List2.exists (fun (_, c) -> contains_leaf n c) children

(* Wall slide: swap two consecutive children within a Frame.
   [a] and [b] identify the two children by a leaf they contain.
   Self-inverse. *)
let wall_slide a b t =
  let rec go_tree = function
    | Schrot.Tile _ -> None
    | Schrot.Frame children ->
      let ch = List2.to_list children in
      match go_children ch with
      | Some ch' ->
        (match List2.of_list_opt ch' with
         | Some ch2 -> Some (Schrot.Frame ch2)
         | None -> assert false)
      | None -> None
  and go_children = function
    | [] -> None
    | [_] -> None
    | ((wx, x) as px) :: ((wy, y) as py) :: rest ->
      let xa = contains_leaf a x and xb = contains_leaf b x in
      let ya = contains_leaf a y and yb = contains_leaf b y in
      if (xa && yb) || (xb && ya) then
        (* a and b are in consecutive children x, y: swap *)
        Some (py :: px :: rest)
      else if xa && xb then
        (* both in x: recurse into x *)
        (match go_tree x with
         | Some x' -> Some ((wx, x') :: (wy, y) :: rest)
         | None -> None)
      else
        (* try further along *)
        (match go_children ((wy, y) :: rest) with
         | Some rest' -> Some ((wx, x) :: rest')
         | None -> None)
  in
  let is_h, tree = t in
  match go_tree tree with
  | Some tree' -> Some (is_h, tree')
  | None -> None

(* Simple dissolve: dissolve a 2-ary Frame whose children are both Tiles.
   At root: flip is_h.  At non-root: splice the two Tiles into the parent. *)
let simple_dissolve n t =
  let is_h, tree = t in
  match tree with
  | Schrot.Frame (Cons2 ((_, Tile a), (_, Tile b), []))
    when a = n || b = n ->
    (* Root case: flip is_h *)
    Some (not is_h, tree)
  | Schrot.Frame _ ->
    (* Non-root: find and dissolve in subtree *)
    let rec go_list = function
      | [] -> None
      | (w, child) :: rest ->
        match child with
        | Schrot.Frame (Cons2 ((_, Tile a), (_, Tile b), []))
          when a = n || b = n ->
          (* Found: splice the two Tiles into parent's list *)
          Some (((), Schrot.Tile a) :: ((), Schrot.Tile b) :: rest)
        | Schrot.Frame sub ->
          (match go_frame sub with
           | Some child' -> Some ((w, child') :: rest)
           | None ->
             match go_list rest with
             | Some rest' -> Some ((w, child) :: rest')
             | None -> None)
        | Schrot.Tile _ ->
          (match go_list rest with
           | Some rest' -> Some ((w, child) :: rest')
           | None -> None)
    and go_frame children =
      match go_list (List2.to_list children) with
      | None -> None
      | Some new_list ->
        match List2.of_list_opt new_list with
        | Some ch -> Some (Schrot.Frame ch)
        | None -> assert false
    in
    (match go_frame (match tree with Schrot.Frame ch -> ch | _ -> assert false) with
     | Some tree' -> Some (is_h, tree')
     | None -> None)
  | Schrot.Tile _ -> None

(* Simple create: wrap two consecutive Tile siblings into a new 2-ary sub-Frame.
   Parent must have >= 3 children. *)
let simple_create a b t =
  let rec go_tree = function
    | Schrot.Tile _ -> None
    | Schrot.Frame children ->
      let ch = List2.to_list children in
      let len = List.length ch in
      match go_create ch len with
      | Some ch' ->
        (match List2.of_list_opt ch' with
         | Some ch2 -> Some (Schrot.Frame ch2)
         | None -> assert false)
      | None ->
        (* Recurse into children *)
        (match go_children ch with
         | Some ch' ->
           (match List2.of_list_opt ch' with
            | Some ch2 -> Some (Schrot.Frame ch2)
            | None -> assert false)
         | None -> None)
  and go_create ch len = match ch with
    | [] | [_] -> None
    | (_, Schrot.Tile x) :: (_, Schrot.Tile y) :: rest
      when ((x = a && y = b) || (x = b && y = a)) && len >= 3 ->
      let wrapped = Schrot.unit_frame (List2.Cons2 (Tile x, Tile y, [])) in
      Some (((), wrapped) :: rest)
    | x :: rest ->
      (match go_create rest len with
       | Some rest' -> Some (x :: rest')
       | None -> None)
  and go_children = function
    | [] -> None
    | (w, child) :: rest ->
      (match go_tree child with
       | Some child' -> Some ((w, child') :: rest)
       | None ->
         match go_children rest with
         | Some rest' -> Some ((w, child) :: rest')
         | None -> None)
  in
  let is_h, tree = t in
  match go_tree tree with
  | Some tree' -> Some (is_h, tree')
  | None -> None

(* Pivot out: extract boundary leaf [n] from its parent Frame S into
   the grandparent Frame G.  At non-root, the result splices into the
   great-grandparent because its orientation matches. *)
let pivot_out n t =
  (* Try to pivot leaf [n] out of a child Frame S within child list [ch].
     Returns Some of the expanded child list (S replaced by [Tile n; S'] or
     [S'; Tile n] inline), or None.  Tile n is placed adjacent to the
     shrunken S', matching the paper's T-flip: move a boundary tile one
     level up from child Frame to parent Frame. *)
  let try_pivot_from_children ch =
    let rec scan before = function
      | [] -> None
      | ((_w, Schrot.Frame sub_ch) as pair) :: after ->
        let sub = List2.to_list sub_ch in
        let len = List.length sub in
        let first_is_n = match sub with
          | (_, Schrot.Tile k) :: _ -> k = n
          | _ -> false
        in
        let last_is_n = match List.rev sub with
          | (_, Schrot.Tile k) :: _ -> k = n
          | _ -> false
        in
        if first_is_n && len >= 2 then begin
          (* n is first child of S.  Remove it, place [Tile n; S'] in parent. *)
          let remainder = List.tl sub in
          let s' = match remainder with
            | [(_w, single)] -> single
            | a :: b :: rest -> Schrot.Frame (List2.Cons2 (a, b, rest))
            | [] -> assert false
          in
          Some (List.rev_append before
            (((), Schrot.Tile n) :: ((), s') :: after))
        end
        else if last_is_n && len >= 2 then begin
          (* n is last child of S.  Remove it, place [S'; Tile n] in parent. *)
          let all_but_last = List.filteri (fun i _ -> i < len - 1) sub in
          let s' = match all_but_last with
            | [(_w, single)] -> single
            | a :: b :: rest -> Schrot.Frame (List2.Cons2 (a, b, rest))
            | [] -> assert false
          in
          Some (List.rev_append before
            (((), s') :: ((), Schrot.Tile n) :: after))
        end
        else
          scan (pair :: before) after
      | pair :: after ->
        scan (pair :: before) after
    in
    scan [] ch
  in
  let list_to_tree = function
    | [(_w, single)] -> single
    | a :: b :: rest -> Schrot.Frame (List2.Cons2 (a, b, rest))
    | [] -> assert false
  in
  (* Recursively descend: at each Frame, check if any child Frame S
     has n as a boundary child.  If yes, return the rebuilt Frame.
     If no, recurse into children. *)
  let rec go_tree = function
    | Schrot.Tile _ -> None
    | Schrot.Frame children ->
      let ch = List2.to_list children in
      match try_pivot_from_children ch with
      | Some expanded_ch ->
        Some (`Tree (list_to_tree expanded_ch))
      | None ->
        go_children_deep [] ch
  and go_children_deep before = function
    | [] -> None
    | (w, child) :: after ->
      (match go_tree child with
       | Some (`Tree child') ->
         let new_ch = List.rev_append before ((w, child') :: after) in
         Some (`Tree (list_to_tree new_ch))
       | None ->
         go_children_deep ((w, child) :: before) after)
  in
  let is_h, tree = t in
  match tree with
  | Schrot.Tile _ -> None
  | Schrot.Frame children ->
    let ch = List2.to_list children in
    match try_pivot_from_children ch with
    | Some expanded_ch ->
      (* Root is G.  Expanded children form the new root (same is_h),
         or collapse with flipped is_h if only one child remains. *)
      (match expanded_ch with
       | [(_w, single)] -> Some (not is_h, single)
       | a :: b :: rest ->
         Some (is_h, Schrot.Frame (List2.Cons2 (a, b, rest)))
       | [] -> assert false)
    | None ->
      (match go_children_deep [] ch with
       | Some (`Tree tree') ->
         Some (is_h, tree')
       | None -> None)

(* Root boundary extraction: n is a direct boundary Tile child of a
   >=3-ary root Frame.  Extract n and form a 2-ary root with flipped is_h.
   [place] controls whether Tile n goes Before or After the remainder. *)
let pivot_out_root n place t =
  let is_h, tree = t in
  match tree with
  | Schrot.Tile _ -> None
  | Schrot.Frame children ->
    let ch = List2.to_list children in
    let len = List.length ch in
    if len < 3 then None
    else
      let first_is_n = match ch with
        | (_, Schrot.Tile k) :: _ -> k = n | _ -> false in
      let last_is_n = match List.rev ch with
        | (_, Schrot.Tile k) :: _ -> k = n | _ -> false in
      if first_is_n || last_is_n then
        let rest =
          if first_is_n then List.tl ch
          else List.filteri (fun i _ -> i < len - 1) ch
        in
        let root' = match rest with
          | a :: b :: r -> Schrot.Frame (List2.Cons2 (a, b, r))
          | _ -> assert false in
        let pair =
          if place = Before
          then (((), Schrot.Tile n), ((), root'))
          else (((), root'), ((), Schrot.Tile n))
        in
        let (a, b) = pair in
        Some (not is_h, Schrot.Frame (List2.Cons2 (a, b, [])))
      else None

(* Pivot-in-root: inverse of pivot_out_root.  Dissolve a 2-ary root,
   flip is_h, and insert Tile n into the surviving child's child list
   at a boundary position determined by [place]. *)
let pivot_in_root n place t =
  let is_h, tree = t in
  match tree with
  | Schrot.Frame (List2.Cons2 ((_, Schrot.Tile k), (_, Schrot.Frame ch), [])) when k = n ->
    let children = List2.to_list ch in
    let new_ch =
      if place = Before then ((), Schrot.Tile n) :: children
      else children @ [((), Schrot.Tile n)]
    in
    (match List2.of_list_opt new_ch with
     | Some c -> Some (not is_h, Schrot.Frame c)
     | None -> assert false)
  | Schrot.Frame (List2.Cons2 ((_, Schrot.Frame ch), (_, Schrot.Tile k), [])) when k = n ->
    let children = List2.to_list ch in
    let new_ch =
      if place = Before then ((), Schrot.Tile n) :: children
      else children @ [((), Schrot.Tile n)]
    in
    (match List2.of_list_opt new_ch with
     | Some c -> Some (not is_h, Schrot.Frame c)
     | None -> assert false)
  | _ -> None

(* Pivot in: two modes.
   Wrap mode: Tile [n] adjacent to a Frame G in some parent P (P must have
     >=3 children so removing n doesn't collapse P).  Find any direct child
     of G that contains leaf [m], wrap it with Tile [n] in a 2-ary sub-frame.
   Insert mode: Tile [n] adjacent to a Frame G in parent P.  Tile [m] must be
     a direct boundary Tile child of G.  Remove n from P, insert into G. *)
let pivot_in n m t =
  (* --- Merge mode: T-flip merges an edge into an existing wall.
     Find the child of G containing m.  If that child is a Frame whose
     boundary child (first when n_before, last otherwise) contains m,
     insert Tile n into that Frame — growing it by one child.  This
     corresponds to the paper's "edge merges with wall t." *)
  let rec merge_tree ~at_root = function
    | Schrot.Tile _ -> None
    | Schrot.Frame children ->
      let ch = List2.to_list children in
      let parent_len = List.length ch in
      (match merge_scan ~at_root parent_len [] ch with
       | Some result -> Some result
       | None -> merge_children [] ch)
  and merge_scan ~at_root parent_len before = function
    | [] -> None
    | (_, Schrot.Tile k) :: after when k = n ->
      if (not at_root) && parent_len < 3 then None
      else
      let try_after = match after with
        | (_, Schrot.Frame sub_ch) :: rest ->
          let sub = List2.to_list sub_ch in
          find_and_merge_in sub true (List.rev_append before) rest
        | _ -> None
      in
      let try_before () = match before with
        | (_, Schrot.Frame sub_ch) :: rest_before ->
          let sub = List2.to_list sub_ch in
          find_and_merge_in sub false (List.rev_append rest_before) after
        | _ -> None
      in
      (match try_after with Some _ as r -> r | None -> try_before ())
    | x :: after -> merge_scan ~at_root parent_len (x :: before) after
  and find_and_merge_in sub n_before mk_parent rest =
    let rec scan_sub before_sub = function
      | [] -> None
      | (w, child) :: after_sub ->
        if contains_leaf m child then
          let merged = match child with
            | Schrot.Frame ch ->
              let children = List2.to_list ch in
              let len = List.length children in
              let bi = if n_before then 0 else len - 1 in
              let (_, bc) = List.nth children bi in
              if contains_leaf m bc then
                let new_ch =
                  if n_before then ((), Schrot.Tile n) :: children
                  else children @ [((), Schrot.Tile n)]
                in
                (match List2.of_list_opt new_ch with
                 | Some c -> Some (Schrot.Frame c) | None -> assert false)
              else None
            | _ -> None  (* Tile m: handled by insert mode *)
          in
          (match merged with
           | Some merged_child ->
             let new_sub = List.rev_append before_sub
               ((w, merged_child) :: after_sub) in
             let nf = match List2.of_list_opt new_sub with
               | Some c -> Schrot.Frame c | None -> assert false in
             rebuild (mk_parent (((), nf) :: rest))
           | None -> None)
        else
          scan_sub ((w, child) :: before_sub) after_sub
    in
    scan_sub [] sub
  and rebuild new_ch =
    match List2.of_list_opt new_ch with
    | Some ch2 -> Some (`Tree (Schrot.Frame ch2))
    | None -> (match new_ch with [(_w, s)] -> Some (`Collapse s) | _ -> assert false)
  and merge_children before = function
    | [] -> None
    | (w, child) :: after ->
      (match merge_tree ~at_root:false child with
       | Some (`Tree c) | Some (`Collapse c) ->
         let nc = List.rev_append before ((w, c) :: after) in
         rebuild nc
       | None -> merge_children ((w, child) :: before) after)
  in
  (* --- Insert mode: Tile n adjacent to Frame G in parent, Tile m is a
     direct boundary child of G.  Remove n from parent, insert n into G
     next to m (growing G by 1). --- *)
  let rec insert_tree ~at_root = function
    | Schrot.Tile _ -> None
    | Schrot.Frame children ->
      let ch = List2.to_list children in
      let parent_len = List.length ch in
      (match insert_scan ~at_root parent_len [] ch with
       | Some result -> Some result
       | None -> insert_children [] ch)
  and insert_scan ~at_root parent_len before = function
    | [] -> None
    | (_, Schrot.Tile k) :: after when k = n ->
      if (not at_root) && parent_len < 3 then None
      else
      (* Lemma 3(c): only insert at the near boundary — the side of the
         Frame that faces Tile n.  Inserting at the far boundary would be
         a non-minimal jump. *)
      let try_after = match after with
        | (_, Schrot.Frame sub_ch) :: rest ->
          let sub = List2.to_list sub_ch in
          let first_is_m = (match sub with (_, Schrot.Tile j) :: _ -> j = m | _ -> false) in
          if first_is_m then
            (* n before Frame, m at near boundary: insert n before m *)
            let new_sub = ((), Schrot.Tile n) :: sub in
            let nf = match List2.of_list_opt new_sub with Some c -> Schrot.Frame c | None -> assert false in
            rebuild (List.rev_append before (((), nf) :: rest))
          else None
        | _ -> None
      in
      let try_before () = match before with
        | (_, Schrot.Frame sub_ch) :: rest_before ->
          let sub = List2.to_list sub_ch in
          let last_is_m = (match List.rev sub with (_, Schrot.Tile j) :: _ -> j = m | _ -> false) in
          if last_is_m then
            (* n after Frame, m at near boundary: insert n after m *)
            let new_sub = sub @ [((), Schrot.Tile n)] in
            let nf = match List2.of_list_opt new_sub with Some c -> Schrot.Frame c | None -> assert false in
            rebuild (List.rev_append rest_before (((), nf) :: after))
          else None
        | _ -> None
      in
      (match try_after with Some _ as r -> r | None -> try_before ())
    | x :: after -> insert_scan ~at_root parent_len (x :: before) after
  and insert_children before = function
    | [] -> None
    | (w, child) :: after ->
      (match insert_tree ~at_root:false child with
       | Some (`Tree c) | Some (`Collapse c) ->
         let nc = List.rev_append before ((w, c) :: after) in
         rebuild nc
       | None -> insert_children ((w, child) :: before) after)
  in
  (* Try merge first (edge merges into existing wall), then insert
     (m is a direct boundary child of G). *)
  let is_h, tree = t in
  let try_mode f =
    match f ~at_root:true tree with
    | Some (`Tree t') -> Some (is_h, t')
    | Some (`Collapse t') -> Some (not is_h, t')
    | None -> None
  in
  match try_mode merge_tree with
  | Some _ as r -> r
  | None -> try_mode insert_tree

(* Pivot in — wrap mode: Tile [n] adjacent to a Frame G in parent P
   (P must have >=3 children, or be root).  Find any direct child of G
   that contains leaf [m], wrap it with Tile [n] in a 2-ary sub-frame.
   This reverses pivot_out from a 2-ary sub-frame. *)
let pivot_in_wrap n m t =
  let rec wrap_tree ~at_root = function
    | Schrot.Tile _ -> None
    | Schrot.Frame children ->
      let ch = List2.to_list children in
      let parent_len = List.length ch in
      (match wrap_scan ~at_root parent_len [] ch with
       | Some result -> Some result
       | None -> wrap_children [] ch)
  and wrap_scan ~at_root parent_len before = function
    | [] -> None
    | (_, Schrot.Tile k) :: after when k = n ->
      if (not at_root) && parent_len < 3 then None
      else
      let try_after = match after with
        | (_, Schrot.Frame sub_ch) :: rest ->
          let sub = List2.to_list sub_ch in
          find_and_wrap_in sub true (List.rev_append before) rest
        | _ -> None
      in
      let try_before () = match before with
        | (_, Schrot.Frame sub_ch) :: rest_before ->
          let sub = List2.to_list sub_ch in
          find_and_wrap_in sub false (List.rev_append rest_before) after
        | _ -> None
      in
      (match try_after with Some _ as r -> r | None -> try_before ())
    | x :: after -> wrap_scan ~at_root parent_len (x :: before) after
  and find_and_wrap_in sub n_before mk_parent rest =
    let rec scan_sub before_sub = function
      | [] -> None
      | (w, child) :: after_sub ->
        if contains_leaf m child then
          let wrapped =
            if n_before
            then Schrot.unit_frame (List2.Cons2 (Schrot.Tile n, child, []))
            else Schrot.unit_frame (List2.Cons2 (child, Schrot.Tile n, []))
          in
          let new_sub = List.rev_append before_sub ((w, wrapped) :: after_sub) in
          let nf = match List2.of_list_opt new_sub with
            | Some c -> Schrot.Frame c | None -> assert false in
          rebuild (mk_parent (((), nf) :: rest))
        else
          scan_sub ((w, child) :: before_sub) after_sub
    in
    scan_sub [] sub
  and rebuild new_ch =
    match List2.of_list_opt new_ch with
    | Some ch2 -> Some (`Tree (Schrot.Frame ch2))
    | None -> (match new_ch with [(_w, s)] -> Some (`Collapse s) | _ -> assert false)
  and wrap_children before = function
    | [] -> None
    | (w, child) :: after ->
      (match wrap_tree ~at_root:false child with
       | Some (`Tree c) | Some (`Collapse c) ->
         let nc = List.rev_append before ((w, c) :: after) in
         rebuild nc
       | None -> wrap_children ((w, child) :: before) after)
  in
  let is_h, tree = t in
  let try_mode f =
    match f ~at_root:true tree with
    | Some (`Tree t') -> Some (is_h, t')
    | Some (`Collapse t') -> Some (not is_h, t')
    | None -> None
  in
  try_mode wrap_tree

(* Enumerate all applicable flips for a tiling and their results. *)
let enumerate_flips t =
  let results = ref [] in
  let add f t' = results := (f, t') :: !results in
  let the_leaves = leaves t in
  List.iter (fun n ->
    match simple_dissolve n t with
    | Some t' -> add (Simple_dissolve n) t'
    | None -> ()
  ) the_leaves;
  List.iter (fun a ->
    List.iter (fun b ->
      if a < b then
        match simple_create a b t with
        | Some t' -> add (Simple_create (a, b)) t'
        | None -> ()
    ) the_leaves
  ) the_leaves;
  (* pivot_out/pivot_in removed — replaced by Geom.enumerate_t_flips *)
  List.iter (fun a ->
    List.iter (fun b ->
      if a < b then
        match wall_slide a b t with
        | Some t' -> add (Wall_slide (a, b)) t'
        | None -> ()
    ) the_leaves
  ) the_leaves;
  (* Deduplicate by result *)
  let seen = Hashtbl.create 16 in
  List.filter (fun (_, t') ->
    let key = to_string t' in
    if Hashtbl.mem seen key then false
    else (Hashtbl.add seen key (); true)
  ) (List.rev !results)

let flip_to_string = function
  | Simple_dissolve n -> Printf.sprintf "dissolve %d" n
  | Simple_create (a, b) -> Printf.sprintf "create %d %d" a b
  | Pivot_out n -> Printf.sprintf "pivot_out %d" n
  | Pivot_in (n, m) -> Printf.sprintf "pivot_in %d %d" n m
  | Wall_slide (a, b) -> Printf.sprintf "slide %d %d" a b
  | T_flip (s, b) -> Printf.sprintf "t_flip %d %d" s b

(* Placeholder for future edge count oracle.
   Dedup makes site-counting unreliable (pivot_in and pivot_out can produce
   the same result).  Properties C + D together suffice for correctness. *)
let count_flip_sites _t = -1
