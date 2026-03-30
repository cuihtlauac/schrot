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

type interval = { num: int; den: int }

(* Tile interval on the given axis: [num/den, (num+1)/den).
   Walk root-to-tile, at each ancestor frame whose orientation matches the axis,
   accumulate num = num * arity + index, den = den * arity. *)
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

let intervals_overlap a b =
  let lo = max (a.num * b.den) (b.num * a.den) in
  let hi = min ((a.num + 1) * b.den) ((b.num + 1) * a.den) in
  hi > lo

(* Perturbed split ratios: slightly asymmetric to avoid accidental "+" junctions.
   The [parity] parameter flips the perturbation direction so sibling frames
   of the same arity use different ratios. *)
let split_positions ~parity k =
  if k <= 1 then [| 0.; 1. |]
  else
    let alpha = 0.15 in
    let sign = if parity then 1. else -1. in
    let weights = Array.init k (fun i ->
      1. +. sign *. alpha *. (2. *. float_of_int i /. float_of_int (k - 1) -. 1.)
    ) in
    let total = Array.fold_left ( +. ) 0. weights in
    let pos = Array.make (k + 1) 0. in
    for i = 0 to k - 1 do
      pos.(i + 1) <- pos.(i) +. weights.(i) /. total
    done;
    pos

(* Perturbed float interval for a tile on the given axis.
   Uses the same split_positions as the renderer. *)
let tile_interval_perturbed ~axis n tiling =
  let axis_is_h = (axis = H) in
  let rec go lo hi is_h parity = function
    | Schrot.Tile k ->
      if k = n then Some (lo, hi) else None
    | Schrot.Frame children ->
      let arity = List2.length children in
      let pos = split_positions ~parity arity in
      let rec try_children i = function
        | [] -> None
        | child :: rest ->
          let lo', hi', parity' =
            if is_h = axis_is_h then
              let new_lo = lo +. pos.(i) *. (hi -. lo) in
              let new_hi = lo +. pos.(i + 1) *. (hi -. lo) in
              (new_lo, new_hi, parity <> (i mod 2 = 0))
            else
              (lo, hi, parity <> (i mod 2 = 0))
          in
          (match go lo' hi' (not is_h) parity' child with
           | Some _ as r -> r
           | None -> try_children (i + 1) rest)
      in
      try_children 0 (List2.to_list children)
  in
  match go 0. 1. (is_h tiling) true (tree tiling) with
  | Some iv -> iv
  | None -> invalid_arg "Tiling.tile_interval_perturbed: tile not found"

let intervals_overlap_f (lo_a, hi_a) (lo_b, hi_b) =
  let eps = 1e-9 in
  min hi_a hi_b -. max lo_a lo_b > eps

(* --- LCA-based adjacency --- *)

type adjacency = North | South | East | West

(* Find the path from root to tile n as list of (index_in_parent, parent_is_h).
   Returns list from root to leaf. *)
let find_tile_path n tiling =
  let rec go is_h = function
    | Schrot.Tile k -> if k = n then Some [] else None
    | Schrot.Frame children ->
      let rec try_ch i = function
        | [] -> None
        | child :: rest ->
          match go (not is_h) child with
          | Some path -> Some ((i, is_h) :: path)
          | None -> try_ch (i + 1) rest
      in
      try_ch 0 (List2.to_list children)
  in
  go (is_h tiling) (tree tiling)

(* Find LCA of two tiles: returns (lca_is_h, child_index_of_a, child_index_of_b)
   where the indices are the children of the LCA frame containing a and b. *)
let find_lca a b tiling =
  match find_tile_path a tiling, find_tile_path b tiling with
  | Some pa, Some pb ->
    (* Walk both paths in parallel; they share a prefix up to the LCA *)
    let rec walk = function
      | (ia, _) :: ra, (ib, _) :: rb when ia = ib ->
        (* Same child, keep descending *)
        walk (ra, rb)
      | (ia, is_h) :: _, (ib, _) :: _ ->
        (* Different children: this is the LCA *)
        Some (is_h, ia, ib)
      | _ -> None (* one is ancestor of the other, or not found *)
    in
    walk (pa, pb)
  | _ -> None

let touches_cut lca_is_h ~first sub_path =
  (* [first] = true means the tile must touch the boundary toward the first
     child (top for H, left for V) of same-orientation frames.
     [first] = false means toward the last child. *)
  List.for_all (fun (idx, arity, frame_is_h) ->
    if frame_is_h <> lca_is_h then true
    else if first then idx = 0
    else idx = arity - 1
  ) sub_path

(* Depth of the LCA of two tiles (length of common path prefix) *)
let lca_depth a b tiling =
  match find_tile_path a tiling, find_tile_path b tiling with
  | Some pa, Some pb ->
    let rec walk depth pa pb =
      match pa, pb with
      | (ia, _) :: ra, (ib, _) :: rb when ia = ib -> walk (depth + 1) ra rb
      | _ -> depth
    in
    walk 0 pa pb
  | _ -> 0

let lca_adjacent_with_depth a b tiling =
  match find_tile_path a tiling, find_tile_path b tiling with
  | Some pa, Some pb ->
    let rec walk depth pa pb =
      match pa, pb with
      | (ia, _) :: ra, (ib, _) :: rb when ia = ib ->
        walk (depth + 1) ra rb
      | (ia, is_h) :: _, (ib, _) :: _ when abs (ia - ib) = 1 ->
        Some (depth, is_h, ia, ib)
      | _ -> None
    in
    (match walk 0 pa pb with
     | None -> None
     | Some (lca_depth, lca_is_h, ia, ib) ->
       (* Enrich sub-paths with arity info for touches_cut *)
       let enrich_path tile_id =
         let rec go is_h = function
           | Schrot.Tile _ -> []
           | Schrot.Frame children ->
             let arity = List2.length children in
             let rec try_ch i = function
               | [] -> []
               | child :: rest ->
                 let sub = go (not is_h) child in
                 if sub <> [] || (match child with Schrot.Tile k -> k = tile_id | _ -> false)
                 then (i, arity, is_h) :: sub
                 else try_ch (i + 1) rest
             in
             try_ch 0 (List2.to_list children)
         in
         go (is_h tiling) (tree tiling)
       in
       (* Get sub-paths with arity: strip common prefix, keep the rest *)
       let full_a = enrich_path a in
       let full_b = enrich_path b in
       let rec skip_common la lb = match la, lb with
         | (ia2, _, _) :: ra, (ib2, _, _) :: rb when ia2 = ib2 ->
           skip_common ra rb
         | _ :: ra, _ :: rb -> (ra, rb)  (* divergence point, skip it *)
         | _ -> ([], [])
       in
       let (sub_a_rich, sub_b_rich) = skip_common full_a full_b in
       (* Tile in child ia < ib must touch boundary toward child ib:
          that's the last (bottom/right) edge → first=false.
          Tile in child ib must touch boundary toward child ia:
          that's the first (top/left) edge → first=true. *)
       let a_touches = touches_cut lca_is_h ~first:(ia > ib) sub_a_rich in
       let b_touches = touches_cut lca_is_h ~first:(ib > ia) sub_b_rich in
       if not (a_touches && b_touches) then None
       else
         let perp_axis = if lca_is_h then V else H in
         let iva = tile_interval_perturbed ~axis:perp_axis a tiling in
         let ivb = tile_interval_perturbed ~axis:perp_axis b tiling in
         if not (intervals_overlap_f iva ivb) then None
         else
           let dir = if lca_is_h then
             (if ia < ib then South else North)
           else
             (if ia < ib then East else West)
           in
           Some (dir, lca_depth))
  | _ -> None

let lca_adjacent a b tiling =
  match lca_adjacent_with_depth a b tiling with
  | Some (dir, _) -> Some dir
  | None -> None

let lca_neighbors n tiling =
  List.filter_map (fun m ->
    if m = n then None
    else match lca_adjacent n m tiling with
      | Some dir -> Some (dir, m)
      | None -> None
  ) (leaves tiling)

(* --- Tabstop-based adjacency --- *)

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

let tabstop_adjacent a_id b_id bounds_list tiling =
  match List.assoc_opt a_id bounds_list, List.assoc_opt b_id bounds_list with
  | Some a, Some b ->
    (* Check shared tabstop, then verify perpendicular interval overlap *)
    let candidate =
      if a.right = b.left then Some (East, H)   (* shared x-tab, check Y overlap *)
      else if a.left = b.right then Some (West, H)
      else if a.bottom = b.top then Some (South, V) (* shared y-tab, check X overlap *)
      else if a.top = b.bottom then Some (North, V)
      else None
    in
    (match candidate with
     | None -> None
     | Some (dir, perp_axis) ->
       let iva = tile_interval_perturbed ~axis:perp_axis a_id tiling in
       let ivb = tile_interval_perturbed ~axis:perp_axis b_id tiling in
       if intervals_overlap_f iva ivb then Some dir else None)
  | _ -> None

let tabstop_neighbors n bounds_list tiling =
  List.filter_map (fun (m, _) ->
    if m = n then None
    else match tabstop_adjacent n m bounds_list tiling with
      | Some dir -> Some (dir, m)
      | None -> None
  ) bounds_list

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

(* --- Adjacency graph --- *)

(* All adjacency edges as sorted (a, b) pairs with a < b. *)
let adjacency_graph tiling =
  let tiles = List.sort compare (leaves tiling) in
  let edges = ref [] in
  List.iter (fun a ->
    List.iter (fun b ->
      if a < b then
        match lca_adjacent a b tiling with
        | Some _ -> edges := (a, b) :: !edges
        | None -> ()
    ) tiles
  ) tiles;
  List.sort compare !edges

(* Adjacency graph with cut depth for each edge *)
let adjacency_graph_with_depth tiling =
  let tiles = List.sort compare (leaves tiling) in
  let edges = ref [] in
  List.iter (fun a ->
    List.iter (fun b ->
      if a < b then
        match lca_adjacent_with_depth a b tiling with
        | Some (_, depth) -> edges := (a, b, depth) :: !edges
        | None -> ()
    ) tiles
  ) tiles;
  !edges

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
