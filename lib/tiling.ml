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
