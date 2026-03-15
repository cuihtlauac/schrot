type wall = [`Left | `Right | `Top | `Bottom]

(* Does tile touch the given wall?
   Left: no V-R step; Right: no V-L; Top: no H-R; Bottom: no H-L *)
let touches_wall wall path =
  let dominated_by = match wall with
    | `Left   -> fun (s : Command.step) -> s.split = `V && s.side = R
    | `Right  -> fun (s : Command.step) -> s.split = `V && s.side = L
    | `Top    -> fun (s : Command.step) -> s.split = `H && s.side = R
    | `Bottom -> fun (s : Command.step) -> s.split = `H && s.side = L
  in
  not (List.exists dominated_by path)

(* Count of perpendicular splits: H for left/right walls, V for top/bottom.
   extent = (1/2)^perp_depth when touching wall, 0 otherwise. *)
let perp_depth wall path =
  let perp_split = match wall with
    | `Left | `Right -> `H
    | `Top | `Bottom -> `V
  in
  List.length (List.filter (fun (s : Command.step) -> s.split = perp_split) path)

(* Did edge extent strictly increase? *)
let extent_increased wall path_before path_after =
  touches_wall wall path_after &&
  (not (touches_wall wall path_before) ||
   perp_depth wall path_after < perp_depth wall path_before)

(* Aspect signature: h_count - v_count. aspect = 2^sig *)
let aspect_sig path =
  List.fold_left (fun acc (s : Command.step) ->
    match s.split with `H -> acc + 1 | `V -> acc - 1
  ) 0 path

(* Sum of |sig_after - sig_before| for all leaves except skip.
   Replaces float aspect_cost; ordering is preserved (differs by ln 2). *)
let aspect_distortion ~skip before after =
  let paths_before =
    List.map (fun n -> (n, Command.find_path n before))
      (let rec leaves acc = function
         | Term.Leaf n -> n :: acc
         | Term.H (a, b) | Term.V (a, b) -> leaves (leaves acc a) b
       in List.rev (leaves [] before))
  in
  List.fold_left (fun acc (n, pb) ->
    if n = skip then acc
    else
      let pa = Command.find_path n after in
      acc + abs (aspect_sig pa - aspect_sig pb)
  ) 0 paths_before

(* Center as dyadic rational: (numerator, denominator) where denom = 2^(k+1).
   center_x reads V-step sides as binary; center_y reads H-step sides. *)
let center_coord axis path =
  let relevant = List.filter (fun (s : Command.step) ->
    s.split = (match axis with `X -> `V | `Y -> `H)
  ) path in
  let k = List.length relevant in
  let binary_val = List.fold_left (fun acc (s : Command.step) ->
    acc * 2 + (match s.side with L -> 0 | R -> 1)
  ) 0 relevant in
  (2 * binary_val + 1, 1 lsl (k + 1))

(* Did center move strictly in the given direction? *)
let center_moved dir path_before path_after =
  let axis = match dir with
    | Command.Left | Command.Right -> `X
    | Command.Up | Command.Down -> `Y
  in
  let (n1, d1) = center_coord axis path_before in
  let (n2, d2) = center_coord axis path_after in
  match dir with
  | Command.Left | Command.Up -> n2 * d1 < n1 * d2
  | Command.Right | Command.Down -> n2 * d1 > n1 * d2
