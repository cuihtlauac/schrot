type wall = [`Left | `Right | `Top | `Bottom]

(* Does tile touch the given wall? Structural — independent of ratios. *)
let touches_wall wall path =
  let dominated_by = match wall with
    | `Left   -> fun (s : Command.step) -> s.split = `V && s.side = R
    | `Right  -> fun (s : Command.step) -> s.split = `V && s.side = L
    | `Top    -> fun (s : Command.step) -> s.split = `H && s.side = R
    | `Bottom -> fun (s : Command.step) -> s.split = `H && s.side = L
  in
  not (List.exists dominated_by path)

(* Edge extent as a rational: product of perpendicular fractions if touching,
   zero otherwise.
   Left/Right: extent = product of H-step fractions (height factors)
   Top/Bottom: extent = product of V-step fractions (width factors) *)
let extent wall path =
  if not (touches_wall wall path) then Q.zero
  else
    let perp_split = match wall with
      | `Left | `Right -> `H
      | `Top | `Bottom -> `V
    in
    List.fold_left (fun acc (s : Command.step) ->
      if s.split = perp_split then
        let frac = match s.side with
          | L -> s.ratio
          | R -> Q.sub Q.one s.ratio
        in
        Q.mul acc frac
      else acc
    ) Q.one path

(* Did edge extent strictly increase? *)
let extent_increased wall path_before path_after =
  let eb = extent wall path_before in
  let ea = extent wall path_after in
  Q.compare ea eb > 0

(* Tile dimensions as rationals *)
let tile_width path =
  List.fold_left (fun acc (s : Command.step) ->
    if s.split = `V then
      Q.mul acc (match s.side with L -> s.ratio | R -> Q.sub Q.one s.ratio)
    else acc
  ) Q.one path

let tile_height path =
  List.fold_left (fun acc (s : Command.step) ->
    if s.split = `H then
      Q.mul acc (match s.side with L -> s.ratio | R -> Q.sub Q.one s.ratio)
    else acc
  ) Q.one path

(* Aspect distortion: sum of |log(aspect_after/aspect_before)| for non-skip leaves.
   Uses float for the log comparison — exact rational comparison of aspects
   would require comparing products of rationals, which is doable but the
   distortion metric itself (log) is transcendental. *)
let aspect_distortion ~skip before after =
  let leaves =
    let rec go acc = function
      | Term.Leaf n -> n :: acc
      | Term.H (a, b, _) | Term.V (a, b, _) -> go (go acc a) b
    in List.rev (go [] before)
  in
  List.fold_left (fun acc n ->
    if n = skip then acc
    else
      let pb = Command.find_path n before in
      let pa = Command.find_path n after in
      let wb = Q.to_float (tile_width pb) in
      let hb = Q.to_float (tile_height pb) in
      let wa = Q.to_float (tile_width pa) in
      let ha = Q.to_float (tile_height pa) in
      let aspect_before = wb /. hb in
      let aspect_after = wa /. ha in
      acc +. abs_float (log (aspect_after /. aspect_before))
  ) 0. leaves

(* Center as rational *)
let center_x path =
  let x = List.fold_left (fun (pos, size) (s : Command.step) ->
    if s.split = `V then
      match s.side with
      | L -> (pos, Q.mul size s.ratio)
      | R -> (Q.add pos (Q.mul size s.ratio), Q.mul size (Q.sub Q.one s.ratio))
    else (pos, size)
  ) (Q.zero, Q.one) path in
  let pos, size = x in
  Q.add pos (Q.div size (Q.make 2 1))

let center_y path =
  let y = List.fold_left (fun (pos, size) (s : Command.step) ->
    if s.split = `H then
      match s.side with
      | L -> (pos, Q.mul size s.ratio)
      | R -> (Q.add pos (Q.mul size s.ratio), Q.mul size (Q.sub Q.one s.ratio))
    else (pos, size)
  ) (Q.zero, Q.one) path in
  let pos, size = y in
  Q.add pos (Q.div size (Q.make 2 1))

(* Did center move strictly in the given direction? *)
let center_moved dir path_before path_after =
  match dir with
  | Command.Left  -> Q.compare (center_x path_after) (center_x path_before) < 0
  | Command.Right -> Q.compare (center_x path_after) (center_x path_before) > 0
  | Command.Up    -> Q.compare (center_y path_after) (center_y path_before) < 0
  | Command.Down  -> Q.compare (center_y path_after) (center_y path_before) > 0
