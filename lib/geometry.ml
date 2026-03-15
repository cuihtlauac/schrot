type rect = { x: float; y: float; w: float; h: float }

let interpret term =
  let acc = ref [] in
  let rec go x y w h = function
    | Term.Leaf n -> acc := (n, { x; y; w; h }) :: !acc
    | Term.H (a, b, r) ->
      let rf = Q.to_float r in
      let ha = h *. rf in
      go x y w ha a;
      go x (y +. ha) w (h -. ha) b
    | Term.V (a, b, r) ->
      let rf = Q.to_float r in
      let wa = w *. rf in
      go x y wa h a;
      go (x +. wa) y (w -. wa) h b
  in
  go 0. 0. 1. 1. term;
  List.rev !acc

let center_x r = r.x +. r.w /. 2.
let center_y r = r.y +. r.h /. 2.

let find_rect n rects =
  List.assoc n rects

let eps = 1e-9

let aspect r = r.w /. r.h

let aspect_cost ~skip before after =
  let rb = interpret before in
  let ra = interpret after in
  List.fold_left (fun acc (n, r0) ->
    if n = skip then acc
    else match List.assoc_opt n ra with
      | None -> acc
      | Some r1 ->
        let ratio = aspect r1 /. aspect r0 in
        acc +. abs_float (log ratio)
  ) 0. rb

let edge_extent side n term =
  let rects = interpret term in
  match List.assoc_opt n rects with
  | None -> 0.
  | Some r ->
    match side with
    | `Left   -> if r.x < eps then r.h else 0.
    | `Right  -> if r.x +. r.w > 1. -. eps then r.h else 0.
    | `Top    -> if r.y < eps then r.w else 0.
    | `Bottom -> if r.y +. r.h > 1. -. eps then r.w else 0.
