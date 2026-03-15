type rect = { x: float; y: float; w: float; h: float }

let interpret term =
  let acc = ref [] in
  let rec go x y w h = function
    | Term.Leaf n -> acc := (n, { x; y; w; h }) :: !acc
    | Term.H (a, b) ->
      let h2 = h /. 2. in
      go x y w h2 a;
      go x (y +. h2) w h2 b
    | Term.V (a, b) ->
      let w2 = w /. 2. in
      go x y w2 h a;
      go (x +. w2) y w2 h b
  in
  go 0. 0. 1. 1. term;
  List.rev !acc

let center_x r = r.x +. r.w /. 2.
let center_y r = r.y +. r.h /. 2.

let find_rect n rects =
  List.assoc n rects

let eps = 1e-9

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
