let colors = [|
  "#e6194b"; "#3cb44b"; "#ffe119"; "#4363d8"; "#f58231";
  "#911eb4"; "#42d4f4"; "#f032e6"; "#bfef45"; "#fabed4";
  "#469990"; "#dcbeff"; "#9a6324"; "#800000"; "#aaffc3";
  "#808000"; "#ffd8b1"; "#000075"; "#a9a9a9"; "#fffac8";
|]

let default_color_of n = colors.(n mod Array.length colors)

let default_text_color_of _n = "black"

let render_group ~x ~y ~width ~height ~margin
    ?(color_of = default_color_of) ?(text_color_of = default_text_color_of) term =
  let buf = Buffer.create 4096 in
  let add = Buffer.add_string buf in
  let addf fmt = Printf.ksprintf add fmt in
  let rec go x y w h = function
    | Term.Leaf n ->
      addf "<rect x=\"%g\" y=\"%g\" width=\"%g\" height=\"%g\" \
            fill=\"%s\" stroke=\"black\" stroke-width=\"1\"/>\n"
        x y w h (color_of n);
      addf "<text x=\"%g\" y=\"%g\" text-anchor=\"middle\" \
            dominant-baseline=\"central\" font-size=\"16\" \
            fill=\"%s\">%d</text>\n"
        (x +. w /. 2.) (y +. h /. 2.) (text_color_of n) n
    | Term.H (a, b) ->
      let h2 = h /. 2. in
      go x y w h2 a;
      go x (y +. h2) w h2 b
    | Term.V (a, b) ->
      let w2 = w /. 2. in
      go x y w2 h a;
      go (x +. w2) y w2 h b
  in
  go (x +. margin) (y +. margin)
    (width -. 2. *. margin) (height -. 2. *. margin) term;
  Buffer.contents buf

let render_interactive ~x ~y ~width ~height ~margin
    ?(color_of = default_color_of) ?(text_color_of = default_text_color_of) term =
  let buf = Buffer.create 4096 in
  let add = Buffer.add_string buf in
  let addf fmt = Printf.ksprintf add fmt in
  let rec go x y w h = function
    | Term.Leaf n ->
      addf "<g data-tile=\"%d\" style=\"cursor:pointer\">\n" n;
      addf "<rect x=\"%g\" y=\"%g\" width=\"%g\" height=\"%g\" \
            fill=\"%s\" stroke=\"black\" stroke-width=\"1\"/>\n"
        x y w h (color_of n);
      addf "<text x=\"%g\" y=\"%g\" text-anchor=\"middle\" \
            dominant-baseline=\"central\" font-size=\"16\" \
            fill=\"%s\" pointer-events=\"none\">%d</text>\n"
        (x +. w /. 2.) (y +. h /. 2.) (text_color_of n) n;
      add "</g>\n"
    | Term.H (a, b) ->
      let h2 = h /. 2. in
      go x y w h2 a;
      go x (y +. h2) w h2 b
    | Term.V (a, b) ->
      let w2 = w /. 2. in
      go x y w2 h a;
      go (x +. w2) y w2 h b
  in
  go (x +. margin) (y +. margin)
    (width -. 2. *. margin) (height -. 2. *. margin) term;
  Buffer.contents buf

let render ~width ~height term =
  let buf = Buffer.create 4096 in
  let add = Buffer.add_string buf in
  let addf fmt = Printf.ksprintf add fmt in
  addf "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%g\" height=\"%g\">\n"
    width height;
  addf "<rect width=\"%g\" height=\"%g\" fill=\"white\"/>\n" width height;
  add (render_group ~x:0. ~y:0. ~width ~height ~margin:100. term);
  add "</svg>\n";
  Buffer.contents buf

(* Spectral colors: ROYGBIV *)
let spectrum = [|
  "#FF0000"; (* 0: Red *)
  "#FF7F00"; (* 1: Orange *)
  "#FFFF00"; (* 2: Yellow *)
  "#00CC00"; (* 3: Green *)
  "#0000FF"; (* 4: Blue *)
  "#4B0082"; (* 5: Indigo *)
  "#8B00FF"; (* 6: Violet *)
|]

(* Map frame depth to spectral color.
   depth 0 = red, deepest = violet, intermediate evenly spaced. *)
let cut_color ~depth ~max_depth =
  if max_depth <= 1 then spectrum.(0)
  else
    let idx = depth * 6 / (max_depth - 1) in
    spectrum.(min idx 6)

(* Render a Schroeder tiling with spectral cut lines *)
let render_tiling_group ~x ~y ~width ~height ~margin
    ?(selected : int option) ?(interactive = false) (tiling : Tiling.t) =
  let buf = Buffer.create 4096 in
  let add = Buffer.add_string buf in
  let addf fmt = Printf.ksprintf add fmt in
  let tree = Tiling.tree tiling in
  let max_depth = Schrot.height tree in
  let x0 = x +. margin and y0 = y +. margin in
  let w0 = width -. 2. *. margin and h0 = height -. 2. *. margin in
  (* Pass 1: tile backgrounds *)
  let rec tiles x y w h is_h = function
    | Schrot.Tile n ->
      let fill = if selected = Some n then "#1a3a6b" else "white" in
      let text_fill = if selected = Some n then "white" else "black" in
      if interactive then
        addf "<g data-tile=\"%d\" style=\"cursor:pointer\">\n" n;
      addf "<rect x=\"%g\" y=\"%g\" width=\"%g\" height=\"%g\" fill=\"%s\"/>\n"
        x y w h fill;
      addf "<text x=\"%g\" y=\"%g\" text-anchor=\"middle\" \
            dominant-baseline=\"central\" font-size=\"16\" \
            fill=\"%s\"%s>%d</text>\n"
        (x +. w /. 2.) (y +. h /. 2.) text_fill
        (if interactive then " pointer-events=\"none\"" else "") n;
      if interactive then add "</g>\n"
    | Schrot.Frame children ->
      let k = float_of_int (List2.length children) in
      if is_h then begin
        let slice = h /. k in
        List2.iteri (fun i child ->
          tiles x (y +. float_of_int i *. slice) w slice (not is_h) child
        ) children
      end else begin
        let slice = w /. k in
        List2.iteri (fun i child ->
          tiles (x +. float_of_int i *. slice) y slice h (not is_h) child
        ) children
      end
  in
  (* Pass 2: collect cut lines by depth, draw deepest first so shallow
     cuts paint over deep ones at intersections *)
  let lines_by_depth = Array.make (max_depth + 1) [] in
  let rec collect x y w h is_h depth = function
    | Schrot.Tile _ -> ()
    | Schrot.Frame children ->
      let k = List2.length children in
      let kf = float_of_int k in
      if is_h then begin
        let slice = h /. kf in
        for i = 1 to k - 1 do
          let cy = y +. float_of_int i *. slice in
          lines_by_depth.(depth) <-
            (x, cy, x +. w, cy) :: lines_by_depth.(depth)
        done;
        List2.iteri (fun i child ->
          collect x (y +. float_of_int i *. slice) w slice (not is_h) (depth + 1) child
        ) children
      end else begin
        let slice = w /. kf in
        for i = 1 to k - 1 do
          let cx = x +. float_of_int i *. slice in
          lines_by_depth.(depth) <-
            (cx, y, cx, y +. h) :: lines_by_depth.(depth)
        done;
        List2.iteri (fun i child ->
          collect (x +. float_of_int i *. slice) y slice h (not is_h) (depth + 1) child
        ) children
      end
  in
  let is_h = Tiling.is_h tiling in
  tiles x0 y0 w0 h0 is_h tree;
  collect x0 y0 w0 h0 is_h 0 tree;
  (* Draw from deepest to shallowest *)
  for depth = max_depth - 1 downto 0 do
    let color = cut_color ~depth ~max_depth in
    List.iter (fun (x1, y1, x2, y2) ->
      addf "<line x1=\"%g\" y1=\"%g\" x2=\"%g\" y2=\"%g\" \
            stroke=\"%s\" stroke-width=\"2\"/>\n"
        x1 y1 x2 y2 color
    ) lines_by_depth.(depth)
  done;
  (* Outer border: black *)
  addf "<rect x=\"%g\" y=\"%g\" width=\"%g\" height=\"%g\" \
        fill=\"none\" stroke=\"black\" stroke-width=\"2\"/>\n"
    x0 y0 w0 h0;
  Buffer.contents buf

let render_tiling ~width ~height tiling =
  let buf = Buffer.create 4096 in
  let add = Buffer.add_string buf in
  let addf fmt = Printf.ksprintf add fmt in
  addf "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%g\" height=\"%g\">\n"
    width height;
  addf "<rect width=\"%g\" height=\"%g\" fill=\"white\"/>\n" width height;
  add (render_tiling_group ~x:0. ~y:0. ~width ~height ~margin:10. tiling);
  add "</svg>\n";
  Buffer.contents buf
