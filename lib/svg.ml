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
