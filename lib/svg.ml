let colors = [|
  "#e6194b"; "#3cb44b"; "#ffe119"; "#4363d8"; "#f58231";
  "#911eb4"; "#42d4f4"; "#f032e6"; "#bfef45"; "#fabed4";
  "#469990"; "#dcbeff"; "#9a6324"; "#800000"; "#aaffc3";
  "#808000"; "#ffd8b1"; "#000075"; "#a9a9a9"; "#fffac8";
|]

let default_color_of n = colors.(n mod Array.length colors)

let default_text_color_of _n = "black"

(* TODO: Bring up to Schroder tilings *)
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

(* TODO: Bring up to Schroder tilings *)
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

(* TODO: Bring up to Schroder tilings *)
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
let cut_index ~depth ~max_depth =
  if max_depth <= 1 then 0
  else min (depth * 6 / (max_depth - 1)) 6

let cut_color ~depth ~max_depth =
  spectrum.(cut_index ~depth ~max_depth)

(* Render a Schroeder tiling with spectral cut lines *)
let dot_r = 4.
let dot_gap = 10.
let dot_row_h = 2. *. dot_r +. 4. (* dots + half text height gap *)

(* Render a Schroeder tiling with spectral cut lines.
   Uses iterative repulsion to eliminate 3-multiplicity junctions:
   starts from equal splits + random salt, then pushes coincident cuts
   apart until reaching a fixpoint. *)
let render_tiling_group ~x ~y ~width ~height ~margin
    ?(selected : int option) ?(interactive = false)
    ?(show_labels = true) ?(show_dots = true)
    ?color_of ?weighted (tiling : Tiling.t) =
  let buf = Buffer.create 4096 in
  let add = Buffer.add_string buf in
  let addf fmt = Printf.ksprintf add fmt in
  let tree = Tiling.tree tiling in
  let max_depth = Schrot.height tree in
  let has_dots = max_depth > 0 in
  let x0 = x +. margin in
  let y0 = y +. margin +. (if has_dots then dot_row_h else 0.) in
  let w0 = width -. 2. *. margin in
  let h0 = height -. 2. *. margin -. (if has_dots then dot_row_h else 0.) in
  let wt = match weighted with Some w -> w | None -> Tiling.resolve_splits tiling in
  (* Pass 1: tile backgrounds from weighted tree *)
  let _ = add in (* used below *)
  let rects = Tiling.rects_of_weighted wt in
  List.iter (fun (n, (r : Tiling.rect)) ->
    let ox = x0 +. r.rx *. w0 in
    let oy = y0 +. r.ry *. h0 in
    let ow = r.rw *. w0 in
    let oh = r.rh *. h0 in
    let fill = if selected = Some n then "#1a3a6b"
      else match color_of with Some f -> f n | None -> "white" in
    let text_fill = if selected = Some n then "white" else "black" in
    if interactive then
      addf "<g data-tile=\"%d\" style=\"cursor:pointer\">\n" n;
    addf "<rect x=\"%g\" y=\"%g\" width=\"%g\" height=\"%g\" fill=\"%s\"/>\n"
      ox oy ow oh fill;
    if show_labels then
      addf "<text x=\"%g\" y=\"%g\" text-anchor=\"middle\" \
            dominant-baseline=\"central\" font-size=\"16\" \
            fill=\"%s\"%s>%d</text>\n"
        (ox +. ow /. 2.) (oy +. oh /. 2.) text_fill
        (if interactive then " pointer-events=\"none\"" else "") n;
    if interactive then add "</g>\n"
  ) rects;
  (* Pass 2: cut lines by depth from the weighted tree *)
  let lines_by_depth = Tiling.cuts_of_weighted wt in
  (* Draw from deepest to shallowest, scaling [0,1]^2 to viewport *)
  for depth = max_depth - 1 downto 0 do
    let color = cut_color ~depth ~max_depth in
    List.iter (fun (lx1, ly1, lx2, ly2) ->
      addf "<line x1=\"%g\" y1=\"%g\" x2=\"%g\" y2=\"%g\" \
            stroke=\"%s\" stroke-width=\"2\"/>\n"
        (x0 +. lx1 *. w0) (y0 +. ly1 *. h0)
        (x0 +. lx2 *. w0) (y0 +. ly2 *. h0) color
    ) lines_by_depth.(depth)
  done;
  (* Outer border *)
  addf "<rect x=\"%g\" y=\"%g\" width=\"%g\" height=\"%g\" \
        fill=\"none\" stroke=\"#999\" stroke-width=\"1\" stroke-dasharray=\"4,4\"/>\n"
    x0 y0 w0 h0;
  (* Color dots *)
  if show_dots && max_depth > 0 then begin
    let total_w = float_of_int max_depth *. (2. *. dot_r) +.
                  float_of_int (max_depth - 1) *. (dot_gap -. 2. *. dot_r) in
    let start_x = x0 +. (w0 -. total_w) /. 2. +. dot_r in
    let cy = y +. margin +. dot_r in
    for depth = 0 to max_depth - 1 do
      let color = cut_color ~depth ~max_depth in
      let cx = start_x +. float_of_int depth *. dot_gap in
      addf "<circle cx=\"%g\" cy=\"%g\" r=\"%g\" fill=\"%s\"/>\n"
        cx cy dot_r color
    done
  end;
  Buffer.contents buf

(* Render clickable segment overlays on top of a tiling.
   Each cut segment gets a transparent hit-target rect and, when selected,
   a highlighted line.  Viewport mapping matches render_tiling_group. *)
let render_segment_overlays ~x ~y ~width ~height ~margin
    ?(selected_segment : int option)
    (segments : Tiling.segment list) (tiling : Tiling.t) =
  let buf = Buffer.create 1024 in
  let addf fmt = Printf.ksprintf (Buffer.add_string buf) fmt in
  let tree = Tiling.tree tiling in
  let max_depth = Schrot.height tree in
  let has_dots = max_depth > 0 in
  let x0 = x +. margin in
  let y0 = y +. margin +. (if has_dots then dot_row_h else 0.) in
  let w0 = width -. 2. *. margin in
  let h0 = height -. 2. *. margin -. (if has_dots then dot_row_h else 0.) in
  let hit_w = 10. in
  List.iter (fun (seg : Tiling.segment) ->
    let sx1 = x0 +. seg.seg_x1 *. w0 in
    let sy1 = y0 +. seg.seg_y1 *. h0 in
    let sx2 = x0 +. seg.seg_x2 *. w0 in
    let sy2 = y0 +. seg.seg_y2 *. h0 in
    let is_sel = selected_segment = Some seg.seg_id in
    addf "<g data-segment=\"%d\" style=\"cursor:%s\">\n"
      seg.seg_id (if seg.seg_is_h then "ns-resize" else "ew-resize");
    if seg.seg_is_h then
      addf "<rect x=\"%g\" y=\"%g\" width=\"%g\" height=\"%g\" \
            fill=\"%s\" opacity=\"%g\" pointer-events=\"all\"/>\n"
        sx1 (sy1 -. hit_w /. 2.) (sx2 -. sx1) hit_w
        (if is_sel then "#f0c040" else "transparent")
        (if is_sel then 0.3 else 0.0)
    else
      addf "<rect x=\"%g\" y=\"%g\" width=\"%g\" height=\"%g\" \
            fill=\"%s\" opacity=\"%g\" pointer-events=\"all\"/>\n"
        (sx1 -. hit_w /. 2.) sy1 hit_w (sy2 -. sy1)
        (if is_sel then "#f0c040" else "transparent")
        (if is_sel then 0.3 else 0.0);
    if is_sel then
      addf "<line x1=\"%g\" y1=\"%g\" x2=\"%g\" y2=\"%g\" \
            stroke=\"#f0c040\" stroke-width=\"4\" opacity=\"0.8\" \
            pointer-events=\"none\"/>\n"
        sx1 sy1 sx2 sy2;
    addf "</g>\n"
  ) segments;
  Buffer.contents buf

(* Node-link tree diagram: root at top, leaves at bottom.
   Leaves get sequential x-positions; internal nodes centered over children.
   Internal nodes colored by depth using the same spectral palette as cut lines. *)
let render_tree_diagram ~x ~y ~width ~height (tree : ('a, 'f) Schrot.t) =
  let buf = Buffer.create 1024 in
  let addf fmt = Printf.ksprintf (Buffer.add_string buf) fmt in
  let n_leaves = Schrot.size tree in
  let max_depth = Schrot.height tree in
  if n_leaves = 0 then Buffer.contents buf
  else
    let margin_x = 10. in
    let margin_y = 10. in
    let usable_w = width -. 2. *. margin_x in
    let usable_h = height -. 2. *. margin_y in
    let x_step = if n_leaves > 1 then usable_w /. float_of_int (n_leaves - 1) else 0. in
    let y_step = if max_depth > 0 then usable_h /. float_of_int max_depth else 0. in
    let leaf_counter = ref 0 in
    let node_r = 3.5 in
    let rec go d = function
      | Schrot.Tile _ ->
        let lx = float_of_int !leaf_counter *. x_step in
        incr leaf_counter;
        let cx = x +. margin_x +. lx in
        let cy = y +. margin_y +. float_of_int d *. y_step in
        addf "<circle cx=\"%g\" cy=\"%g\" r=\"%g\" fill=\"white\" \
              stroke=\"black\" stroke-width=\"1\"/>\n" cx cy node_r;
        (cx, cy)
      | Schrot.Frame children ->
        let child_positions = List.map (fun (_, c) -> go (d + 1) c) (List2.to_list children) in
        let cx_sum = List.fold_left (fun acc (cx, _) -> acc +. cx) 0. child_positions in
        let cx = cx_sum /. float_of_int (List.length child_positions) in
        let cy = y +. margin_y +. float_of_int d *. y_step in
        List.iter (fun (child_cx, child_cy) ->
          addf "<line x1=\"%g\" y1=\"%g\" x2=\"%g\" y2=\"%g\" \
                stroke=\"black\" stroke-width=\"1\"/>\n"
            cx cy child_cx child_cy
        ) child_positions;
        let color = cut_color ~depth:d ~max_depth in
        addf "<circle cx=\"%g\" cy=\"%g\" r=\"%g\" fill=\"%s\"/>\n"
          cx cy node_r color;
        (cx, cy)
    in
    ignore (go 0 tree);
    Buffer.contents buf

(* Render adjacency graph from a Geom.t. Edges colored by cut depth. *)
let render_adjacency_graph ~x ~y ~width ~height (g : Geom.t) =
  let buf = Buffer.create 1024 in
  let addf fmt = Printf.ksprintf (Buffer.add_string buf) fmt in
  let mg = 15. in
  let max_depth = Schrot.height (Tiling.tree g.tiling) in
  let usable_w = width -. 2. *. mg in
  let usable_h = height -. 2. *. mg in
  let pos n =
    let (cx, cy) = Geom.center_of g n in
    (x +. mg +. cx *. usable_w, y +. mg +. cy *. usable_h)
  in
  List.iter (fun (a, b) ->
    let (ax, ay) = pos a and (bx, by) = pos b in
    let depth = Tiling.cut_depth a b g.tiling in
    let color = cut_color ~depth ~max_depth in
    addf "<line x1=\"%g\" y1=\"%g\" x2=\"%g\" y2=\"%g\" \
          stroke=\"%s\" stroke-width=\"2.5\"/>\n"
      ax ay bx by color
  ) (Geom.edges g);
  let node_r = 4. in
  List.iter (fun n ->
    let (cx, cy) = pos n in
    addf "<circle cx=\"%g\" cy=\"%g\" r=\"%g\" fill=\"black\"/>\n"
      cx cy node_r
  ) (List.sort compare (Tiling.leaves g.tiling));
  Buffer.contents buf

(* Global SVG <defs> for arrow markers — one per spectrum color.
   Call once per SVG document, before any render_hasse_diagram. *)
let arrow_defs =
  let arrow_sz = 2.5 in
  let buf = Buffer.create 512 in
  let addf fmt = Printf.ksprintf (Buffer.add_string buf) fmt in
  Buffer.add_string buf "<defs>\n";
  Array.iteri (fun i color ->
    addf "<marker id=\"arrow_%d\" markerWidth=\"%g\" markerHeight=\"%g\" \
          refX=\"%g\" refY=\"%g\" orient=\"auto\">\
          <path d=\"M0,0 L%g,%g L0,%g\" fill=\"%s\"/></marker>\n"
      i (arrow_sz *. 2.) (arrow_sz *. 2.)
      (arrow_sz *. 2.) arrow_sz
      (arrow_sz *. 2.) arrow_sz (arrow_sz *. 2.) color
  ) spectrum;
  Buffer.add_string buf "</defs>\n";
  Buffer.contents buf

(* Orient an adjacency edge (a,b) into (lo, hi) where lo < hi in the
   poset.  Uses geometry directly: left-of or below = smaller. *)
let orient_edge rects (a, b) =
  let ar = List.assoc a rects and br = List.assoc b rects in
  let eps = 1e-9 in
  if abs_float ((ar.Geom.x +. ar.Geom.w) -. br.Geom.x) < eps then (a, b)
  else if abs_float ((br.Geom.x +. br.Geom.w) -. ar.Geom.x) < eps then (b, a)
  else if abs_float ((ar.Geom.y +. ar.Geom.h) -. br.Geom.y) < eps then (b, a)
  else (a, b)

(* Adjacency graph with directed edges (lo → hi in the poset).
   Planar embedding: tile centers guarantee no crossings. *)
let render_hasse_diagram ~x ~y ~width ~height (g : Geom.t) =
  let buf = Buffer.create 1024 in
  let add = Buffer.add_string buf in
  let addf fmt = Printf.ksprintf add fmt in
  let mg = 15. in
  let node_r = 8. in
  let usable_w = width -. 2. *. mg in
  let usable_h = height -. 2. *. mg in
  let n = Tiling.size g.tiling in
  if n <= 0 then Buffer.contents buf
  else begin
    let max_depth = Schrot.height (Tiling.tree g.tiling) in
    let pos tile_id =
      let (cx, cy) = Geom.center_of g tile_id in
      (x +. mg +. cx *. usable_w, y +. mg +. cy *. usable_h)
    in
    List.iter (fun (a, b) ->
      let (lo, hi) = orient_edge g.rects (a, b) in
      let (ax, ay) = pos lo and (bx, by) = pos hi in
      let depth = Tiling.cut_depth a b g.tiling in
      let color = cut_color ~depth ~max_depth in
      let cidx = cut_index ~depth ~max_depth in
      (* Shorten line so arrow doesn't overlap nodes *)
      let dx = bx -. ax and dy = by -. ay in
      let len = sqrt (dx *. dx +. dy *. dy) in
      if len > 0. then begin
        let ux = dx /. len and uy = dy /. len in
        let x1 = ax +. ux *. node_r and y1 = ay +. uy *. node_r in
        let x2 = bx -. ux *. node_r and y2 = by -. uy *. node_r in
        addf "<line x1=\"%g\" y1=\"%g\" x2=\"%g\" y2=\"%g\" \
              stroke=\"%s\" stroke-width=\"2\" \
              marker-end=\"url(#arrow_%d)\"/>\n"
          x1 y1 x2 y2 color cidx
      end
    ) (Geom.edges g);
    for tile_id = 0 to n - 1 do
      let (cx, cy) = pos tile_id in
      addf "<circle cx=\"%g\" cy=\"%g\" r=\"%g\" fill=\"white\" \
            stroke=\"black\" stroke-width=\"1.5\"/>\n" cx cy node_r;
      addf "<text x=\"%g\" y=\"%g\" font-size=\"11\" text-anchor=\"middle\" \
            dominant-baseline=\"central\" fill=\"black\">%d</text>\n"
        cx cy tile_id
    done;
    Buffer.contents buf
  end

(* Proportional distribution: each child gets span * s / total,
   remainders distributed left to right. *)
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
