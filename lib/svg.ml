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
let dot_r = 4.
let dot_gap = 10.
let dot_row_h = 2. *. dot_r +. 4. (* dots + half text height gap *)

let split_positions = Tiling.split_positions

let render_tiling_group ~x ~y ~width ~height ~margin
    ?(selected : int option) ?(interactive = false)
    ?(show_labels = true) ?(show_dots = true)
    ?color_of (tiling : Tiling.t) =
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
  (* Pass 1: tile backgrounds. [parity] alternates per child index
     to ensure sibling frames use different split ratios. *)
  let rec tiles x y w h is_h parity = function
    | Schrot.Tile n ->
      let fill = if selected = Some n then "#1a3a6b"
        else match color_of with Some f -> f n | None -> "white" in
      let text_fill = if selected = Some n then "white" else "black" in
      if interactive then
        addf "<g data-tile=\"%d\" style=\"cursor:pointer\">\n" n;
      addf "<rect x=\"%g\" y=\"%g\" width=\"%g\" height=\"%g\" fill=\"%s\"/>\n"
        x y w h fill;
      if show_labels then
        addf "<text x=\"%g\" y=\"%g\" text-anchor=\"middle\" \
              dominant-baseline=\"central\" font-size=\"16\" \
              fill=\"%s\"%s>%d</text>\n"
          (x +. w /. 2.) (y +. h /. 2.) text_fill
          (if interactive then " pointer-events=\"none\"" else "") n;
      if interactive then add "</g>\n"
    | Schrot.Frame children ->
      let k = List2.length children in
      let pos = split_positions ~parity k in
      if is_h then
        List2.iteri (fun i child ->
          let cy = y +. pos.(i) *. h in
          let ch = (pos.(i + 1) -. pos.(i)) *. h in
          tiles x cy w ch (not is_h) (parity <> (i mod 2 = 0)) child
        ) children
      else
        List2.iteri (fun i child ->
          let cx = x +. pos.(i) *. w in
          let cw = (pos.(i + 1) -. pos.(i)) *. w in
          tiles cx y cw h (not is_h) (parity <> (i mod 2 = 0)) child
        ) children
  in
  (* Pass 2: collect cut lines by depth, draw deepest first so shallow
     cuts paint over deep ones at intersections *)
  let lines_by_depth = Array.make (max_depth + 1) [] in
  let rec collect x y w h is_h parity depth = function
    | Schrot.Tile _ -> ()
    | Schrot.Frame children ->
      let k = List2.length children in
      let pos = split_positions ~parity k in
      if is_h then begin
        for i = 1 to k - 1 do
          let cy = y +. pos.(i) *. h in
          lines_by_depth.(depth) <-
            (x, cy, x +. w, cy) :: lines_by_depth.(depth)
        done;
        List2.iteri (fun i child ->
          let cy = y +. pos.(i) *. h in
          let ch = (pos.(i + 1) -. pos.(i)) *. h in
          collect x cy w ch (not is_h) (parity <> (i mod 2 = 0)) (depth + 1) child
        ) children
      end else begin
        for i = 1 to k - 1 do
          let cx = x +. pos.(i) *. w in
          lines_by_depth.(depth) <-
            (cx, y, cx, y +. h) :: lines_by_depth.(depth)
        done;
        List2.iteri (fun i child ->
          let cx = x +. pos.(i) *. w in
          let cw = (pos.(i + 1) -. pos.(i)) *. w in
          collect cx y cw h (not is_h) (parity <> (i mod 2 = 0)) (depth + 1) child
        ) children
      end
  in
  let is_h = Tiling.is_h tiling in
  tiles x0 y0 w0 h0 is_h true tree;
  collect x0 y0 w0 h0 is_h true 0 tree;
  (* Draw from deepest to shallowest *)
  for depth = max_depth - 1 downto 0 do
    let color = cut_color ~depth ~max_depth in
    List.iter (fun (x1, y1, x2, y2) ->
      addf "<line x1=\"%g\" y1=\"%g\" x2=\"%g\" y2=\"%g\" \
            stroke=\"%s\" stroke-width=\"2\"/>\n"
        x1 y1 x2 y2 color
    ) lines_by_depth.(depth)
  done;
  (* Outer border: grey dashed *)
  addf "<rect x=\"%g\" y=\"%g\" width=\"%g\" height=\"%g\" \
        fill=\"none\" stroke=\"#999\" stroke-width=\"1\" stroke-dasharray=\"4,4\"/>\n"
    x0 y0 w0 h0;
  (* Color dots: one per cut level, centered above the tiling *)
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

(* Node-link tree diagram: root at top, leaves at bottom.
   Leaves get sequential x-positions; internal nodes centered over children.
   Internal nodes colored by depth using the same spectral palette as cut lines. *)
let render_tree_diagram ~x ~y ~width ~height (tree : 'a Schrot.t) =
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
        let child_positions = List.map (go (d + 1)) (List2.to_list children) in
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
  List.iter (fun (a, b, depth) ->
    let (ax, ay) = pos a and (bx, by) = pos b in
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

(* Proportional distribution: each child gets span * s / total,
   remainders distributed left to right. *)
let distribute_proportional span leaf_counts =
  let k = List.length leaf_counts in
  if k = 0 then []
  else if k = 1 then [span]
  else
    let total = List.fold_left ( + ) 0 leaf_counts in
    let result = Array.make k 0 in
    let remaining = ref span in
    let rem_leaves = ref total in
    for i = 0 to k - 1 do
      let s = List.nth leaf_counts i in
      let alloc = if i = k - 1 then !remaining
        else max 1 (!remaining * s / !rem_leaves) in
      result.(i) <- alloc;
      remaining := !remaining - alloc;
      rem_leaves := !rem_leaves - s
    done;
    Array.to_list result

let cumsum sizes =
  let a = Array.of_list sizes in
  let arr = Array.make (Array.length a + 1) 0 in
  Array.iteri (fun i s -> arr.(i + 1) <- arr.(i) + s) a;
  arr

(* Proportional + 1-cell bias: shift first child +1 / last -1 (or reverse).
   This ensures sibling frames place cuts at different grid positions. *)
let distribute_biased ~bias span leaf_counts =
  let base = distribute_proportional span leaf_counts in
  let k = List.length base in
  if k < 2 then base
  else
    let arr = Array.of_list base in
    if not bias then begin
      if arr.(k - 1) >= 2 then begin
        arr.(0) <- arr.(0) + 1;
        arr.(k - 1) <- arr.(k - 1) - 1
      end
    end else begin
      if arr.(0) >= 2 then begin
        arr.(0) <- arr.(0) - 1;
        arr.(k - 1) <- arr.(k - 1) + 1
      end
    end;
    Array.to_list arr

(* Grid-based tiling rendering (Wikipedia style).
   n×n grid, proportional layout. Sibling frames get opposite bias
   (±1 grid cell) to prevent "+" crosses. *)
let render_tiling_grid ~x ~y ~width ~height (tiling : Tiling.t) =
  let buf = Buffer.create 1024 in
  let addf fmt = Printf.ksprintf (Buffer.add_string buf) fmt in
  let tree = Tiling.tree tiling in
  let is_h = Tiling.is_h tiling in
  let n = Schrot.size tree in
  let max_depth = Schrot.height tree in
  if n = 0 then Buffer.contents buf
  else
    let mg = 4. in
    let usable_w = width -. 2. *. mg in
    let usable_h = height -. 2. *. mg in
    let scale_x = usable_w /. float_of_int n in
    let scale_y = usable_h /. float_of_int n in
    let px gx = x +. mg +. float_of_int gx *. scale_x in
    let py gy = y +. mg +. float_of_int gy *. scale_y in
    (* Background grid *)
    for i = 1 to n - 1 do
      addf "<line x1=\"%g\" y1=\"%g\" x2=\"%g\" y2=\"%g\" \
            stroke=\"#ddd\" stroke-width=\"0.5\"/>\n"
        (px i) (py 0) (px i) (py n);
      addf "<line x1=\"%g\" y1=\"%g\" x2=\"%g\" y2=\"%g\" \
            stroke=\"#ddd\" stroke-width=\"0.5\"/>\n"
        (px 0) (py i) (px n) (py i)
    done;
    addf "<rect x=\"%g\" y=\"%g\" width=\"%g\" height=\"%g\" \
          fill=\"none\" stroke=\"#999\" stroke-width=\"1\"/>\n"
      (px 0) (py 0) usable_w usable_h;
    (* Pass 1: collect segments as (gx1,gy1,gx2,gy2,depth,is_horiz) in grid coords *)
    let segments = ref [] in
    let rec collect gx gy gw gh cur_is_h (bias : bool option) depth = function
      | Schrot.Tile _ -> ()
      | Schrot.Frame children ->
        let ch = List2.to_list children in
        let k = List.length ch in
        let leaf_counts = List.map Schrot.size ch in
        let span = if cur_is_h then gh else gw in
        let sizes = match bias with
          | Some b -> distribute_biased ~bias:b span leaf_counts
          | None -> distribute_proportional span leaf_counts in
        let sizes_a = Array.of_list sizes in
        let positions = cumsum sizes in
        let n_frame_children = List.length (List.filter (function
          | Schrot.Frame _ -> true | _ -> false) ch) in
        let frame_idx = ref 0 in
        if cur_is_h then begin
          for i = 1 to k - 1 do
            segments := (gx, gy + positions.(i), gx + gw, gy + positions.(i),
                         depth, true) :: !segments
          done;
          List.iteri (fun i child ->
            let child_bias = match child with
              | Schrot.Frame _ when n_frame_children >= 2 ->
                let b = Some (!frame_idx mod 2 = 1) in
                incr frame_idx; b
              | _ -> None in
            collect gx (gy + positions.(i)) gw sizes_a.(i)
              (not cur_is_h) child_bias (depth + 1) child
          ) ch
        end else begin
          for i = 1 to k - 1 do
            segments := (gx + positions.(i), gy, gx + positions.(i), gy + gh,
                         depth, false) :: !segments
          done;
          List.iteri (fun i child ->
            let child_bias = match child with
              | Schrot.Frame _ when n_frame_children >= 2 ->
                let b = Some (!frame_idx mod 2 = 1) in
                incr frame_idx; b
              | _ -> None in
            collect (gx + positions.(i)) gy sizes_a.(i) gh
              (not cur_is_h) child_bias (depth + 1) child
          ) ch
        end
    in
    collect 0 0 n n is_h None 0 tree;
    let segs = !segments in
    (* Pass 2: detect remaining "+" crosses and nudge deeper segments.
       A cross: H at y=hy from [hx1,hx2] and V at x=vx from [vy1,vy2],
       with hx1 < vx < hx2 AND vy1 < hy < vy2. *)
    let h_segs = List.filter (fun (_, _, _, _, _, h) -> h) segs in
    let v_segs = List.filter (fun (_, _, _, _, _, h) -> not h) segs in
    (* For each cross, nudge the DEEPER segment by 0.4 grid cells *)
    let nudge_amt = 0.4 in
    let h_nudges : ((int * int * int * int), float) Hashtbl.t = Hashtbl.create 4 in
    let v_nudges : ((int * int * int * int), float) Hashtbl.t = Hashtbl.create 4 in
    (* Detect interior crosses: H spans across V, or V spans across H *)
    List.iter (fun (hx1, hy, hx2, _, hd, _) ->
      List.iter (fun (vx, vy1, _, vy2, vd, _) ->
        if vx > hx1 && vx < hx2 && hy > vy1 && hy < vy2 then begin
          if vd >= hd then begin
            let key = (vx, vy1, vy2, vd) in
            if not (Hashtbl.mem v_nudges key) then
              Hashtbl.replace v_nudges key
                (if vx - hx1 >= hx2 - vx then -. nudge_amt else nudge_amt)
          end else begin
            let key = (hx1, hy, hx2, hd) in
            if not (Hashtbl.mem h_nudges key) then
              Hashtbl.replace h_nudges key
                (if hy - vy1 >= vy2 - hy then -. nudge_amt else nudge_amt)
          end
        end
      ) v_segs
    ) h_segs;
    (* Detect double T-junctions: two H segments from opposite sides meet
       a V segment at the same point, creating a visual cross.
       H1 ends at (vx, hy) from the left, H2 starts at (vx, hy) from the right. *)
    List.iter (fun (vx, vy1, _, vy2, vd, _) ->
      if vy1 < vy2 then begin
        let h_ending_here = List.filter (fun (_, hy, hx2, _, _, _) ->
          hx2 = vx && hy > vy1 && hy < vy2) h_segs in
        let h_starting_here = List.filter (fun (hx1, hy, _, _, _, _) ->
          hx1 = vx && hy > vy1 && hy < vy2) h_segs in
        List.iter (fun (_, hy1, _, _, _, _) ->
          let has_match = List.exists (fun (_, hy2, _, _, _, _) -> hy1 = hy2)
            h_starting_here in
          if has_match then begin
            let key = (vx, vy1, vy2, vd) in
            if not (Hashtbl.mem v_nudges key) then
              Hashtbl.replace v_nudges key nudge_amt
          end
        ) h_ending_here
      end
    ) v_segs;
    (* Same for two V segments meeting an H segment *)
    List.iter (fun (hx1, hy, hx2, _, hd, _) ->
      if hx1 < hx2 then begin
        let v_ending_here = List.filter (fun (vx, _, _, vy2, _, _) ->
          vy2 = hy && vx > hx1 && vx < hx2) v_segs in
        let v_starting_here = List.filter (fun (vx, vy1, _, _, _, _) ->
          vy1 = hy && vx > hx1 && vx < hx2) v_segs in
        List.iter (fun (vx1, _, _, _, _, _) ->
          let has_match = List.exists (fun (vx2, _, _, _, _, _) -> vx1 = vx2)
            v_starting_here in
          if has_match then begin
            let key = (hx1, hy, hx2, hd) in
            if not (Hashtbl.mem h_nudges key) then
              Hashtbl.replace h_nudges key nudge_amt
          end
        ) v_ending_here
      end
    ) h_segs;
    (* Draw: deepest first, with nudges *)
    let sorted = List.sort (fun (_, _, _, _, d1, _) (_, _, _, _, d2, _) ->
      compare d2 d1) segs in
    List.iter (fun (gx1, gy1, gx2, gy2, depth, is_horiz) ->
      let color = cut_color ~depth ~max_depth in
      if is_horiz then begin
        let key = (gx1, gy1, gx2, depth) in
        let dy = match Hashtbl.find_opt h_nudges key with
          | Some d -> d *. scale_y | None -> 0. in
        addf "<line x1=\"%g\" y1=\"%g\" x2=\"%g\" y2=\"%g\" \
              stroke=\"%s\" stroke-width=\"1.5\"/>\n"
          (px gx1) (py gy1 +. dy) (px gx2) (py gy2 +. dy) color
      end else begin
        let key = (gx1, gy1, gy2, depth) in
        let dx = match Hashtbl.find_opt v_nudges key with
          | Some d -> d *. scale_x | None -> 0. in
        addf "<line x1=\"%g\" y1=\"%g\" x2=\"%g\" y2=\"%g\" \
              stroke=\"%s\" stroke-width=\"1.5\"/>\n"
          (px gx1 +. dx) (py gy1) (px gx1 +. dx) (py gy2) color
      end
    ) sorted;
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
