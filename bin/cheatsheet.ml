(* SVG cheat sheet: 3 rows of forward/inverse operation pairs.

   Each row shows the shared base tiling h(5, v(4, 3, h(v(2, 1), 0)), 6)
   on the left, the post-operation tiling on the right, and two
   horizontal arrows between — forward label on top, inverse label on
   bottom.  Focus highlights and in-tile arrows reflect the real
   keystroke UX: the selected tile on each panel is the one the user
   presses on, the in-tile arrow shows the direction of the
   corresponding arrow key. *)

let tile_w = 200.
let tile_h = 160.
let margin = 8.
let gap = 14.
let label_w = 140.
let between_gap = 120.
let row_h = tile_h +. 60.
let label_y_off = 14.

let write_file path content =
  let oc = open_out path in
  output_string oc content;
  close_out oc

(* Colors *)
let selected_color = "#1a3a6b"   (* dark blue: selected tile *)
let affected_color = "#a8d0e6"   (* light blue: other affected tile *)
let unchanged_color = "#f0f0f0"  (* light gray: unchanged *)

let color_fn selected affected n =
  if n = selected then selected_color
  else if List.mem n affected then affected_color
  else unchanged_color

let text_color_of selected n =
  if n = selected then "white" else "black"

(* Render a tiling with color highlighting *)
let render_tile ~x ~y ~selected ~affected tiling =
  let buf = Buffer.create 2048 in
  let add = Buffer.add_string buf in
  let addf fmt = Printf.ksprintf add fmt in
  let wt = Tiling.resolve_splits tiling in
  let rects = Tiling.rects_of_weighted wt in
  let x0 = x +. margin and y0 = y +. margin in
  let w0 = tile_w -. 2. *. margin and h0 = tile_h -. 2. *. margin in
  List.iter (fun (n, (r : Tiling.rect)) ->
    let ox = x0 +. r.rx *. w0 in
    let oy = y0 +. r.ry *. h0 in
    let ow = r.rw *. w0 in
    let oh = r.rh *. h0 in
    let fill = color_fn selected affected n in
    let tfill = text_color_of selected n in
    addf "<rect x=\"%g\" y=\"%g\" width=\"%g\" height=\"%g\" \
          fill=\"%s\" stroke=\"#ccc\" stroke-width=\"0.5\"/>\n"
      ox oy ow oh fill;
    addf "<text x=\"%g\" y=\"%g\" text-anchor=\"middle\" \
          dominant-baseline=\"central\" font-size=\"13\" \
          fill=\"%s\">%d</text>\n"
      (ox +. ow /. 2.) (oy +. oh /. 2.) tfill n
  ) rects;
  let lines_by_depth = Tiling.cuts_of_weighted wt in
  let max_depth = Array.length lines_by_depth - 1 in
  for depth = max_depth - 1 downto 0 do
    List.iter (fun (lx1, ly1, lx2, ly2) ->
      addf "<line x1=\"%g\" y1=\"%g\" x2=\"%g\" y2=\"%g\" \
            stroke=\"#555\" stroke-width=\"1.5\"/>\n"
        (x0 +. lx1 *. w0) (y0 +. ly1 *. h0)
        (x0 +. lx2 *. w0) (y0 +. ly2 *. h0)
    ) lines_by_depth.(depth)
  done;
  addf "<rect x=\"%g\" y=\"%g\" width=\"%g\" height=\"%g\" \
        fill=\"none\" stroke=\"#333\" stroke-width=\"1.5\"/>\n"
    x0 y0 w0 h0;
  Buffer.contents buf

(* Draw a direction arrow inside the selected tile *)
let render_arrow ~x ~y ~selected ~dir tiling =
  let wt = Tiling.resolve_splits tiling in
  let rects = Tiling.rects_of_weighted wt in
  let x0 = x +. margin and y0 = y +. margin in
  let w0 = tile_w -. 2. *. margin and h0 = tile_h -. 2. *. margin in
  match List.assoc_opt selected rects with
  | None -> ""
  | Some (r : Tiling.rect) ->
    let cx = x0 +. (r.rx +. r.rw /. 2.) *. w0 in
    let cy = y0 +. (r.ry +. r.rh /. 2.) *. h0 in
    let dx, dy = match dir with
      | `Left  -> (-16., 0.)
      | `Right -> (16., 0.)
      | `Up    -> (0., -16.)
      | `Down  -> (0., 16.)
    in
    let buf = Buffer.create 128 in
    Printf.bprintf buf
      "<line x1=\"%g\" y1=\"%g\" x2=\"%g\" y2=\"%g\" \
       stroke=\"white\" stroke-width=\"2.5\" marker-end=\"url(#arrowhead-white)\"/>\n"
      (cx -. dx *. 0.6) (cy -. dy *. 0.6) (cx +. dx *. 0.8) (cy +. dy *. 0.8);
    Buffer.contents buf

type row = {
  label : string;
  sublabel : string;
  forward_op : string;
  inverse_op : string;
  before_selected : int;
  before_affected : int list;
  before_dir : [ `Left | `Right | `Up | `Down ] option;
  after_selected : int;
  after_affected : int list;
  after_dir : [ `Left | `Right | `Up | `Down ] option;
  before : Tiling.t;
  after : Tiling.t;
}

(* One row: label + base + bidirectional labeled arrows + post-op *)
let render_row ~x ~y (r : row) =
  let buf = Buffer.create 4096 in
  let add = Buffer.add_string buf in
  let addf fmt = Printf.ksprintf add fmt in
  let lx = x in
  let bx = x +. label_w in
  let ax1 = bx +. tile_w in
  let ax2 = ax1 +. between_gap in
  let rx = ax2 in
  (* Row label *)
  addf "<text x=\"%g\" y=\"%g\" font-size=\"14\" font-weight=\"bold\" \
        font-family=\"monospace\" fill=\"#333\">%s</text>\n"
    lx (y +. tile_h /. 2. -. 4.) r.label;
  addf "<text x=\"%g\" y=\"%g\" font-size=\"10\" \
        font-family=\"monospace\" fill=\"#888\">%s</text>\n"
    lx (y +. tile_h /. 2. +. 12.) r.sublabel;
  (* Before *)
  add (render_tile ~x:bx ~y ~selected:r.before_selected
         ~affected:r.before_affected r.before);
  (match r.before_dir with
   | Some d -> add (render_arrow ~x:bx ~y ~selected:r.before_selected
                      ~dir:d r.before)
   | None -> ());
  (* Bidirectional arrows between the two tilings *)
  let mid_y = y +. tile_h /. 2. in
  let amid = (ax1 +. ax2) /. 2. in
  let arrow_inset = 10. in
  let line_x1 = ax1 +. arrow_inset in
  let line_x2 = ax2 -. arrow_inset in
  (* Forward arrow: left to right, above midline *)
  addf "<line x1=\"%g\" y1=\"%g\" x2=\"%g\" y2=\"%g\" \
        stroke=\"#555\" stroke-width=\"1.5\" marker-end=\"url(#arrowhead-right)\"/>\n"
    line_x1 (mid_y -. 12.) line_x2 (mid_y -. 12.);
  addf "<text x=\"%g\" y=\"%g\" font-size=\"12\" text-anchor=\"middle\" \
        font-family=\"monospace\" fill=\"#333\">%s</text>\n"
    amid (mid_y -. 18.) r.forward_op;
  (* Inverse arrow: right to left, below midline *)
  addf "<line x1=\"%g\" y1=\"%g\" x2=\"%g\" y2=\"%g\" \
        stroke=\"#555\" stroke-width=\"1.5\" marker-end=\"url(#arrowhead-left)\"/>\n"
    line_x2 (mid_y +. 12.) line_x1 (mid_y +. 12.);
  addf "<text x=\"%g\" y=\"%g\" font-size=\"12\" text-anchor=\"middle\" \
        font-family=\"monospace\" fill=\"#333\">%s</text>\n"
    amid (mid_y +. 28.) r.inverse_op;
  (* After *)
  add (render_tile ~x:rx ~y ~selected:r.after_selected
         ~affected:r.after_affected r.after);
  (match r.after_dir with
   | Some d -> add (render_arrow ~x:rx ~y ~selected:r.after_selected
                      ~dir:d r.after)
   | None -> ());
  (* Tiling strings below each tiling *)
  addf "<text x=\"%g\" y=\"%g\" font-size=\"9\" font-family=\"monospace\" \
        fill=\"#999\">%s</text>\n"
    bx (y +. tile_h +. label_y_off) (Tiling.to_string r.before);
  addf "<text x=\"%g\" y=\"%g\" font-size=\"9\" font-family=\"monospace\" \
        fill=\"#999\">%s</text>\n"
    rx (y +. tile_h +. label_y_off) (Tiling.to_string r.after);
  Buffer.contents buf

let () =
  let output_dir = ref "svg" in
  Arg.parse [
    "--output", Arg.Set_string output_dir, "Output directory";
  ] (fun _ -> ()) "cheatsheet [--output DIR]";
  (try Sys.mkdir !output_dir 0o755 with Sys_error _ -> ());

  (* Shared base tiling: h(5, v(4, 3, h(v(2, 1), 0)), 6)
     Geometry:
              5
     -------------------
           |       | 2 | 1
       4   |   3   |-------
           |       |   0
     -------------------
              6
  *)
  let base : Tiling.t = (true, Schrot.unit_frame (List2.Cons2 (
    Tile 5,
    Schrot.unit_frame (List2.Cons2 (Tile 4, Tile 3,
      [Schrot.unit_frame (List2.Cons2 (
        Schrot.unit_frame (List2.Cons2 (Tile 2, Tile 1, [])),
        Tile 0, []))])),
    [Tile 6]))) in

  let rows = [
    { label = "Split / Close";
      sublabel = "add / remove a tile";
      forward_op = "split"; inverse_op = "close";
      before_selected = 3; before_affected = []; before_dir = Some `Left;
      after_selected = 7; after_affected = []; after_dir = None;
      before = base;
      after = Tiling.split ~side:Before 3 Tiling.V base;
    };
    { label = "Subframe / Dissolve";
      sublabel = "wrap a sibling pair";
      forward_op = "subframe"; inverse_op = "dissolve";
      before_selected = 3; before_affected = [4]; before_dir = Some `Left;
      after_selected = 3; after_affected = [4]; after_dir = None;
      before = base;
      after = (match Tiling.simple_create 4 3 base with
               | Some t -> t | None -> base);
    };
    { label = "Push";
      sublabel = "stem extends, bar yields (self-inverse)";
      forward_op = "push"; inverse_op = "push";
      before_selected = 0; before_affected = [3; 2; 1]; before_dir = Some `Left;
      after_selected = 3; after_affected = [2; 1; 0]; after_dir = Some `Down;
      before = base;
      after = (match Geom.t_flip ~stem:0 ~bar:3 base with
               | Some t -> t | None -> base);
    };
  ] in

  let nrows = List.length rows in
  let total_w = gap +. label_w +. tile_w +. between_gap +. tile_w +. gap in
  let total_h = float_of_int nrows *. row_h +. gap +. 36. in

  let buf = Buffer.create 16384 in
  let add = Buffer.add_string buf in
  let addf fmt = Printf.ksprintf add fmt in
  addf "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%g\" height=\"%g\">\n"
    total_w total_h;
  addf "<rect width=\"%g\" height=\"%g\" fill=\"white\"/>\n" total_w total_h;
  (* Arrow marker definitions *)
  add "<defs>\n";
  add "  <marker id=\"arrowhead-white\" markerWidth=\"8\" markerHeight=\"6\" \
       refX=\"7\" refY=\"3\" orient=\"auto\">\
       <polygon points=\"0 0, 8 3, 0 6\" fill=\"white\"/></marker>\n";
  add "  <marker id=\"arrowhead-right\" markerWidth=\"8\" markerHeight=\"6\" \
       refX=\"7\" refY=\"3\" orient=\"auto\">\
       <polygon points=\"0 0, 8 3, 0 6\" fill=\"#555\"/></marker>\n";
  add "  <marker id=\"arrowhead-left\" markerWidth=\"8\" markerHeight=\"6\" \
       refX=\"7\" refY=\"3\" orient=\"auto\">\
       <polygon points=\"0 0, 8 3, 0 6\" fill=\"#555\"/></marker>\n";
  add "</defs>\n";

  List.iteri (fun i row ->
    let y = gap +. float_of_int i *. row_h in
    add (render_row ~x:gap ~y row)
  ) rows;

  (* Legend *)
  let ly = total_h -. 24. in
  addf "<rect x=\"%g\" y=\"%g\" width=\"12\" height=\"12\" fill=\"%s\" \
        stroke=\"#ccc\" stroke-width=\"0.5\"/>\n"
    gap ly selected_color;
  addf "<text x=\"%g\" y=\"%g\" font-size=\"10\" font-family=\"monospace\" \
        fill=\"#666\">selected</text>\n"
    (gap +. 16.) (ly +. 10.);
  addf "<rect x=\"%g\" y=\"%g\" width=\"12\" height=\"12\" fill=\"%s\" \
        stroke=\"#ccc\" stroke-width=\"0.5\"/>\n"
    (gap +. 80.) ly affected_color;
  addf "<text x=\"%g\" y=\"%g\" font-size=\"10\" font-family=\"monospace\" \
        fill=\"#666\">affected</text>\n"
    (gap +. 96.) (ly +. 10.);
  addf "<rect x=\"%g\" y=\"%g\" width=\"12\" height=\"12\" fill=\"%s\" \
        stroke=\"#ccc\" stroke-width=\"0.5\"/>\n"
    (gap +. 170.) ly unchanged_color;
  addf "<text x=\"%g\" y=\"%g\" font-size=\"10\" font-family=\"monospace\" \
        fill=\"#666\">unchanged</text>\n"
    (gap +. 186.) (ly +. 10.);

  add "</svg>\n";

  let path = Printf.sprintf "%s/cheatsheet.svg" !output_dir in
  write_file path (Buffer.contents buf);
  Printf.printf "Wrote %s\n" path
