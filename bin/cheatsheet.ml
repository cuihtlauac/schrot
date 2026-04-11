(* SVG cheat sheet: one row per transformation, before -> after.
   Common starting tiling, highlighted selected tile + affected tiles,
   arrow overlay for oriented operations. *)

let tile_w = 160.
let tile_h = 130.
let margin = 8.
let gap = 14.
let arrow_gap = 40.
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
  (* Tile fills *)
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
  (* Cut lines *)
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
  (* Border *)
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
      | `Left  -> (-14., 0.)
      | `Right -> (14., 0.)
      | `Up    -> (0., -14.)
      | `Down  -> (0., 14.)
    in
    let buf = Buffer.create 128 in
    Printf.bprintf buf
      "<line x1=\"%g\" y1=\"%g\" x2=\"%g\" y2=\"%g\" \
       stroke=\"white\" stroke-width=\"2.5\" marker-end=\"url(#arrowhead)\"/>\n"
      (cx -. dx *. 0.6) (cy -. dy *. 0.6) (cx +. dx *. 0.8) (cy +. dy *. 0.8);
    Buffer.contents buf

(* One row: label, before tiling, arrow, after tiling *)
let render_row ~y ~label ~sublabel ~selected ~affected ~dir ~before ~after =
  let buf = Buffer.create 4096 in
  let add = Buffer.add_string buf in
  let addf fmt = Printf.ksprintf add fmt in
  let lx = gap in
  let bx = 130. in
  let ax = bx +. tile_w in
  let rx = ax +. arrow_gap in
  (* Row label *)
  addf "<text x=\"%g\" y=\"%g\" font-size=\"14\" font-weight=\"bold\" \
        font-family=\"monospace\" fill=\"#333\">%s</text>\n"
    lx (y +. tile_h /. 2. -. 4.) label;
  addf "<text x=\"%g\" y=\"%g\" font-size=\"10\" \
        font-family=\"monospace\" fill=\"#888\">%s</text>\n"
    lx (y +. tile_h /. 2. +. 12.) sublabel;
  (* Before *)
  add (render_tile ~x:bx ~y ~selected ~affected before);
  (match dir with
   | Some d -> add (render_arrow ~x:bx ~y ~selected ~dir:d before)
   | None -> ());
  (* Arrow between *)
  let amx = ax +. arrow_gap /. 2. in
  let amy = y +. tile_h /. 2. in
  addf "<text x=\"%g\" y=\"%g\" font-size=\"20\" text-anchor=\"middle\" \
        dominant-baseline=\"central\" fill=\"#666\">\xE2\x86\x92</text>\n"
    amx amy;
  (* After *)
  add (render_tile ~x:rx ~y ~selected ~affected after);
  (* Tiling strings *)
  addf "<text x=\"%g\" y=\"%g\" font-size=\"9\" font-family=\"monospace\" \
        fill=\"#999\">%s</text>\n"
    bx (y +. tile_h +. label_y_off) (Tiling.to_string before);
  addf "<text x=\"%g\" y=\"%g\" font-size=\"9\" font-family=\"monospace\" \
        fill=\"#999\">%s</text>\n"
    rx (y +. tile_h +. label_y_off) (Tiling.to_string after);
  Buffer.contents buf

let () =
  let output_dir = ref "svg" in
  Arg.parse [
    "--output", Arg.Set_string output_dir, "Output directory";
  ] (fun _ -> ()) "cheatsheet [--output DIR]";
  (try Sys.mkdir !output_dir 0o755 with Sys_error _ -> ());

  (* Common starting tiling: h(5, v(4, 3, h(v(2, 1), 0)), 6)
     Geometry:
              5
     -------------------
           |       | 2 | 1
       4   |   3   |-------
           |       |   0
     -------------------
              6

     v-frame is 3-ary [4, 3, h(v(2,1), 0)] -> 2-subframe, slide
     v(2,1) is 2-ary all-Tile inside h-frame -> dissolve, exit
     Tile 3 adjacent to h-frame in v-frame -> enter
     Tile 6 at bottom makes the v-frame cut non-wall
  *)
  let t : Tiling.t = (true, Schrot.unit_frame (List2.Cons2 (
    Tile 5,
    Schrot.unit_frame (List2.Cons2 (Tile 4, Tile 3,
      [Schrot.unit_frame (List2.Cons2 (
        Schrot.unit_frame (List2.Cons2 (Tile 2, Tile 1, [])),
        Tile 0, []))])),
    [Tile 6]))) in

  let apply f = match f t with Some t' -> t' | None -> t in

  let rows = [
    (* Split: split tile 3 to the right (V, After) *)
    ("Split", "Alt+Right on 3",
     3, [7],
     Some `Right,
     t, Tiling.split 3 Tiling.V t);

    (* Close: close tile 3 *)
    ("Close", "Alt+Del on 3",
     3, [],
     None,
     t, Tiling.close 3 t);

    (* Slide: swap tile 3 with the h(v(2,1),0) sub-frame *)
    ("Slide", "Shift+Right on 3",
     3, [2; 1; 0],
     Some `Right,
     t, apply (Tiling.wall_slide 3 0));

    (* 2-subframe: group tiles 3 and 4 into a sub-frame *)
    ("2-subframe", "f+Left on 3",
     3, [4],
     Some `Left,
     t, apply (Tiling.simple_create 4 3));

    (* Enter: push tile 3 into h(v(2,1), 0), pairing with tile 2 *)
    ("Enter", "e+Right on 3",
     3, [2],
     Some `Right,
     t, apply (Tiling.pivot_in 3 2));

    (* Exit: extract tile 2 from v(2,1) *)
    ("Exit", "x on 2",
     2, [1],
     None,
     t, apply (Tiling.pivot_out 2));

    (* Dissolve: dissolve v(2,1) pair *)
    ("Dissolve", "d on 2",
     2, [1],
     None,
     t, apply (Tiling.simple_dissolve 2));
  ] in

  let nrows = List.length rows in
  let total_w = 130. +. tile_w +. arrow_gap +. tile_w +. gap in
  let total_h = float_of_int nrows *. row_h +. gap in

  let buf = Buffer.create 16384 in
  let add = Buffer.add_string buf in
  let addf fmt = Printf.ksprintf add fmt in
  addf "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%g\" height=\"%g\">\n"
    total_w total_h;
  addf "<rect width=\"%g\" height=\"%g\" fill=\"white\"/>\n" total_w total_h;
  (* Arrow marker definition *)
  add "<defs><marker id=\"arrowhead\" markerWidth=\"8\" markerHeight=\"6\" \
       refX=\"7\" refY=\"3\" orient=\"auto\"><polygon points=\"0 0, 8 3, 0 6\" \
       fill=\"white\"/></marker></defs>\n";

  List.iteri (fun i (label, sublabel, selected, affected, dir, before, after) ->
    let y = gap +. float_of_int i *. row_h in
    add (render_row ~y ~label ~sublabel ~selected ~affected ~dir ~before ~after)
  ) rows;

  (* Legend *)
  let ly = total_h -. 20. in
  addf "<rect x=\"%g\" y=\"%g\" width=\"12\" height=\"12\" fill=\"%s\" stroke=\"#ccc\" stroke-width=\"0.5\"/>\n"
    gap ly selected_color;
  addf "<text x=\"%g\" y=\"%g\" font-size=\"10\" font-family=\"monospace\" fill=\"#666\">selected</text>\n"
    (gap +. 16.) (ly +. 10.);
  addf "<rect x=\"%g\" y=\"%g\" width=\"12\" height=\"12\" fill=\"%s\" stroke=\"#ccc\" stroke-width=\"0.5\"/>\n"
    (gap +. 80.) ly affected_color;
  addf "<text x=\"%g\" y=\"%g\" font-size=\"10\" font-family=\"monospace\" fill=\"#666\">affected</text>\n"
    (gap +. 96.) (ly +. 10.);
  addf "<rect x=\"%g\" y=\"%g\" width=\"12\" height=\"12\" fill=\"%s\" stroke=\"#ccc\" stroke-width=\"0.5\"/>\n"
    (gap +. 170.) ly unchanged_color;
  addf "<text x=\"%g\" y=\"%g\" font-size=\"10\" font-family=\"monospace\" fill=\"#666\">unchanged</text>\n"
    (gap +. 186.) (ly +. 10.);

  add "</svg>\n";

  let path = Printf.sprintf "%s/cheatsheet.svg" !output_dir in
  write_file path (Buffer.contents buf);
  Printf.printf "Wrote %s\n" path
