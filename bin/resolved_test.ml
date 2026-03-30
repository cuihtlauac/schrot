open Nachum

let tile_w = 200.
let tile_h = 200.
let margin = 10.
let gap = 20.

let write_file path content =
  let oc = open_out path in
  output_string oc content;
  close_out oc

let label_tiling (is_h, unit_tree) =
  let counter = ref 0 in
  let rec label = function
    | Schrot.Tile () -> let k = !counter in incr counter; Schrot.Tile k
    | Schrot.Frame ch -> Schrot.Frame (List2.map label ch)
  in
  ((is_h, label unit_tree) : Tiling.t)

let () =
  let output_dir = ref "svg" in
  let max_leaves = ref 6 in
  Arg.parse [
    "--output", Arg.Set_string output_dir, "DIR Output directory (default: svg)";
    "--max-leaves", Arg.Set_int max_leaves, "N Max leaves (default: 6)";
  ] (fun _ -> ()) "resolved_test [--output DIR] [--max-leaves N]";
  let dir = !output_dir in
  for n = 2 to !max_leaves do
    let tilings = Schrot.enum n in
    let labeled = List.map label_tiling tilings in
    let with_degen = List.filter (fun t ->
      Tiling.degenerate_corners t <> []
    ) labeled in
    let all = if with_degen = [] then labeled else with_degen in
    let cols = 6 in
    let n_tilings = List.length all in
    let rows = (n_tilings + cols - 1) / cols in
    let cell_h = tile_h +. 30. in
    let page_w = float_of_int cols *. (tile_w +. gap) +. gap in
    let page_h = float_of_int rows *. (cell_h +. gap) +. gap in
    let buf = Buffer.create 4096 in
    let add = Buffer.add_string buf in
    let addf fmt = Printf.ksprintf add fmt in
    addf "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%g\" height=\"%g\">\n"
      page_w page_h;
    add "<rect width=\"100%\" height=\"100%\" fill=\"white\"/>\n";
    List.iteri (fun idx t ->
      let col = idx mod cols in
      let row = idx / cols in
      let x = gap +. float_of_int col *. (tile_w +. gap) in
      let y = gap +. float_of_int row *. (cell_h +. gap) in
      add (Svg.render_tiling_group ~x ~y ~width:tile_w ~height:tile_h
        ~margin ~show_dots:false t);
      let label = Tiling.to_string t in
      let degen = Tiling.degenerate_corners t in
      let n_degen = List.length degen in
      let n_cross = List.length (List.filter (fun (_, m) -> m >= 4) degen) in
      let n_t = n_degen - n_cross in
      addf "<text x=\"%g\" y=\"%g\" font-size=\"9\" \
            font-family=\"monospace\" fill=\"black\">%s</text>\n"
        x (y +. tile_h +. 15.) label;
      addf "<text x=\"%g\" y=\"%g\" font-size=\"9\" \
            font-family=\"monospace\" fill=\"#666\">T:%d +:%d</text>\n"
        x (y +. tile_h +. 26.) n_t n_cross
    ) all;
    add "</svg>\n";
    let path = Printf.sprintf "%s/resolved_%d.svg" dir n in
    write_file path (Buffer.contents buf);
    Printf.printf "n=%d: %d tilings (%d with degenerate vertices) -> %s\n%!"
      n n_tilings (List.length with_degen) path
  done
