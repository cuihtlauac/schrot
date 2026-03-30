open Nachum

let tile_w = 300.
let tile_h = 300.
let margin = 10.
let gap = 30.

let write_file path content =
  let oc = open_out path in
  output_string oc content;
  close_out oc

let mk l = match List2.of_list_opt l with Some x -> x | None -> assert false

(* Build specific test tilings *)
let examples : (string * Tiling.t) list = [
  "h(v(0,1),v(2,3))",
  (true, Schrot.Frame (mk [
    Schrot.Frame (mk [Schrot.Tile 0; Schrot.Tile 1]);
    Schrot.Frame (mk [Schrot.Tile 2; Schrot.Tile 3])]));

  "h(v(0,1,2),v(3,4,5))",
  (true, Schrot.Frame (mk [
    Schrot.Frame (mk [Schrot.Tile 0; Schrot.Tile 1; Schrot.Tile 2]);
    Schrot.Frame (mk [Schrot.Tile 3; Schrot.Tile 4; Schrot.Tile 5])]));

  "h(0,v(1,2))",
  (true, Schrot.Frame (mk [
    Schrot.Tile 0;
    Schrot.Frame (mk [Schrot.Tile 1; Schrot.Tile 2])]));

  "h(v(0,1),v(2,3),v(4,5))",
  (true, Schrot.Frame (mk [
    Schrot.Frame (mk [Schrot.Tile 0; Schrot.Tile 1]);
    Schrot.Frame (mk [Schrot.Tile 2; Schrot.Tile 3]);
    Schrot.Frame (mk [Schrot.Tile 4; Schrot.Tile 5])]));

  "h(v(0,1),v(2,3,4))",
  (true, Schrot.Frame (mk [
    Schrot.Frame (mk [Schrot.Tile 0; Schrot.Tile 1]);
    Schrot.Frame (mk [Schrot.Tile 2; Schrot.Tile 3; Schrot.Tile 4])]));

  "h(v(0,1),v(2,3,4,5))",
  (true, Schrot.Frame (mk [
    Schrot.Frame (mk [Schrot.Tile 0; Schrot.Tile 1]);
    Schrot.Frame (mk [Schrot.Tile 2; Schrot.Tile 3; Schrot.Tile 4; Schrot.Tile 5])]));

  "h(v(h(0,1),h(2,3)),v(h(4,5),h(6,7)))",
  (true, Schrot.Frame (mk [
    Schrot.Frame (mk [
      Schrot.Frame (mk [Schrot.Tile 0; Schrot.Tile 1]);
      Schrot.Frame (mk [Schrot.Tile 2; Schrot.Tile 3])]);
    Schrot.Frame (mk [
      Schrot.Frame (mk [Schrot.Tile 4; Schrot.Tile 5]);
      Schrot.Frame (mk [Schrot.Tile 6; Schrot.Tile 7])])]));
]

let () =
  let output_dir = ref "svg" in
  Arg.parse [
    "--output", Arg.Set_string output_dir, "DIR Output directory (default: svg)";
  ] (fun _ -> ()) "resolved_examples [--output DIR]";
  let n = List.length examples in
  let cols = min n 4 in
  let rows = (n + cols - 1) / cols in
  let cell_h = tile_h +. 40. in
  let page_w = float_of_int cols *. (tile_w +. gap) +. gap in
  let page_h = float_of_int rows *. (cell_h +. gap) +. gap in
  let buf = Buffer.create 4096 in
  let add = Buffer.add_string buf in
  let addf fmt = Printf.ksprintf add fmt in
  addf "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%g\" height=\"%g\">\n"
    page_w page_h;
  add "<rect width=\"100%\" height=\"100%\" fill=\"white\"/>\n";
  List.iteri (fun idx (name, t) ->
    let col = idx mod cols in
    let row = idx / cols in
    let x = gap +. float_of_int col *. (tile_w +. gap) in
    let y = gap +. float_of_int row *. (cell_h +. gap) in
    add (Svg.render_tiling_group ~x ~y ~width:tile_w ~height:tile_h
      ~margin ~show_dots:false t);
    let degen = Tiling.degenerate_corners t in
    let n_degen = List.length degen in
    addf "<text x=\"%g\" y=\"%g\" font-size=\"11\" font-weight=\"bold\" \
          font-family=\"monospace\" fill=\"black\">%s</text>\n"
      x (y +. tile_h +. 20.) name;
    addf "<text x=\"%g\" y=\"%g\" font-size=\"10\" \
          font-family=\"monospace\" fill=\"#666\">%d degenerate vertices</text>\n"
      x (y +. tile_h +. 33.) n_degen
  ) examples;
  add "</svg>\n";
  let path = Printf.sprintf "%s/resolved_examples.svg" !output_dir in
  write_file path (Buffer.contents buf);
  Printf.printf "Wrote %s\n%!" path
