open Nachum

let tile_w = 200.
let tile_h = 200.
let margin = 10.
let label_h = 46.
let gap = 20.

let write_file path content =
  let oc = open_out path in
  output_string oc content;
  close_out oc

let render_labeled_tiling ~x ~y ~lines tiling =
  let buf = Buffer.create 1024 in
  let add = Buffer.add_string buf in
  let addf fmt = Printf.ksprintf add fmt in
  List.iteri (fun i line ->
    addf "<text x=\"%g\" y=\"%g\" font-size=\"11\" \
          font-family=\"monospace\" fill=\"black\">%s</text>\n"
      (x +. margin) (y +. 12. +. float_of_int i *. 14.) line
  ) lines;
  add (Svg.render_tiling_group ~x ~y:(y +. label_h)
    ~width:tile_w ~height:tile_h ~margin tiling);
  Buffer.contents buf

(* Enumerate all tilings with n leaves and render them in a grid *)
let enum_page n cols output_dir =
  let tilings = Schrot.enum n in
  let count = List.length tilings in
  let rows = (count + cols - 1) / cols in
  let cell_w = tile_w +. gap in
  let cell_h = tile_h +. label_h +. gap in
  let svg_w = float_of_int cols *. cell_w +. gap in
  let svg_h = float_of_int rows *. cell_h +. gap in
  let buf = Buffer.create 4096 in
  let add = Buffer.add_string buf in
  let addf fmt = Printf.ksprintf add fmt in
  addf "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%g\" height=\"%g\">\n"
    svg_w svg_h;
  addf "<rect width=\"%g\" height=\"%g\" fill=\"white\"/>\n" svg_w svg_h;
  (* Label the tilings with sequential leaf labels *)
  let labeled = List.mapi (fun i (is_h, tree) ->
    let counter = ref 0 in
    let rec label = function
      | Schrot.Tile () -> let n = !counter in incr counter; Schrot.Tile n
      | Schrot.Frame ch -> Schrot.Frame (List2.map label ch)
    in
    (i, (is_h, label tree))
  ) tilings in
  List.iter (fun (i, tiling) ->
    let col = i mod cols in
    let row = i / cols in
    let x = gap +. float_of_int col *. cell_w in
    let y = gap +. float_of_int row *. cell_h in
    let lines = [Printf.sprintf "#%d  %s" i (Tiling.to_string tiling)] in
    add (render_labeled_tiling ~x ~y ~lines tiling)
  ) labeled;
  add "</svg>\n";
  let path = Printf.sprintf "%s/enum_%d.svg" output_dir n in
  write_file path (Buffer.contents buf);
  Printf.printf "Wrote %s (%d tilings)\n" path count

(* Show split and close operations on a few tilings *)
let operations_page output_dir =
  let examples = [
    (* Start with simple 2-tile tilings *)
    (true, Schrot.Frame (List2.Cons2 (Tile 0, Tile 1, [])));
    (false, Schrot.Frame (List2.Cons2 (Tile 0, Tile 1, [])));
    (* 3-way split *)
    (true, Schrot.Frame (List2.Cons2 (Tile 0, Tile 1, [Tile 2])));
    (* Nested *)
    (true, Schrot.Frame (List2.Cons2 (
      Tile 0,
      Frame (List2.Cons2 (Tile 1, Tile 2, [])),
      [])));
  ] in
  let ops_per_example = 5 in (* before, split_h 0, split_v 0, close 0, close 1 *)
  let cols = ops_per_example in
  let rows = List.length examples in
  let cell_w = tile_w +. gap in
  let cell_h = tile_h +. label_h +. gap in
  let svg_w = float_of_int cols *. cell_w +. gap in
  let svg_h = float_of_int rows *. cell_h +. gap in
  let buf = Buffer.create 4096 in
  let add = Buffer.add_string buf in
  let addf fmt = Printf.ksprintf add fmt in
  addf "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%g\" height=\"%g\">\n"
    svg_w svg_h;
  addf "<rect width=\"%g\" height=\"%g\" fill=\"white\"/>\n" svg_w svg_h;
  List.iteri (fun row tiling ->
    let y = gap +. float_of_int row *. cell_h in
    let input_s = Tiling.to_string tiling in
    let render col op result =
      let x = gap +. float_of_int col *. cell_w in
      let lines = [op; "in:  " ^ input_s; "out: " ^ Tiling.to_string result] in
      add (render_labeled_tiling ~x ~y ~lines result)
    in
    render 0 "original" tiling;
    let sh = Tiling.split 0 Tiling.H tiling in
    render 1 "split 0 H" sh;
    let sv = Tiling.split 0 Tiling.V tiling in
    render 2 "split 0 V" sv;
    let c0 = Tiling.close 0 tiling in
    render 3 "close 0" c0;
    (if Tiling.size tiling > 2 then begin
      let c1 = Tiling.close 1 tiling in
      render 4 "close 1" c1
    end)
  ) examples;
  add "</svg>\n";
  let path = Printf.sprintf "%s/operations.svg" output_dir in
  write_file path (Buffer.contents buf);
  Printf.printf "Wrote %s\n" path

let () =
  let output_dir = ref "svg" in
  let max_leaves = ref 4 in
  Arg.parse [
    "--output", Arg.Set_string output_dir, "DIR Output directory (default: svg)";
    "--max-leaves", Arg.Set_int max_leaves, "N Max leaves for enumeration (default: 4)";
  ] (fun _ -> ()) "tiling_test [--output DIR] [--max-leaves N]";
  (try Sys.mkdir !output_dir 0o755 with Sys_error _ -> ());
  for n = 1 to !max_leaves do
    enum_page n 8 !output_dir
  done;
  operations_page !output_dir
