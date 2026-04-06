let tile_w = 200.
let tile_h = 200.
let margin = 10.
let label_h = 46.
let gap = 20.

let write_file path content =
  let oc = open_out path in
  output_string oc content;
  close_out oc

let render_labeled_tiling ~x ~y ~bottom_lines tiling =
  let buf = Buffer.create 1024 in
  let add = Buffer.add_string buf in
  let addf fmt = Printf.ksprintf add fmt in
  add (Svg.render_tiling_group ~x ~y ~margin
    ~width:tile_w ~height:tile_h ~show_dots:false tiling);
  let text_y = y +. tile_h +. 6. in
  List.iteri (fun i line ->
    addf "<text x=\"%g\" y=\"%g\" font-size=\"11\" \
          font-family=\"monospace\" fill=\"black\">%s</text>\n"
      (x +. margin) (text_y +. float_of_int i *. 14.) line
  ) bottom_lines;
  Buffer.contents buf

let flips_page output_dir =
  let examples = [
    (* Simple flip at root *)
    (true, Schrot.Frame (List2.Cons2 (Tile 0, Tile 1, [])));
    (* Simple create + wall slide *)
    (true, Schrot.Frame (List2.Cons2 (Tile 0, Tile 1, [Tile 2])));
    (* Simple dissolve + pivot_out + pivot_in *)
    (true, Schrot.Frame (List2.Cons2 (
      Frame (List2.Cons2 (Tile 0, Tile 1, [])),
      Tile 2, [])));
    (* Pivot_out from 3-ary + wall slide *)
    (true, Schrot.Frame (List2.Cons2 (
      Frame (List2.Cons2 (Tile 0, Tile 1, [Tile 2])),
      Tile 3, [])));
    (* Pivot_in from both sides *)
    (false, Schrot.Frame (List2.Cons2 (Tile 0,
      Frame (List2.Cons2 (Tile 1, Tile 2, [])), [Tile 3])));
    (* Complex: 5-tile with multiple flip types *)
    (true, Schrot.Frame (List2.Cons2 (
      Frame (List2.Cons2 (Tile 0, Tile 1, [Tile 2])),
      Tile 3, [Tile 4])));
  ] in
  (* For each example, enumerate flips *)
  let rows = List.map (fun tiling ->
    let flips = Tiling.enumerate_flips tiling in
    (tiling, flips)
  ) examples in
  let max_cols = List.fold_left (fun acc (_, flips) ->
    max acc (1 + List.length flips)
  ) 0 rows in
  let cell_w = tile_w +. gap in
  let cell_h = tile_h +. label_h +. gap in
  let svg_w = float_of_int max_cols *. cell_w +. gap in
  let svg_h = float_of_int (List.length rows) *. cell_h +. gap in
  let buf = Buffer.create 8192 in
  let add = Buffer.add_string buf in
  let addf fmt = Printf.ksprintf add fmt in
  addf "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%g\" height=\"%g\">\n"
    svg_w svg_h;
  addf "<rect width=\"%g\" height=\"%g\" fill=\"white\"/>\n" svg_w svg_h;
  List.iteri (fun row (tiling, flips) ->
    let y = gap +. float_of_int row *. cell_h in
    let input_s = Tiling.to_string tiling in
    (* Original *)
    let x0 = gap in
    add (render_labeled_tiling ~x:x0 ~y
      ~bottom_lines:["original"; input_s] tiling);
    (* Each flip *)
    List.iteri (fun col (flip, result) ->
      let x = gap +. float_of_int (col + 1) *. cell_w in
      let bottom_lines = [
        Tiling.flip_to_string flip;
        Tiling.to_string result
      ] in
      add (render_labeled_tiling ~x ~y ~bottom_lines result)
    ) flips
  ) rows;
  add "</svg>\n";
  let path = Printf.sprintf "%s/flips.svg" output_dir in
  write_file path (Buffer.contents buf);
  Printf.printf "Wrote %s (%d examples, up to %d columns)\n"
    path (List.length rows) max_cols

let label_tiling (is_h, tree) =
  let c = ref 0 in
  let rec go = function
    | Schrot.Tile () -> let n = !c in incr c; Schrot.Tile n
    | Schrot.Frame ch -> Schrot.Frame (List2.map go ch)
  in (is_h, go tree)

(* Collect non-invertible flips up to max_n *)
let collect_counterexamples max_n =
  let cases = ref [] in
  for n = 2 to max_n do
    let tilings = List.map label_tiling (Schrot.enum n) in
    List.iter (fun t ->
      let flips = Tiling.enumerate_flips t in
      List.iter (fun (flip, t') ->
        let reverse = Tiling.enumerate_flips t' in
        let orig_s = Tiling.to_string t in
        let found = List.exists (fun (_, t'') ->
          Tiling.to_string t'' = orig_s
        ) reverse in
        if not found then begin
          let t'_rel = Tiling.relabel t' in
          let t_rel = Tiling.relabel t in
          let rev_rel = Tiling.enumerate_flips t'_rel in
          let found_rel = List.exists (fun (_, t'') ->
            Tiling.to_string t'' = Tiling.to_string t_rel
          ) rev_rel in
          if not found_rel then
            cases := (t, flip, t') :: !cases
        end
      ) flips
    ) tilings
  done;
  List.rev !cases

let counterexamples_page output_dir =
  let cases = collect_counterexamples 5 in
  if cases = [] then begin
    Printf.printf "No counterexamples found up to n=5.\n";
  end else begin
    (* Each row: before -> arrow -> after, labeled with flip name *)
    let pair_w = tile_w *. 2. +. gap *. 2. in
    let cols = 8 in
    let rows = (List.length cases + cols - 1) / cols in
    let cell_w = pair_w +. gap in
    let cell_h = tile_h +. label_h +. gap in
    let svg_w = float_of_int cols *. cell_w +. gap in
    let svg_h = float_of_int rows *. cell_h +. gap in
    let buf = Buffer.create 8192 in
    let add = Buffer.add_string buf in
    let addf fmt = Printf.ksprintf add fmt in
    addf "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%g\" height=\"%g\">\n"
      svg_w svg_h;
    addf "<rect width=\"%g\" height=\"%g\" fill=\"white\"/>\n" svg_w svg_h;
    List.iteri (fun idx (t, flip, t') ->
      let col = idx mod cols in
      let row = idx / cols in
      let bx = gap +. float_of_int col *. cell_w in
      let by = gap +. float_of_int row *. cell_h in
      (* Before *)
      add (render_labeled_tiling ~x:bx ~y:by
        ~bottom_lines:[Tiling.to_string t] t);
      (* Arrow + flip label *)
      let ax = bx +. tile_w +. 2. in
      let ay = by +. tile_h /. 2. in
      addf "<text x=\"%g\" y=\"%g\" font-size=\"10\" \
            font-family=\"monospace\" fill=\"#c00\" \
            text-anchor=\"middle\">%s</text>\n"
        (ax +. gap /. 2.) (ay -. 4.) (Tiling.flip_to_string flip);
      addf "<text x=\"%g\" y=\"%g\" font-size=\"16\" \
            text-anchor=\"middle\" fill=\"#c00\">\xE2\x86\x92</text>\n"
        (ax +. gap /. 2.) (ay +. 12.);
      (* After *)
      let rx = bx +. tile_w +. gap *. 2. in
      add (render_labeled_tiling ~x:rx ~y:by
        ~bottom_lines:[Tiling.to_string t'; "NO INVERSE"] t')
    ) cases;
    add "</svg>\n";
    let path = Printf.sprintf "%s/flip_counterexamples.svg" output_dir in
    write_file path (Buffer.contents buf);
    Printf.printf "Wrote %s (%d counterexamples)\n" path (List.length cases)
  end

let () =
  let output_dir = ref "svg" in
  Arg.parse [
    "--output", Arg.Set_string output_dir, "Output directory (default: svg)";
  ] (fun _ -> ()) "flip_test [--output DIR]";
  (try Sys.mkdir !output_dir 0o755 with Sys_error _ -> ());
  flips_page !output_dir;
  counterexamples_page !output_dir
