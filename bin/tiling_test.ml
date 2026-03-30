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

let render_labeled_tiling ~x ~y ?top_label ~bottom_lines tiling =
  let buf = Buffer.create 1024 in
  let add = Buffer.add_string buf in
  let addf fmt = Printf.ksprintf add fmt in
  (match top_label with
   | Some label ->
     addf "<text x=\"%g\" y=\"%g\" font-size=\"11\" \
           font-family=\"monospace\" text-anchor=\"middle\" \
           dominant-baseline=\"auto\" fill=\"#666\">%s</text>\n"
       (x +. tile_w /. 2.) (y +. 15.) label
   | None -> ());
  add (Svg.render_tiling_group ~x ~y ~margin
    ~width:tile_w ~height:tile_h ~show_dots:false tiling);
  let text_y = y +. tile_h +. 6. in
  List.iteri (fun i line ->
    addf "<text x=\"%g\" y=\"%g\" font-size=\"11\" \
          font-family=\"monospace\" fill=\"black\">%s</text>\n"
      (x +. margin) (text_y +. float_of_int i *. 14.) line
  ) bottom_lines;
  Buffer.contents buf

(* Compare trees: at each level, Tile before Frame, then by arity,
   then recursively by children. The preference for V-before-H at root
   (and alternating at deeper levels) is handled by the tiling comparator. *)
let rec compare_tree t1 t2 =
  match t1, t2 with
  | Schrot.Tile _, Schrot.Tile _ -> 0
  | Schrot.Tile _, Schrot.Frame _ -> -1
  | Schrot.Frame _, Schrot.Tile _ -> 1
  | Schrot.Frame ch1, Schrot.Frame ch2 ->
    let c = compare (List2.length ch1) (List2.length ch2) in
    if c <> 0 then c
    else compare_children (List2.to_list ch1) (List2.to_list ch2)
and compare_children l1 l2 =
  match l1, l2 with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | a :: ra, b :: rb ->
    let c = compare_tree a b in
    if c <> 0 then c else compare_children ra rb

(* Sort tilings: V-root before H-root, then by tree structure *)
let compare_tiling (is_h1, t1) (is_h2, t2) =
  let c = compare is_h1 is_h2 in (* false < true, so V before H *)
  if c <> 0 then c else compare_tree t1 t2

(* Label a unit tiling with sequential leaf numbers *)
let label_unit_tiling (is_h, tree) =
  let counter = ref 0 in
  let rec label = function
    | Schrot.Tile () -> let n = !counter in incr counter; Schrot.Tile n
    | Schrot.Frame ch -> Schrot.Frame (List2.map label ch)
  in
  (is_h, label tree)

(* Group tilings by D4 orbit, preserving sort order within each group *)
let group_by_d4 tilings =
  let indexed = List.mapi (fun i t -> (i, t)) tilings in
  let tbl : (string, (int * Tiling.t) list) Hashtbl.t = Hashtbl.create 64 in
  let order : (string, int) Hashtbl.t = Hashtbl.create 64 in
  List.iter (fun (i, unit_tiling) ->
    let labeled = label_unit_tiling unit_tiling in
    let canon = Tiling.canonical_d4 labeled in
    let prev = try Hashtbl.find tbl canon with Not_found -> [] in
    Hashtbl.replace tbl canon ((i, labeled) :: prev);
    if not (Hashtbl.mem order canon) then
      Hashtbl.replace order canon i
  ) indexed;
  let groups = Hashtbl.fold (fun canon members acc ->
    let first = Hashtbl.find order canon in
    (first, List.rev members) :: acc
  ) tbl [] in
  List.sort (fun (a, _) (b, _) -> compare a b) groups
  |> List.map snd

let sep_gap = 10.

(* Enumerate all tilings with n leaves, grouped by D4 orbit *)
let enum_page n cols output_dir =
  let tilings = List.sort compare_tiling (Schrot.enum n) in
  let count = List.length tilings in
  let groups = group_by_d4 tilings in
  let cell_w = tile_w +. gap in
  let cell_h = tile_h +. label_h +. gap in
  let max_w = float_of_int cols *. cell_w +. gap in
  (* Layout: place groups left to right, wrapping rows.
     Track (col, row) cursor; separator before each group except first on a row. *)
  let positions = ref [] in
  let separators = ref [] in
  let col = ref 0 in
  let row = ref 0 in
  let max_col = ref 0 in
  List.iter (fun group ->
    let gsize = List.length group in
    (* Does this group fit on the current row? *)
    let needed = float_of_int (!col + gsize) *. cell_w +. gap in
    if !col > 0 && needed > max_w then begin
      col := 0;
      incr row
    end;
    (* Draw separator before group if not at start of row *)
    if !col > 0 then begin
      let sx = gap +. float_of_int !col *. cell_w -. sep_gap /. 2. in
      separators := (sx, !row) :: !separators
    end;
    (* Place each tiling in the group *)
    List.iter (fun (i, tiling) ->
      positions := (i, tiling, !col, !row) :: !positions;
      if !col + 1 > !max_col then max_col := !col + 1;
      incr col
    ) group
  ) groups;
  let total_rows = !row + 1 in
  let total_cols = !max_col in
  let svg_w = float_of_int total_cols *. cell_w +. gap in
  let svg_h = float_of_int total_rows *. cell_h +. gap in
  let buf = Buffer.create 4096 in
  let add = Buffer.add_string buf in
  let addf fmt = Printf.ksprintf add fmt in
  addf "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%g\" height=\"%g\">\n"
    svg_w svg_h;
  addf "<rect width=\"%g\" height=\"%g\" fill=\"white\"/>\n" svg_w svg_h;
  (* Render tilings *)
  List.iter (fun (i, tiling, c, r) ->
    let x = gap +. float_of_int c *. cell_w in
    let y = gap +. float_of_int r *. cell_h in
    let top_label = Printf.sprintf "#%d" i in
    let bottom_lines = [Tiling.to_string tiling] in
    add (render_labeled_tiling ~x ~y ~top_label ~bottom_lines tiling)
  ) (List.rev !positions);
  (* Render dashed vertical separators *)
  List.iter (fun (sx, r) ->
    let y1 = gap +. float_of_int r *. cell_h in
    let y2 = y1 +. cell_h -. gap in
    addf "<line x1=\"%g\" y1=\"%g\" x2=\"%g\" y2=\"%g\" \
          stroke=\"#999\" stroke-width=\"1\" stroke-dasharray=\"4,4\"/>\n"
      sx y1 sx y2
  ) !separators;
  add "</svg>\n";
  let path = Printf.sprintf "%s/schroeder_%d.svg" output_dir n in
  write_file path (Buffer.contents buf);
  Printf.printf "Wrote %s (%d tilings, %d D4 orbits)\n"
    path count (List.length groups)

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
      let bottom_lines = [op; "in:  " ^ input_s; "out: " ^ Tiling.to_string result] in
      add (render_labeled_tiling ~x ~y ~bottom_lines result)
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

(* Print topology equivalence classes for each n *)
let topology_classes max_n =
  for n = 1 to max_n do
    let tilings = List.sort compare_tiling (Schrot.enum n) in
    let indexed = List.mapi (fun i t -> (i, t)) tilings in
    let label t = label_unit_tiling t in
    let group_by_str canon tilings =
      let tbl : (string, int list) Hashtbl.t = Hashtbl.create 32 in
      List.iter (fun (i, t) ->
        let c = canon t in
        let prev = try Hashtbl.find tbl c with Not_found -> [] in
        Hashtbl.replace tbl c (i :: prev)
      ) tilings;
      Hashtbl.length tbl
    in
    let nd4 = group_by_str (fun t -> Tiling.canonical_d4 (label t)) indexed in
    let nv4 = group_by_str (fun t -> Tiling.canonical_v4 (label t)) indexed in
    Printf.printf "n=%d: %d tilings, %d D4 orbits, %d V4 orbits\n"
      n (List.length tilings) nd4 nv4
  done

(* D4 orbit representative page: one tiling per orbit, no labels, with tree diagram *)
let d4_page n output_dir =
  let tilings = Schrot.enum n in
  (* Filter to D4 canonical representatives *)
  let representatives = List.filter_map (fun unit_tiling ->
    let labeled = label_unit_tiling unit_tiling in
    let self_erased =
      let (is_h, tree) = Tiling.erase labeled in
      Tiling.unit_tree_to_string is_h tree
    in
    let canon = Tiling.canonical_d4 labeled in
    if canon = self_erased then Some labeled
    else None
  ) tilings in
  (* Sort by graph fingerprint so isomorphic adjacency graphs appear side by side *)
  let with_fp = List.map (fun t ->
    let g = Geom.of_tiling t in
    let edges = Geom.edge_pairs g in
    let fp = Tiling.adjacency_fingerprint n edges in
    (t, fp)
  ) representatives in
  let representatives = List.sort (fun (_, fp1) (_, fp2) ->
    compare fp1 fp2
  ) with_fp |> List.map fst in
  let count = List.length representatives in
  let tree_w = tile_w in
  let graph_w = tile_w in
  let mid_w = 30. in (* middle column for label + dots *)
  let cell_w = tile_w +. mid_w +. tree_w +. graph_w +. gap in
  let cell_h = tile_h +. gap in
  (* Target landscape aspect ratio sqrt(2):1, accounting for cell shape *)
  let ratio = sqrt 2. *. cell_h /. cell_w in
  let cols = max 1 (int_of_float (ceil (sqrt (ratio *. float_of_int count)))) in
  let rows = (count + cols - 1) / cols in
  let svg_w = float_of_int cols *. cell_w +. gap in
  let svg_h = float_of_int rows *. cell_h +. gap in
  let buf = Buffer.create 4096 in
  let add = Buffer.add_string buf in
  let addf fmt = Printf.ksprintf add fmt in
  addf "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%g\" height=\"%g\">\n"
    svg_w svg_h;
  addf "<rect width=\"%g\" height=\"%g\" fill=\"white\"/>\n" svg_w svg_h;
  List.iteri (fun i tiling ->
    let col = i mod cols in
    let row = i / cols in
    let x = gap +. float_of_int col *. cell_w in
    let y = gap +. float_of_int row *. cell_h in
    (* Tiling with junction-resolved splits *)
    add (Svg.render_tiling_group ~x ~y ~margin
      ~width:tile_w ~height:tile_h ~show_dots:false tiling);
    (* Middle column: number label + color dots vertically *)
    let mid_cx = x +. tile_w +. mid_w /. 2. in
    addf "<text x=\"%g\" y=\"%g\" font-size=\"11\" \
          font-family=\"monospace\" text-anchor=\"middle\" \
          fill=\"black\">#%d</text>\n"
      mid_cx (y +. 14.) i;
    let tree = Tiling.tree tiling in
    let max_depth = Schrot.height tree in
    if max_depth > 0 then begin
      let dot_r = 4. in
      let dot_step = 10. in
      let total_h = float_of_int max_depth *. dot_step in
      let start_y = y +. (tile_h -. total_h) /. 2. +. 10. in
      for d = 0 to max_depth - 1 do
        let color = Svg.cut_color ~depth:d ~max_depth in
        let cy = start_y +. float_of_int d *. dot_step in
        addf "<circle cx=\"%g\" cy=\"%g\" r=\"%g\" fill=\"%s\"/>\n"
          mid_cx cy dot_r color
      done
    end;
    (* Tree diagram *)
    add (Svg.render_tree_diagram
      ~x:(x +. tile_w +. mid_w) ~y ~width:tree_w ~height:tile_h tree);
    (* Adjacency graph *)
    let g = Geom.of_tiling tiling in
    add (Svg.render_adjacency_graph
      ~x:(x +. tile_w +. mid_w +. tree_w) ~y
      ~width:graph_w ~height:tile_h g)
  ) representatives;
  add "</svg>\n";
  let path = Printf.sprintf "%s/d4_shrot_%d.svg" output_dir n in
  write_file path (Buffer.contents buf);
  Printf.printf "Wrote %s (%d D4 orbits)\n" path count

let () =
  let output_dir = ref "svg" in
  let max_leaves = ref 4 in
  let max_svg = ref 6 in
  Arg.parse [
    "--output", Arg.Set_string output_dir, "DIR Output directory (default: svg)";
    "--max-leaves", Arg.Set_int max_leaves, "N Max leaves for enumeration (default: 4)";
    "--max-svg", Arg.Set_int max_svg, "N Max leaves for SVG generation (default: 6)";
  ] (fun _ -> ()) "tiling_test [--output DIR] [--max-leaves N] [--max-svg N]";
  (try Sys.mkdir !output_dir 0o755 with Sys_error _ -> ());
  for n = 1 to !max_leaves do
    if n <= !max_svg then begin
      let count = List.length (Schrot.enum n) in
      let cols = max 1 (int_of_float (ceil (sqrt (sqrt 2. *. float_of_int count)))) in
      enum_page n cols !output_dir;
      d4_page n !output_dir
    end
  done;
  if !max_svg >= 1 then operations_page !output_dir;
  topology_classes !max_leaves
