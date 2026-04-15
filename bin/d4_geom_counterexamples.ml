
(* D4 geometric adjacency counterexamples.
   Shows tilings where D4 orbit members have non-isomorphic
   geometric adjacency after resolve_splits. *)

let label_tiling (is_h, unit_tree) =
  let counter = ref 0 in
  let rec label = function
    | Schrot.Tile () -> let k = !counter in incr counter; Schrot.Tile k
    | Schrot.Frame ch -> Schrot.Frame (List2.map (fun (w, c) -> (w, label c)) ch)
  in
  ((is_h, label unit_tree) : Tiling.t)

let write_file path content =
  let oc = open_out path in
  output_string oc content;
  close_out oc

let degree_seq (g : Geom.t) =
  let n = List.length g.rects in
  let deg = Array.make n 0 in
  let ids = List.map fst g.rects in
  let idx_of id = let rec go i = function
    | [] -> 0 | x :: _ when x = id -> i | _ :: rest -> go (i+1) rest
    in go 0 ids in
  List.iter (fun (a, b) ->
    deg.(idx_of a) <- deg.(idx_of a) + 1;
    deg.(idx_of b) <- deg.(idx_of b) + 1
  ) g.adjacency;
  let s = Array.to_list deg in
  List.sort compare s

let degree_seq_str ds =
  "[" ^ String.concat "," (List.map string_of_int ds) ^ "]"

(* Find counterexample pairs: one per distinct D4 action, smallest n first *)
let find_counterexamples max_n =
  let results = ref [] in
  let d4_names = Tiling.d4_actions in
  let seen_actions = Hashtbl.create 8 in
  for n = 2 to max_n do
    let tilings = Schrot.enum n in
    let labeled = List.map label_tiling tilings in
    List.iter (fun t ->
      let g = Geom.of_tiling t in
      let ds1 = degree_seq g in
      Array.iter (fun (name, action) ->
        if not (Hashtbl.mem seen_actions name) then begin
          let t2 = action t in
          let g2 = Geom.of_tiling t2 in
          let ds2 = degree_seq g2 in
          if ds1 <> ds2 then begin
            Hashtbl.add seen_actions name true;
            results := (t, g, t2, g2, name, ds1, ds2) :: !results
          end
        end
      ) d4_names
    ) labeled
  done;
  List.rev !results

(* Render a degree label below the adjacency graph *)
let render_degree_label ~x ~y ~width ds =
  Printf.sprintf
    "<text x=\"%g\" y=\"%g\" font-size=\"11\" font-family=\"monospace\" \
     text-anchor=\"middle\" fill=\"#c00\">degrees: %s</text>\n"
    (x +. width /. 2.) y (degree_seq_str ds)

let tile_w = 180.
let tile_h = 180.
let adj_w = 180.
let adj_h = 150.
let margin = 10.
let col_gap = 30.
let row_gap = 40.
let label_h = 20.
let arrow_w = 40.

let () =
  let output = ref "svg" in
  let max_n = ref 7 in
  let _ = () in
  Arg.parse [
    "--output", Arg.Set_string output, "DIR Output directory (default: svg)";
    "--max-leaves", Arg.Set_int max_n, "N Max tiling size (default: 7)";
  ] (fun _ -> ()) "d4_geom_counterexamples [--output DIR]";
  let pairs = find_counterexamples !max_n in
  let n_pairs = List.length pairs in
  if n_pairs = 0 then
    Printf.printf "No counterexamples found up to n=%d\n" !max_n
  else begin
    (* Layout: each row has [tiling1] [adj1] --name--> [tiling2] [adj2]
       with degree sequences below each adjacency graph *)
    let row_h = label_h +. tile_h +. label_h +. adj_h +. label_h in
    let total_w = tile_w +. col_gap +. adj_w +. col_gap +. arrow_w
                  +. col_gap +. tile_w +. col_gap +. adj_w +. 2. *. margin in
    let total_h = float_of_int n_pairs *. (row_h +. row_gap) +. margin +. 30. in
    let buf = Buffer.create 8192 in
    let add = Buffer.add_string buf in
    let addf fmt = Printf.ksprintf add fmt in
    addf "<svg xmlns=\"http://www.w3.org/2000/svg\" \
          width=\"%g\" height=\"%g\" viewBox=\"0 0 %g %g\">\n"
      total_w total_h total_w total_h;
    addf "<rect width=\"%g\" height=\"%g\" fill=\"white\"/>\n" total_w total_h;
    (* Title *)
    addf "<text x=\"%g\" y=\"20\" font-size=\"14\" font-family=\"sans-serif\" \
          font-weight=\"bold\" text-anchor=\"middle\" \
          fill=\"#333\">resolve_splits breaks D4 adjacency covariance</text>\n"
      (total_w /. 2.);
    List.iteri (fun i (t1, g1, t2, g2, d4_name, ds1, ds2) ->
      let base_y = 30. +. float_of_int i *. (row_h +. row_gap) in
      let left_x = margin in
      (* --- Left side: original tiling --- *)
      (* Tree label *)
      addf "<text x=\"%g\" y=\"%g\" font-size=\"11\" font-family=\"monospace\" \
            text-anchor=\"middle\" fill=\"#333\">%s</text>\n"
        (left_x +. tile_w /. 2.) (base_y +. 14.)
        (Tiling.to_string t1);
      (* Tiling *)
      let ty = base_y +. label_h in
      add (Svg.render_tiling_group ~x:left_x ~y:ty ~width:tile_w ~height:tile_h
             ~margin ~show_dots:false t1);
      (* Adjacency graph *)
      let adj1_x = left_x +. tile_w +. col_gap in
      let adj1_y = ty +. (tile_h -. adj_h) /. 2. in
      addf "<text x=\"%g\" y=\"%g\" font-size=\"10\" font-family=\"monospace\" \
            text-anchor=\"middle\" fill=\"#666\">adjacency (%d edges)</text>\n"
        (adj1_x +. adj_w /. 2.) (adj1_y -. 4.)
        (List.length (Geom.edges g1));
      add (Svg.render_adjacency_graph ~x:adj1_x ~y:adj1_y
             ~width:adj_w ~height:adj_h g1);
      (* Degree sequence *)
      add (render_degree_label
             ~x:adj1_x ~y:(adj1_y +. adj_h +. 14.) ~width:adj_w ds1);
      (* --- Arrow --- *)
      let arrow_x = adj1_x +. adj_w +. col_gap /. 2. in
      let arrow_y = ty +. tile_h /. 2. in
      addf "<line x1=\"%g\" y1=\"%g\" x2=\"%g\" y2=\"%g\" \
            stroke=\"#999\" stroke-width=\"1.5\" marker-end=\"url(#arrowhead)\"/>\n"
        arrow_x arrow_y (arrow_x +. arrow_w) arrow_y;
      addf "<text x=\"%g\" y=\"%g\" font-size=\"10\" font-family=\"monospace\" \
            text-anchor=\"middle\" fill=\"#666\">%s</text>\n"
        (arrow_x +. arrow_w /. 2.) (arrow_y -. 10.) d4_name;
      (* --- Right side: D4 image --- *)
      let right_x = arrow_x +. arrow_w +. col_gap /. 2. in
      (* Tree label *)
      addf "<text x=\"%g\" y=\"%g\" font-size=\"11\" font-family=\"monospace\" \
            text-anchor=\"middle\" fill=\"#333\">%s</text>\n"
        (right_x +. tile_w /. 2.) (base_y +. 14.)
        (Tiling.to_string t2);
      (* Tiling *)
      add (Svg.render_tiling_group ~x:right_x ~y:ty ~width:tile_w ~height:tile_h
             ~margin ~show_dots:false t2);
      (* Adjacency graph *)
      let adj2_x = right_x +. tile_w +. col_gap in
      let adj2_y = adj1_y in
      addf "<text x=\"%g\" y=\"%g\" font-size=\"10\" font-family=\"monospace\" \
            text-anchor=\"middle\" fill=\"#666\">adjacency (%d edges)</text>\n"
        (adj2_x +. adj_w /. 2.) (adj2_y -. 4.)
        (List.length (Geom.edges g2));
      add (Svg.render_adjacency_graph ~x:adj2_x ~y:adj2_y
             ~width:adj_w ~height:adj_h g2);
      (* Degree sequence *)
      add (render_degree_label
             ~x:adj2_x ~y:(adj2_y +. adj_h +. 14.) ~width:adj_w ds2);
      (* Separator line *)
      if i < n_pairs - 1 then begin
        let sep_y = base_y +. row_h +. row_gap /. 2. in
        addf "<line x1=\"%g\" y1=\"%g\" x2=\"%g\" y2=\"%g\" \
              stroke=\"#ddd\" stroke-width=\"1\" stroke-dasharray=\"6,4\"/>\n"
          margin sep_y (total_w -. margin) sep_y
      end
    ) pairs;
    (* Arrowhead marker *)
    add "<defs><marker id=\"arrowhead\" markerWidth=\"10\" markerHeight=\"7\" \
         refX=\"10\" refY=\"3.5\" orient=\"auto\">\
         <polygon points=\"0 0, 10 3.5, 0 7\" fill=\"#999\"/>\
         </marker></defs>\n";
    add "</svg>\n";
    let path = Printf.sprintf "%s/d4_geom_counterexamples.svg" !output in
    write_file path (Buffer.contents buf);
    Printf.printf "Wrote %d counterexample pairs to %s\n" n_pairs path
  end
