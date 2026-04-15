(* Render the 7 failing T-flip cases from n=7 Property C.
   For each: before tiling, forward T-flip result, and the reverse
   T-flips attempted from the post-flip geometry (new_rects).
   Also shows the T-joints found in each geometry. *)

let tile_w = 180.
let tile_h = 180.
let margin = 8.
let label_h = 58.
let gap = 14.
let arrow_w = 36.

let write_file path content =
  let oc = open_out path in
  output_string oc content;
  close_out oc

let render_tiling ~x ~y ~lines tiling =
  let buf = Buffer.create 1024 in
  let add = Buffer.add_string buf in
  let addf fmt = Printf.ksprintf add fmt in
  add (Svg.render_tiling_group ~x ~y ~margin
    ~width:tile_w ~height:tile_h ~show_dots:false tiling);
  let text_y = y +. tile_h +. 4. in
  List.iteri (fun i line ->
    addf "<text x=\"%g\" y=\"%g\" font-size=\"9\" \
          font-family=\"monospace\" fill=\"black\">%s</text>\n"
      (x +. 4.) (text_y +. float_of_int i *. 12.) line
  ) lines;
  Buffer.contents buf

let render_rects_tiling ~x ~y ~lines rects =
  (* Render a tiling from raw (int * rect) list — not from a tree.
     Draws rectangles directly with tile IDs. *)
  let buf = Buffer.create 1024 in
  let add = Buffer.add_string buf in
  let addf fmt = Printf.ksprintf add fmt in
  let x0 = x +. margin and y0 = y +. margin in
  let w0 = tile_w -. 2. *. margin and h0 = tile_h -. 2. *. margin in
  (* Dashed border *)
  addf "<rect x=\"%g\" y=\"%g\" width=\"%g\" height=\"%g\" \
        fill=\"none\" stroke=\"#ccc\" stroke-dasharray=\"4,2\"/>\n"
    x0 y0 w0 h0;
  List.iter (fun (id, (r : Geom.rect)) ->
    let ox = x0 +. r.x *. w0 in
    let oy = y0 +. r.y *. h0 in
    let ow = r.w *. w0 in
    let oh = r.h *. h0 in
    addf "<rect x=\"%g\" y=\"%g\" width=\"%g\" height=\"%g\" \
          fill=\"white\" stroke=\"#888\" stroke-width=\"0.5\"/>\n" ox oy ow oh;
    addf "<text x=\"%g\" y=\"%g\" font-size=\"10\" \
          font-family=\"sans-serif\" fill=\"#444\" \
          text-anchor=\"middle\" dominant-baseline=\"central\">%d</text>\n"
      (ox +. ow /. 2.) (oy +. oh /. 2.) id
  ) rects;
  let text_y = y +. tile_h +. 4. in
  List.iteri (fun i line ->
    addf "<text x=\"%g\" y=\"%g\" font-size=\"9\" \
          font-family=\"monospace\" fill=\"black\">%s</text>\n"
      (x +. 4.) (text_y +. float_of_int i *. 12.) line
  ) lines;
  Buffer.contents buf

let render_arrow ~x ~y ~label =
  Printf.sprintf
    "<text x=\"%g\" y=\"%g\" font-size=\"8\" font-family=\"monospace\" \
     fill=\"#c00\" text-anchor=\"middle\">%s</text>\n\
     <text x=\"%g\" y=\"%g\" font-size=\"14\" text-anchor=\"middle\" \
     fill=\"#c00\">\xE2\x86\x92</text>\n"
    x (y -. 4.) label x (y +. 10.)

let label_tiling (is_h, tree) =
  let c = ref 0 in
  let rec go = function
    | Schrot.Tile () -> let n = !c in incr c; Schrot.Tile n
    | Schrot.Frame ch -> Schrot.Frame (List2.map (fun (w, c) -> (w, go c)) ch)
  in (is_h, go tree)

(* The 7 failing cases from flip_check --max-leaves 7 *)
let cases = [
  ("h(6, v(h(5, 4), 3, h(2, v(1, 0))))", "t_flip 5 3");
  ("h(6, v(h(5, 4), 3, h(2, v(1, 0))))", "t_flip 4 3");
  ("h(6, v(h(5, 4), 3, h(v(2, 1), 0)))", "t_flip 5 3");
  ("h(6, v(h(5, 4), 3, h(v(2, 1), 0)))", "t_flip 4 3");
  ("h(6, v(5, h(4, v(3, 2))), v(1, 0))", "t_flip 4 6");
  ("h(v(6, 5), 4, 0, v(3, h(2, 1)))", "t_flip 6 4");
  ("h(v(6, 5), 4, 0, v(3, h(2, 1)))", "t_flip 5 4");
]

let () =
  let output_dir = "svg" in
  (try Sys.mkdir output_dir 0o755 with Sys_error _ -> ());
  (* Build tiling lookup *)
  let all_tilings = ref [] in
  for n = 1 to 8 do
    all_tilings := !all_tilings @ List.map label_tiling (Schrot.enum n)
  done;
  let find_tiling s =
    List.find (fun t -> Tiling.to_string t = s) !all_tilings
  in
  (* For each case: find original, apply forward flip, show geometry *)
  let n_cases = List.length cases in
  (* Layout: one row per case.  Each row:
     [before] -> [after] -> [new_rects] -> [reverse attempts...] *)
  let max_rev = 6 in  (* max reverse flips to show *)
  let row_tiles = 3 + max_rev in  (* before + after + new_rects + reverses *)
  let row_w = float_of_int row_tiles *. (tile_w +. arrow_w) +. gap in
  let row_h = tile_h +. label_h +. gap in
  let svg_w = row_w +. gap *. 2. in
  let svg_h = float_of_int n_cases *. row_h +. gap *. 2. in
  let buf = Buffer.create 16384 in
  let add = Buffer.add_string buf in
  let addf fmt = Printf.ksprintf add fmt in
  addf "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%g\" height=\"%g\">\n"
    svg_w svg_h;
  addf "<rect width=\"%g\" height=\"%g\" fill=\"#f8f8f8\"/>\n" svg_w svg_h;
  List.iteri (fun row_idx (tiling_str, flip_str) ->
    let by = gap +. float_of_int row_idx *. row_h in
    let t = find_tiling tiling_str in
    let col = ref 0 in
    let next_x () =
      let x = gap +. float_of_int !col *. (tile_w +. arrow_w) in
      incr col; x
    in
    (* 1. Before tiling *)
    let bx = next_x () in
    add (render_tiling ~x:bx ~y:by ~lines:[tiling_str] t);
    (* Find the specific T-flip *)
    let g = Geom.of_tiling t in
    let all_tflips = Geom.enumerate_t_flips g in
    let flip_result = List.find_opt (fun (f, _, _) ->
      Tiling.flip_to_string f = flip_str
    ) all_tflips in
    (match flip_result with
     | None ->
       let ax = next_x () in
       addf "<text x=\"%g\" y=\"%g\" font-size=\"10\" fill=\"red\">flip not found</text>\n"
         ax (by +. tile_h /. 2.)
     | Some (_flip, t', new_rects) ->
       (* Arrow *)
       let arrow_x = bx +. tile_w +. arrow_w /. 2. in
       add (render_arrow ~x:arrow_x ~y:(by +. tile_h /. 2.) ~label:flip_str);
       (* 2. After tiling (from tree) *)
       let ax = next_x () in
       add (render_tiling ~x:ax ~y:by
         ~lines:[Tiling.to_string t'; "result tree"] t');
       (* Arrow *)
       let arrow_x2 = ax +. tile_w +. arrow_w /. 2. in
       add (render_arrow ~x:arrow_x2 ~y:(by +. tile_h /. 2.) ~label:"new_rects");
       (* 3. Post-flip geometry (new_rects) *)
       let nx = next_x () in
       (* Annotate with T-joints found in new_rects *)
       let g_nr = { Geom.tiling = t'; rects = new_rects; adjacency = [] } in
       let joints_nr = Geom.subwall_simplicity g_nr in
       let joint_lines = List.map (fun (j, lo, hi) ->
         Printf.sprintf "T@(%.2f,%.2f) s=(%d,%d) b=%d %s%s"
           j.Geom.jx j.jy (fst j.stem_tiles) (snd j.stem_tiles)
           j.bar_tile
           (if lo then "L" else "") (if hi then "H" else "")
       ) joints_nr in
       add (render_rects_tiling ~x:nx ~y:by
         ~lines:("post-flip rects" :: joint_lines) new_rects);
       (* 4. Reverse T-flips from new_rects *)
       let rev_flips = Geom.enumerate_t_flips_from_rects new_rects in
       let shown = ref 0 in
       List.iter (fun (rf, rt) ->
         if !shown < max_rev then begin
           let arrow_x3 = nx +. tile_w +. arrow_w /. 2.
             +. float_of_int !shown *. (tile_w +. arrow_w) in
           let inv = Tiling.to_string rt = tiling_str in
           add (render_arrow ~x:arrow_x3 ~y:(by +. tile_h /. 2.)
             ~label:(Tiling.flip_to_string rf));
           let rx = next_x () in
           add (render_tiling ~x:rx ~y:by
             ~lines:[Tiling.to_string rt;
                     if inv then "INVERSE OK" else "not inverse"] rt);
           incr shown
         end
       ) rev_flips;
       if rev_flips = [] then begin
         let rx = next_x () in
         addf "<text x=\"%g\" y=\"%g\" font-size=\"11\" fill=\"red\" \
               font-family=\"monospace\">no reverse T-flips</text>\n"
           (rx +. 4.) (by +. tile_h /. 2.)
       end)
  ) cases;
  add "</svg>\n";
  let path = Printf.sprintf "%s/tflip_failures.svg" output_dir in
  write_file path (Buffer.contents buf);
  Printf.printf "Wrote %s (%d cases)\n" path n_cases
