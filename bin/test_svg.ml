open Nachum

type test_case = {
  label: string;
  input: Term.t;
  command: Command.t;
}

(* Open/Close cases: split and close operations *)
let open_close_tests = [
  (* Split cases *)
  { label = "split right";
    input = Term.Leaf 0;
    command = Split (0, Right) };
  { label = "split left";
    input = Term.Leaf 0;
    command = Split (0, Left) };
  { label = "split down";
    input = Term.Leaf 0;
    command = Split (0, Down) };
  { label = "split up";
    input = Term.Leaf 0;
    command = Split (0, Up) };

  (* Close cases *)
  { label = "close left child of H";
    input = Term.H (Leaf 0, Leaf 1);
    command = Close 0 };
  { label = "close right child of H";
    input = Term.H (Leaf 0, Leaf 1);
    command = Close 1 };
  { label = "close left child of V";
    input = Term.V (Leaf 0, Leaf 1);
    command = Close 0 };
  { label = "close right child of V";
    input = Term.V (Leaf 0, Leaf 1);
    command = Close 1 };
]

(* Move cases: 2-tile swaps + 3-tile representative moves.
   16 complex representatives cover all 96 cases by symmetry:
   - H/V rotation: h(_v) <-> v(_h), h(v_) <-> v(h_), etc.
   - Outer child swap: h(_v) <-> h(v_), etc.
   - Inner child swap: tile 1 <-> tile 2 *)

let move_tests =
  let simple = [
    { label = "swap in V";
      input = Term.V (Leaf 0, Leaf 1);
      command = Move (0, Right) };
    { label = "swap in V";
      input = Term.V (Leaf 0, Leaf 1);
      command = Move (1, Left) };
    { label = "swap in H";
      input = Term.H (Leaf 0, Leaf 1);
      command = Move (0, Down) };
    { label = "swap in H";
      input = Term.H (Leaf 0, Leaf 1);
      command = Move (1, Up) };
  ] in
  (* Orbit 1 representative: h(0, v(1, 2)) — mixed inner/outer *)
  let orbit1 = Term.H (Leaf 0, V (Leaf 1, Leaf 2)) in
  (* Orbit 2 representative: h(0, h(1, 2)) — same inner/outer *)
  let orbit2 = Term.H (Leaf 0, H (Leaf 1, Leaf 2)) in
  let dirs = Command.[Left; Right; Up; Down] in
  let tiles = [0; 1] in
  let cases input =
    List.concat_map (fun n ->
      List.map (fun dir ->
        { label = ""; input; command = Move (n, dir) }
      ) dirs
    ) tiles
  in
  simple @ cases orbit1 @ cases orbit2

let rect_w = 200.
let rect_h = 140.
let arrow_gap = 50.
let row_text_h = 80.
let margin = 40.
let num_w = 24.
let header_h = 30.

let dark_blue = "#1a3a6b"
let light_blue = "#a8d0e6"
let no_color = "white"

(* Check if leaf n has the same path in both terms *)
let same_position n t1 t2 =
  try Command.find_path n t1 = Command.find_path n t2
  with _ -> false

(* Target leaf of a command *)
let target_of = function
  | Command.Move (n, _) | Command.Split (n, _) | Command.Close n -> n

let render_test_row ~policy ~index ~y test =
  let buf = Buffer.create 4096 in
  let add = Buffer.add_string buf in
  let addf fmt = Printf.ksprintf add fmt in
  let target = target_of test.command in
  let color_in n =
    if n = target then dark_blue else no_color
  in
  let rules = Policy.compile policy test.command test.input in
  let output =
    List.fold_left (fun t r -> Rewrite.apply r t) test.input rules
  in
  let color_out n =
    if n = target then dark_blue
    else if not (same_position n test.input output) then light_blue
    else no_color
  in
  (* Row number *)
  addf "<text x=\"%g\" y=\"%g\" font-size=\"14\" font-weight=\"bold\" \
        font-family=\"sans-serif\" fill=\"#999\">%d.</text>\n"
    (margin -. 2.) (y +. rect_h /. 2.) index;
  (* Input rectangle *)
  let x_in = margin +. num_w in
  addf "<rect x=\"%g\" y=\"%g\" width=\"%g\" height=\"%g\" \
        fill=\"none\" stroke=\"#ccc\" stroke-width=\"1\"/>\n"
    x_in y rect_w rect_h;
  add (Svg.render_group ~x:x_in ~y ~width:rect_w ~height:rect_h
         ~margin:8. ~color_of:color_in test.input);
  (* Arrow *)
  let x_base = margin +. num_w +. rect_w in
  let arrow_x1 = x_base +. 10. in
  let arrow_x2 = x_base +. arrow_gap -. 10. in
  let arrow_y = y +. rect_h /. 2. in
  addf "<line x1=\"%g\" y1=\"%g\" x2=\"%g\" y2=\"%g\" \
        stroke=\"black\" stroke-width=\"1.5\" marker-end=\"url(#arrow)\"/>\n"
    arrow_x1 arrow_y arrow_x2 arrow_y;
  (* Output rectangle *)
  let x_out = x_base +. arrow_gap in
  addf "<rect x=\"%g\" y=\"%g\" width=\"%g\" height=\"%g\" \
        fill=\"none\" stroke=\"#ccc\" stroke-width=\"1\"/>\n"
    x_out y rect_w rect_h;
  add (Svg.render_group ~x:x_out ~y ~width:rect_w ~height:rect_h
         ~margin:8. ~color_of:color_out output);
  (* Text: input tree + output tree *)
  let text_y = y +. rect_h +. 18. in
  addf "<text x=\"%g\" y=\"%g\" font-size=\"11\" font-family=\"monospace\">%s</text>\n"
    x_in text_y (Term.to_string test.input);
  addf "<text x=\"%g\" y=\"%g\" font-size=\"11\" font-family=\"monospace\">%s</text>\n"
    x_out text_y (Term.to_string output);
  (* Text: command + rules *)
  let cmd_y = text_y +. 16. in
  addf "<text x=\"%g\" y=\"%g\" font-size=\"11\" font-family=\"monospace\" fill=\"#555\">%s</text>\n"
    x_in cmd_y (Command.to_string test.command);
  addf "<text x=\"%g\" y=\"%g\" font-size=\"10\" font-family=\"monospace\" fill=\"#888\">%s</text>\n"
    x_out cmd_y (Command.rule_list_to_string rules);
  (* Label *)
  (if test.label <> "" then
    let label_y = text_y +. 32. in
    let center_x = margin +. num_w +. (rect_w +. arrow_gap +. rect_w) /. 2. in
    addf "<text x=\"%g\" y=\"%g\" text-anchor=\"middle\" \
          font-size=\"12\" font-family=\"sans-serif\" fill=\"#888\">%s</text>\n"
      center_x label_y test.label);
  Buffer.contents buf

let row_w = num_w +. rect_w +. arrow_gap +. rect_w

let write_svg filename ~policy tests =
  let row_h = rect_h +. row_text_h in
  let total_h = margin +. header_h +. (float_of_int (List.length tests)) *. row_h +. margin in
  let total_w = margin +. row_w +. margin in
  let buf = Buffer.create 16384 in
  let add = Buffer.add_string buf in
  let addf fmt = Printf.ksprintf add fmt in
  addf "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%g\" height=\"%g\">\n"
    total_w total_h;
  addf "<rect width=\"%g\" height=\"%g\" fill=\"white\"/>\n" total_w total_h;
  add "<defs>\n\
       <marker id=\"arrow\" markerWidth=\"10\" markerHeight=\"7\" \
       refX=\"10\" refY=\"3.5\" orient=\"auto\">\n\
       <polygon points=\"0 0, 10 3.5, 0 7\" fill=\"black\"/>\n\
       </marker>\n</defs>\n";
  (* Column headers *)
  let x_in = margin +. num_w in
  addf "<text x=\"%g\" y=\"%g\" font-size=\"14\" font-weight=\"bold\" \
        font-family=\"sans-serif\" fill=\"#333\">input</text>\n"
    x_in (margin +. 14.);
  let x_out = margin +. num_w +. rect_w +. arrow_gap in
  addf "<text x=\"%g\" y=\"%g\" font-size=\"14\" font-weight=\"bold\" \
        font-family=\"sans-serif\" fill=\"#333\">%s</text>\n"
    x_out (margin +. 14.) (Policy.name policy);
  List.iteri (fun i test ->
    let y = margin +. header_h +. (float_of_int i) *. row_h in
    add (render_test_row ~policy ~index:i ~y test)
  ) tests;
  add "</svg>\n";
  let oc = open_out filename in
  output_string oc (Buffer.contents buf);
  close_out oc;
  Printf.printf "Wrote %s (%d tests)\n" filename (List.length tests)

let () =
  let output = ref "" in
  let policy_names = ref "" in
  let args = Array.to_list Sys.argv |> List.tl in
  let rec parse_args = function
    | "--output" :: v :: rest -> output := v; parse_args rest
    | "--policies" :: v :: rest -> policy_names := v; parse_args rest
    | [] -> ()
    | arg :: _ ->
      Printf.eprintf "Unknown argument: %s\n" arg;
      exit 1
  in
  parse_args args;
  if !output = "" then begin
    Printf.eprintf "Usage: test_svg --output DIR [--policies name1,name2]\n\
                    Generates DIR/{policy}_open_close.svg and DIR/{policy}_move.svg\n";
    exit 1
  end;
  let policies =
    if !policy_names = "" then Policy.all
    else
      String.split_on_char ',' !policy_names
      |> List.map (fun s -> Policy.find (String.trim s))
  in
  List.iter (fun policy ->
    let pname = Policy.name policy in
    write_svg (Filename.concat !output (pname ^ "_open_close.svg"))
      ~policy open_close_tests;
    write_svg (Filename.concat !output (pname ^ "_move.svg"))
      ~policy move_tests
  ) policies
