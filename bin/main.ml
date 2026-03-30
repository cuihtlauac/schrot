(* TODO: Bring up to Schroder tilings *)
let () =
  let width = ref 800. in
  let height = ref 600. in
  let output = ref "" in
  let args = Array.to_list Sys.argv |> List.tl in
  let rec parse_args = function
    | "--width" :: v :: rest ->
      width := float_of_string v;
      parse_args rest
    | "--height" :: v :: rest ->
      height := float_of_string v;
      parse_args rest
    | "--output" :: v :: rest ->
      output := v;
      parse_args rest
    | [] -> ()
    | arg :: _ ->
      Printf.eprintf "Unknown argument: %s\n" arg;
      exit 1
  in
  parse_args args;
  if !output = "" then begin
    Printf.eprintf "Usage: nachum --output FILE [--width W] [--height H]\n";
    exit 1
  end;
  let write_svg term =
    let svg = Nachum.Svg.render ~width:!width ~height:!height term in
    let oc = open_out !output in
    output_string oc svg;
    close_out oc
  in
  let term = ref (Nachum.Term.Leaf 0) in
  write_svg !term;
  try while true do
    let line = input_line stdin in
    if String.trim line <> "" then begin
      let rule = Nachum.Parser.parse_rule line in
      term := Nachum.Rewrite.apply rule !term;
      print_endline (Nachum.Term.to_string !term);
      flush stdout;
      write_svg !term
    end
  done with End_of_file -> ()
