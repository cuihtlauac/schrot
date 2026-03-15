let parse_rule line =
  let line = String.trim line in
  let parse_int s =
    match int_of_string_opt (String.trim s) with
    | Some n -> n
    | None ->
      Printf.eprintf "Parse error: expected integer in '%s'\n" s;
      exit 1
  in
  let parse_arg prefix =
    let plen = String.length prefix in
    let rest = String.sub line plen (String.length line - plen) in
    let rest = String.trim rest in
    if String.length rest < 2 || rest.[0] <> '('
       || rest.[String.length rest - 1] <> ')' then begin
      Printf.eprintf "Parse error: expected (...) after '%s'\n" prefix;
      exit 1
    end;
    parse_int (String.sub rest 1 (String.length rest - 2))
  in
  if String.length line >= 8 && String.sub line 0 8 = "split_h(" then
    Rewrite.Split_h (parse_arg "split_h")
  else if String.length line >= 8 && String.sub line 0 8 = "split_v(" then
    Rewrite.Split_v (parse_arg "split_v")
  else if String.length line >= 10 && String.sub line 0 10 = "close_h_l(" then
    Rewrite.Close_h_l (parse_arg "close_h_l")
  else if String.length line >= 10 && String.sub line 0 10 = "close_h_r(" then
    Rewrite.Close_h_r (parse_arg "close_h_r")
  else if String.length line >= 10 && String.sub line 0 10 = "close_v_l(" then
    Rewrite.Close_v_l (parse_arg "close_v_l")
  else if String.length line >= 10 && String.sub line 0 10 = "close_v_r(" then
    Rewrite.Close_v_r (parse_arg "close_v_r")
  else if String.length line >= 5 && String.sub line 0 5 = "swap(" then
    Rewrite.Swap (parse_arg "swap")
  else if String.length line >= 8 && String.sub line 0 8 = "promote(" then
    Rewrite.Promote (parse_arg "promote")
  else if String.length line >= 7 && String.sub line 0 7 = "demote(" then
    Rewrite.Demote (parse_arg "demote")
  else if String.length line >= 7 && String.sub line 0 7 = "rotate(" then
    Rewrite.Rotate (parse_arg "rotate")
  else if String.length line >= 10 && String.sub line 0 10 = "transpose(" then
    Rewrite.Transpose (parse_arg "transpose")
  else begin
    Printf.eprintf "Parse error: unknown rule '%s'\n" line;
    exit 1
  end

let parse input =
  let lines = String.split_on_char '\n' input in
  let lines = List.filter (fun s -> String.trim s <> "") lines in
  List.map parse_rule lines
