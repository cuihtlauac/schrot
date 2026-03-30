(* TODO: Bring up to Schroder tilings *)
type dir = Path.dir = Left | Right | Up | Down

type t =
  | Split of int * dir
  | Close of int
  | Move of int * dir

type side = Path.side = L | R
type step = Path.step = { split: [`H | `V]; side: side }

let find_path = Path.find_path
let navigate = Path.navigate

let compile cmd term =
  match cmd with
  | Split (n, Right) -> [Rewrite.Split_v n]
  | Split (n, Left) -> [Rewrite.Split_v n; Rewrite.Swap n]
  | Split (n, Down) -> [Rewrite.Split_h n]
  | Split (n, Up) -> [Rewrite.Split_h n; Rewrite.Swap n]
  | Close n ->
    let path = find_path n term in
    let last = List.nth path (List.length path - 1) in
    (match last.split, last.side with
     | `H, L -> [Rewrite.Close_h_l n]
     | `H, R -> [Rewrite.Close_h_r n]
     | `V, L -> [Rewrite.Close_v_l n]
     | `V, R -> [Rewrite.Close_v_r n])
  | Move (n, dir) ->
    let path = find_path n term in
    let len = List.length path in
    if len = 0 then []
    else
      let aligned_split, favorable_side = match dir with
        | Left -> `V, R
        | Right -> `V, L
        | Up -> `H, R
        | Down -> `H, L
      in
      (* Scan from leaf toward root for first aligned+favorable ancestor *)
      let rec find_favorable i =
        if i < 0 then None
        else
          let step = List.nth path i in
          if step.split = aligned_split && step.side = favorable_side then
            Some i
          else
            find_favorable (i - 1)
      in
      match find_favorable (len - 1) with
      | None -> []
      | Some i when i = len - 1 ->
        (* Immediate parent: slide over neighbor *)
        [Rewrite.Slide n]
      | Some _ ->
        (* Ancestor: exchange with nearest symbolic neighbor *)
        let ts = Tabstop.extract term in
        let tab_dir = match dir with
          | Left -> Tabstop.Left | Right -> Tabstop.Right
          | Up -> Tabstop.Up | Down -> Tabstop.Down
        in
        (match Tabstop.neighbor ts n tab_dir term with
         | Some neighbor -> [Rewrite.Exchange (n, neighbor)]
         | None -> [])

let to_string = function
  | Split (n, Left) -> Printf.sprintf "split(%d, left)" n
  | Split (n, Right) -> Printf.sprintf "split(%d, right)" n
  | Split (n, Up) -> Printf.sprintf "split(%d, up)" n
  | Split (n, Down) -> Printf.sprintf "split(%d, down)" n
  | Close n -> Printf.sprintf "close(%d)" n
  | Move (n, Left) -> Printf.sprintf "move_left(%d)" n
  | Move (n, Right) -> Printf.sprintf "move_right(%d)" n
  | Move (n, Up) -> Printf.sprintf "move_up(%d)" n
  | Move (n, Down) -> Printf.sprintf "move_down(%d)" n

let rule_list_to_string rules =
  "[" ^ String.concat "; " (List.map (fun r ->
    match r with
    | Rewrite.Split_h n -> Printf.sprintf "split_h(%d)" n
    | Rewrite.Split_v n -> Printf.sprintf "split_v(%d)" n
    | Rewrite.Close_h_l n -> Printf.sprintf "close_h_l(%d)" n
    | Rewrite.Close_h_r n -> Printf.sprintf "close_h_r(%d)" n
    | Rewrite.Close_v_l n -> Printf.sprintf "close_v_l(%d)" n
    | Rewrite.Close_v_r n -> Printf.sprintf "close_v_r(%d)" n
    | Rewrite.Swap n -> Printf.sprintf "swap(%d)" n
    | Rewrite.Promote n -> Printf.sprintf "promote(%d)" n
    | Rewrite.Demote n -> Printf.sprintf "demote(%d)" n
    | Rewrite.Rotate n -> Printf.sprintf "rotate(%d)" n
    | Rewrite.Transpose n -> Printf.sprintf "transpose(%d)" n
    | Rewrite.Slide n -> Printf.sprintf "slide(%d)" n
    | Rewrite.Exchange (m, k) -> Printf.sprintf "exchange(%d, %d)" m k
  ) rules) ^ "]"
