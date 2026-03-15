type dir = Left | Right | Up | Down

type t =
  | Split of int * dir
  | Close of int
  | Move of int * dir

type side = L | R
type step = { split: [`H | `V]; side: side; ratio: Q.t }

let find_path n term =
  let rec go = function
    | Term.Leaf m -> if m = n then Some [] else None
    | Term.H (a, b, r) ->
      (match go a with
       | Some path -> Some ({ split = `H; side = L; ratio = r } :: path)
       | None ->
         match go b with
         | Some path -> Some ({ split = `H; side = R; ratio = r } :: path)
         | None -> None)
    | Term.V (a, b, r) ->
      (match go a with
       | Some path -> Some ({ split = `V; side = L; ratio = r } :: path)
       | None ->
         match go b with
         | Some path -> Some ({ split = `V; side = R; ratio = r } :: path)
         | None -> None)
  in
  match go term with
  | Some path -> path
  | None -> failwith (Printf.sprintf "find_path: leaf %d not found" n)

let root_sibling n = function
  | Term.H (Term.Leaf m, b, _) when m = n -> b
  | Term.H (a, Term.Leaf m, _) when m = n -> a
  | Term.V (Term.Leaf m, b, _) when m = n -> b
  | Term.V (a, Term.Leaf m, _) when m = n -> a
  | _ -> assert false

let is_split_type split = function
  | Term.H _ -> split = `H
  | Term.V _ -> split = `V
  | Term.Leaf _ -> false

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
      let last = List.nth path (len - 1) in
      if last.split = aligned_split then
        if last.side = favorable_side then
          if len = 1 then
            let sib = root_sibling n term in
            if is_split_type aligned_split sib then
              [Rewrite.Swap n; Rewrite.Demote n; Rewrite.Swap n]
            else
              [Rewrite.Swap n]
          else
            [Rewrite.Swap n]
        else
          if len >= 2 then
            let outer = List.nth path (len - 2) in
            if outer.split = aligned_split
               && outer.side = favorable_side then
              [Rewrite.Promote n; Rewrite.Promote n]
            else []
          else []
      else
        if len >= 2 then
          let outer = List.nth path (len - 2) in
          if outer.split = aligned_split
             && outer.side = favorable_side then
            [Rewrite.Promote n]
          else []
        else []

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
  ) rules) ^ "]"
