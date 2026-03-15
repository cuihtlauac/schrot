type dir = Left | Right | Up | Down

type t =
  | Split of int * dir
  | Close of int
  | Move of int * dir

type side = L | R
type step = { split: [`H | `V]; side: side }

let find_path n term =
  let rec go = function
    | Term.Leaf m -> if m = n then Some [] else None
    | Term.H (a, b) ->
      (match go a with
       | Some path -> Some ({ split = `H; side = L } :: path)
       | None ->
         match go b with
         | Some path -> Some ({ split = `H; side = R } :: path)
         | None -> None)
    | Term.V (a, b) ->
      (match go a with
       | Some path -> Some ({ split = `V; side = L } :: path)
       | None ->
         match go b with
         | Some path -> Some ({ split = `V; side = R } :: path)
         | None -> None)
  in
  match go term with
  | Some path -> path
  | None -> failwith (Printf.sprintf "find_path: leaf %d not found" n)


(* Find the geometric neighbor of tile n in direction dir.
   Must overlap in the perpendicular axis and be adjacent (or nearest)
   in the movement axis. *)
let geometric_neighbor n dir term =
  let rects = Geometry.interpret term in
  match List.assoc_opt n rects with
  | None -> None
  | Some rn ->
    let eps = 1e-9 in
    let candidates = List.filter_map (fun (m, rm) ->
      if m = n then None
      else
        let open Geometry in
        let perp_overlap, dist = match dir with
          | Down ->
            let ox = min (rn.x +. rn.w) (rm.x +. rm.w) -. max rn.x rm.x in
            (ox, rm.y -. (rn.y +. rn.h))
          | Up ->
            let ox = min (rn.x +. rn.w) (rm.x +. rm.w) -. max rn.x rm.x in
            (ox, rn.y -. (rm.y +. rm.h))
          | Right ->
            let oy = min (rn.y +. rn.h) (rm.y +. rm.h) -. max rn.y rm.y in
            (oy, rm.x -. (rn.x +. rn.w))
          | Left ->
            let oy = min (rn.y +. rn.h) (rm.y +. rm.h) -. max rn.y rm.y in
            (oy, rn.x -. (rm.x +. rm.w))
        in
        if perp_overlap > eps && dist > -.eps then
          Some (m, perp_overlap, dist)
        else None
    ) rects in
    (* Nearest: smallest distance, then most overlap *)
    let pick_best = List.fold_left (fun best (m, o, d) ->
      match best with
      | None -> Some (m, o, d)
      | Some (_, bo, bd) ->
        if d < bd -. eps then Some (m, o, d)
        else if d < bd +. eps && o > bo +. eps then Some (m, o, d)
        else best
    ) None in
    match pick_best candidates with
    | Some (m, _, _) -> Some m
    | None -> None

(* Navigate the tree following path steps *)
let rec navigate term = function
  | [] -> term
  | step :: rest ->
    match term with
    | Term.H (a, b) | Term.V (a, b) ->
      navigate (if step.side = L then a else b) rest
    | Term.Leaf _ -> assert false

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
        (* Ancestor: exchange with nearest geometric neighbor *)
        (match geometric_neighbor n dir term with
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
