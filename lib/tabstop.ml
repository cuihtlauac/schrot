(* TODO: Bring up to Schroder tilings *)
type tab = int

type bounds = {
  left: tab;
  right: tab;
  top: tab;
  bottom: tab;
}

type t = {
  tiles: (int * bounds) list;
  by_left:   (tab * int list) list;
  by_right:  (tab * int list) list;
  by_top:    (tab * int list) list;
  by_bottom: (tab * int list) list;
}

type dir = Left | Right | Up | Down

let extract term =
  let next_id = ref 0 in
  let fresh () = let id = !next_id in incr next_id; id in
  let x_left = fresh () in
  let x_right = fresh () in
  let y_top = fresh () in
  let y_bottom = fresh () in
  let tiles = ref [] in
  let rec go left right top bottom = function
    | Term.Leaf n ->
      tiles := (n, { left; right; top; bottom }) :: !tiles
    | Term.H (a, b) ->
      let mid = fresh () in
      go left right top mid a;
      go left right mid bottom b
    | Term.V (a, b) ->
      let mid = fresh () in
      go left mid top bottom a;
      go mid right top bottom b
  in
  go x_left x_right y_top y_bottom term;
  let tiles = List.rev !tiles in
  let group_by proj =
    let tbl = Hashtbl.create 16 in
    List.iter (fun (n, b) ->
      let k = proj b in
      let prev = try Hashtbl.find tbl k with Not_found -> [] in
      Hashtbl.replace tbl k (n :: prev)
    ) tiles;
    Hashtbl.fold (fun k vs acc -> (k, List.rev vs) :: acc) tbl []
  in
  { tiles;
    by_left   = group_by (fun b -> b.left);
    by_right  = group_by (fun b -> b.right);
    by_top    = group_by (fun b -> b.top);
    by_bottom = group_by (fun b -> b.bottom);
  }

let bounds_of ts n =
  List.assoc_opt n ts.tiles

(* Symbolic neighbor: use path-based dyadic intervals for perpendicular overlap.
   Resolves all cases that tabstop identity left ambiguous. *)
let neighbor ts n dir term =
  match bounds_of ts n with
  | None -> None
  | Some bn ->
    let candidates = match dir with
      | Right -> (try List.assoc bn.right ts.by_left with Not_found -> [])
      | Left  -> (try List.assoc bn.left  ts.by_right with Not_found -> [])
      | Down  -> (try List.assoc bn.bottom ts.by_top with Not_found -> [])
      | Up    -> (try List.assoc bn.top   ts.by_bottom with Not_found -> [])
    in
    match candidates with
    | [] -> None
    | [m] -> Some m
    | _ ->
      let perp_axis = match dir with
        | Left | Right -> `Y
        | Up | Down -> `X
      in
      let path_n = Path.find_path n term in
      let scored = List.filter_map (fun m ->
        let path_m = Path.find_path m term in
        let (num, den) = Path.perp_overlap perp_axis path_n path_m in
        if num > 0 then Some (m, num, den)
        else None
      ) candidates in
      match scored with
      | [] -> None
      | _ ->
        let (bm, _, _) = List.fold_left (fun (bm, bn, bd) (m, num, den) ->
          if num * bd > bn * den then (m, num, den)
          else if num * bd = bn * den && m < bm then (m, num, den)
          else (bm, bn, bd)
        ) (List.hd scored) (List.tl scored) in
        Some bm
