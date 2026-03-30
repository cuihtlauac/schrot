(* Tiling geometry on the unit square.
   Computes tile rectangles via iterative repulsion and adjacency from them. *)

type rect = { x: float; y: float; w: float; h: float }

type t = {
  tiling: Tiling.t;
  rects: (int * rect) list;
  adjacency: (int * int * int) list;  (* (a, b, lca_depth) with a < b *)
}

(* Compute tile rectangles on [0,1)² using repulsion-resolved splits *)
let compute_rects tiling =
  let st = Tiling.resolve_splits tiling in
  let raw = Tiling.rects_of_split_tree (Tiling.is_h tiling) st in
  List.map (fun (n, (r : Tiling.rect)) ->
    (n, { x = r.rx; y = r.ry; w = r.rw; h = r.rh })
  ) raw

(* Compute adjacency from tile rects: shared boundary segment (not just a point) *)
let compute_adjacency tiling rects =
  let eps = 1e-9 in
  let edges = ref [] in
  List.iter (fun (a, ar) ->
    List.iter (fun (b, br) ->
      if a < b then begin
        let shared_v =
          (abs_float ((ar.x +. ar.w) -. br.x) < eps ||
           abs_float ((br.x +. br.w) -. ar.x) < eps)
          && (min (ar.y +. ar.h) (br.y +. br.h) -. max ar.y br.y > eps)
        in
        let shared_h =
          (abs_float ((ar.y +. ar.h) -. br.y) < eps ||
           abs_float ((br.y +. br.h) -. ar.y) < eps)
          && (min (ar.x +. ar.w) (br.x +. br.w) -. max ar.x br.x > eps)
        in
        if shared_v || shared_h then
          edges := (a, b, Tiling.lca_depth a b tiling) :: !edges
      end
    ) rects
  ) rects;
  !edges

let of_tiling tiling =
  let rects = compute_rects tiling in
  let adjacency = compute_adjacency tiling rects in
  { tiling; rects; adjacency }

(* --- Queries --- *)

let rect_of g n =
  List.assoc n g.rects

let center_of g n =
  let r = rect_of g n in
  (r.x +. r.w /. 2., r.y +. r.h /. 2.)

let edges g = g.adjacency

let neighbors g n =
  List.filter_map (fun (a, b, d) ->
    if a = n then Some (b, d)
    else if b = n then Some (a, d)
    else None
  ) g.adjacency

let degree g n = List.length (neighbors g n)

(* --- Graph analysis --- *)

(* Adjacency edges without depth *)
let edge_pairs g =
  List.map (fun (a, b, _) -> (a, b)) g.adjacency

(* Graph fingerprint: sorted multiset of (degree, sorted_neighbor_degrees) *)
let fingerprint g =
  let n = Tiling.size g.tiling in
  if n <= 1 then []
  else
    let adj = Array.make n [] in
    List.iter (fun (a, b, _) ->
      adj.(a) <- b :: adj.(a);
      adj.(b) <- a :: adj.(b)
    ) g.adjacency;
    let deg = Array.map List.length adj in
    let fp = Array.init n (fun v ->
      let nd = List.map (fun u -> deg.(u)) adj.(v) |> List.sort compare in
      (deg.(v), nd)
    ) in
    Array.to_list fp |> List.sort compare

(* Graph isomorphism via backtracking *)
let graphs_isomorphic n edges1 edges2 =
  if n <= 1 then true
  else
    let adj1 = Array.make n [] and adj2 = Array.make n [] in
    List.iter (fun (a, b) -> adj1.(a) <- b :: adj1.(a); adj1.(b) <- a :: adj1.(b)) edges1;
    List.iter (fun (a, b) -> adj2.(a) <- b :: adj2.(a); adj2.(b) <- a :: adj2.(b)) edges2;
    let deg1 = Array.map List.length adj1 in
    let deg2 = Array.map List.length adj2 in
    let perm = Array.make n (-1) in
    let used = Array.make n false in
    let rec go k =
      if k = n then true
      else
        let d = deg1.(k) in
        let found = ref false in
        for j = 0 to n - 1 do
          if not !found && not used.(j) && deg2.(j) = d then begin
            let consistent = List.for_all (fun nb ->
              if perm.(nb) >= 0 then List.mem perm.(nb) adj2.(j)
              else true
            ) adj1.(k) in
            if consistent then begin
              perm.(k) <- j;
              used.(j) <- true;
              if go (k + 1) then found := true
              else begin perm.(k) <- -1; used.(j) <- false end
            end
          end
        done;
        !found
    in
    go 0
