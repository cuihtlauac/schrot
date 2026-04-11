type ('a, 'f) t = Tile of 'a | Frame of ('f * ('a, 'f) t) List2.t

type ('a, 'f) tiling = bool * ('a, 'f) t

let rec fold f g = function
  | Tile x -> f x
  | Frame children -> g (List2.map (fun (w, c) -> (w, fold f g c)) children)

let rec unfold coalg seed =
  match coalg seed with
  | Either.Left a -> Tile a
  | Either.Right seeds -> Frame (List2.map (fun (w, s) -> (w, unfold coalg s)) seeds)

let map f = fold (fun x -> Tile (f x)) (fun ch -> Frame ch)

let map_weights f =
  let rec go = function
    | Tile x -> Tile x
    | Frame children -> Frame (List2.map (fun (w, c) -> (f w, go c)) children)
  in
  go

let rec equal eq weq t1 t2 =
  match t1, t2 with
  | Tile a, Tile b -> eq a b
  | Frame ch1, Frame ch2 ->
    List2.equal (fun (w1, c1) (w2, c2) -> weq w1 w2 && equal eq weq c1 c2) ch1 ch2
  | _ -> false

let size t = fold (fun _ -> 1) (fun ch -> List2.fold_left (fun acc (_, n) -> acc + n) 0 ch) t

let height t = fold (fun _ -> 0) (fun ch -> 1 + List2.fold_left (fun acc (_, h) -> max acc h) 0 ch) t

let leaves t =
  fold (fun x -> [x]) (fun ch -> List2.fold_left (fun acc (_, l) -> acc @ l) [] ch) t

let unit_frame children = Frame (List2.map (fun c -> ((), c)) children)

let enum =
  let tbl = Hashtbl.create 16 in
  let rec compositions n k =
    if k = 1 then [[n]]
    else if n < k then []
    else
      List.concat_map (fun first ->
        List.map (fun rest -> first :: rest) (compositions (n - first) (k - 1))
      ) (List.init (n - k + 1) (fun i -> i + 1))
  in
  let rec cartesian = function
    | [] -> [[]]
    | xs :: rest ->
      let rest_products = cartesian rest in
      List.concat_map (fun x ->
        List.map (fun rp -> x :: rp) rest_products
      ) xs
  in
  let rec enum_t n =
    match Hashtbl.find_opt tbl n with
    | Some r -> r
    | None ->
      let r =
        if n <= 0 then []
        else if n = 1 then [Tile ()]
        else
          List.concat_map (fun k ->
            List.concat_map (fun comp ->
              let child_trees = List.map enum_t comp in
              let products = cartesian child_trees in
              List.map (fun children ->
                match children with
                | a :: b :: rest ->
                  unit_frame (List2.Cons2 (a, b, rest))
                | _ -> assert false
              ) products
            ) (compositions n k)
          ) (List.init (n - 1) (fun i -> i + 2))
      in
      Hashtbl.add tbl n r; r
  in
  fun n ->
    let trees = enum_t n in
    if n <= 1 then List.map (fun t -> (false, t)) trees
    else List.concat_map (fun t -> [(true, t); (false, t)]) trees
