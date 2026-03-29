type 'a t = Tile of 'a | Frame of 'a t List2.t

type 'a tiling = bool * 'a t

let rec fold f g = function
  | Tile x -> f x
  | Frame children -> g (List2.map (fold f g) children)

let rec unfold coalg seed =
  match coalg seed with
  | Either.Left a -> Tile a
  | Either.Right seeds -> Frame (List2.map (unfold coalg) seeds)

let map f = fold (fun x -> Tile (f x)) (fun ch -> Frame ch)

let rec equal eq t1 t2 =
  match t1, t2 with
  | Tile a, Tile b -> eq a b
  | Frame ch1, Frame ch2 -> List2.equal (equal eq) ch1 ch2
  | _ -> false

let size t = fold (fun _ -> 1) (fun ch -> List2.fold_left ( + ) 0 ch) t

let height t = fold (fun _ -> 0) (fun ch -> 1 + List2.fold_left max 0 ch) t

let leaves t =
  fold (fun x -> [x]) (fun ch -> List2.fold_left (fun acc l -> acc @ l) [] ch) t

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
                | a :: b :: rest -> Frame (List2.Cons2 (a, b, rest))
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
