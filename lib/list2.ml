type 'a t = Cons2 of 'a * 'a * 'a list

let to_list (Cons2 (a, b, rest)) = a :: b :: rest

let of_list d0 d1 = function
  | a :: b :: rest -> Cons2 (a, b, rest)
  | [a] -> Cons2 (a, d1, [])
  | [] -> Cons2 (d0, d1, [])

let of_list_opt = function
  | a :: b :: rest -> Some (Cons2 (a, b, rest))
  | _ -> None

let length (Cons2 (_, _, rest)) = 2 + List.length rest

let compare_lengths (Cons2 (_, _, r1)) (Cons2 (_, _, r2)) =
  List.compare_lengths r1 r2

let compare_length_with (Cons2 (_, _, rest)) n =
  List.compare_length_with rest (n - 2)

let cons x (Cons2 (a, b, rest)) = Cons2 (x, a, b :: rest)

let hd (Cons2 (a, _, _)) = a

let nth (Cons2 (a, b, rest)) = function
  | 0 -> a
  | 1 -> b
  | n -> List.nth rest (n - 2)

let nth_opt (Cons2 (a, b, rest)) = function
  | 0 -> Some a
  | 1 -> Some b
  | n -> List.nth_opt rest (n - 2)

let rev (Cons2 (a, b, rest)) =
  match List.rev (b :: rest) with
  | z :: y :: mid -> Cons2 (z, y, mid @ [a])
  | [z] -> Cons2 (z, a, [])
  | [] -> assert false

let append (Cons2 (a, b, rest)) rest2 = Cons2 (a, b, rest @ rest2)

let rev_append (Cons2 (a, b, rest)) l2 =
  let rev_tail = List.rev_append rest (b :: a :: l2) in
  match rev_tail with
  | z :: y :: mid -> Cons2 (z, y, mid)
  | _ -> assert false

let init n f =
  if n < 2 then invalid_arg "List2.init"
  else Cons2 (f 0, f 1, List.init (n - 2) (fun i -> f (i + 2)))

let equal eq (Cons2 (a1, b1, r1)) (Cons2 (a2, b2, r2)) =
  eq a1 a2 && eq b1 b2 && List.equal eq r1 r2

let compare cmp (Cons2 (a1, b1, r1)) (Cons2 (a2, b2, r2)) =
  let c = cmp a1 a2 in
  if c <> 0 then c
  else let c = cmp b1 b2 in
  if c <> 0 then c
  else List.compare cmp r1 r2

let iter f (Cons2 (a, b, rest)) = f a; f b; List.iter f rest

let iteri f (Cons2 (a, b, rest)) =
  f 0 a; f 1 b; List.iteri (fun i x -> f (i + 2) x) rest

let map f (Cons2 (a, b, rest)) = Cons2 (f a, f b, List.map f rest)

let mapi f (Cons2 (a, b, rest)) =
  Cons2 (f 0 a, f 1 b, List.mapi (fun i x -> f (i + 2) x) rest)

let rev_map f (Cons2 (a, b, rest)) =
  let rev = List.rev_map f (b :: rest) in
  match rev with
  | z :: y :: mid -> Cons2 (z, y, mid @ [f a])
  | [z] -> Cons2 (z, f a, [])
  | [] -> assert false

let fold_left f acc (Cons2 (a, b, rest)) =
  List.fold_left f (f (f acc a) b) rest

let fold_right f (Cons2 (a, b, rest)) acc =
  f a (f b (List.fold_right f rest acc))

let fold_left_map f acc (Cons2 (a, b, rest)) =
  let acc, a' = f acc a in
  let acc, b' = f acc b in
  let acc, rest' = List.fold_left_map f acc rest in
  (acc, Cons2 (a', b', rest'))

let iter2 f (Cons2 (a1, b1, r1)) (Cons2 (a2, b2, r2)) =
  f a1 a2; f b1 b2; List.iter2 f r1 r2

let map2 f (Cons2 (a1, b1, r1)) (Cons2 (a2, b2, r2)) =
  Cons2 (f a1 a2, f b1 b2, List.map2 f r1 r2)

let rev_map2 f (Cons2 (a1, b1, r1)) (Cons2 (a2, b2, r2)) =
  let rev = List.rev_map2 f (b1 :: r1) (b2 :: r2) in
  match rev with
  | z :: y :: mid -> Cons2 (z, y, mid @ [f a1 a2])
  | [z] -> Cons2 (z, f a1 a2, [])
  | [] -> assert false

let fold_left2 f acc (Cons2 (a1, b1, r1)) (Cons2 (a2, b2, r2)) =
  List.fold_left2 f (f (f acc a1 a2) b1 b2) r1 r2

let fold_right2 f (Cons2 (a1, b1, r1)) (Cons2 (a2, b2, r2)) acc =
  f a1 a2 (f b1 b2 (List.fold_right2 f r1 r2 acc))

let for_all f (Cons2 (a, b, rest)) = f a && f b && List.for_all f rest

let exists f (Cons2 (a, b, rest)) = f a || f b || List.exists f rest

let for_all2 f (Cons2 (a1, b1, r1)) (Cons2 (a2, b2, r2)) =
  f a1 a2 && f b1 b2 && List.for_all2 f r1 r2

let exists2 f (Cons2 (a1, b1, r1)) (Cons2 (a2, b2, r2)) =
  f a1 a2 || f b1 b2 || List.exists2 f r1 r2

let mem x (Cons2 (a, b, rest)) = x = a || x = b || List.mem x rest

let memq x (Cons2 (a, b, rest)) = x == a || x == b || List.memq x rest

let find f (Cons2 (a, b, rest)) =
  if f a then a
  else if f b then b
  else List.find f rest

let find_opt f (Cons2 (a, b, rest)) =
  if f a then Some a
  else if f b then Some b
  else List.find_opt f rest

let find_index f (Cons2 (a, b, rest)) =
  if f a then Some 0
  else if f b then Some 1
  else match List.find_index f rest with
  | Some i -> Some (i + 2)
  | None -> None

let find_map f (Cons2 (a, b, rest)) =
  match f a with
  | Some _ as r -> r
  | None -> match f b with
    | Some _ as r -> r
    | None -> List.find_map f rest

let find_mapi f (Cons2 (a, b, rest)) =
  match f 0 a with
  | Some _ as r -> r
  | None -> match f 1 b with
    | Some _ as r -> r
    | None -> List.find_mapi (fun i x -> f (i + 2) x) rest

let split (Cons2 ((a1, b1), (a2, b2), rest)) =
  let la, lb = List.split rest in
  (Cons2 (a1, a2, la), Cons2 (b1, b2, lb))

let combine (Cons2 (a1, b1, r1)) (Cons2 (a2, b2, r2)) =
  Cons2 ((a1, a2), (b1, b2), List.combine r1 r2)

let unsafe_of_list = function
  | a :: b :: rest -> Cons2 (a, b, rest)
  | _ -> assert false

let sort cmp l = to_list l |> List.sort cmp |> unsafe_of_list

let stable_sort cmp l = to_list l |> List.stable_sort cmp |> unsafe_of_list

let fast_sort cmp l = to_list l |> List.fast_sort cmp |> unsafe_of_list

let merge cmp l1 l2 =
  List.merge cmp (to_list l1) (to_list l2) |> unsafe_of_list

let to_seq (Cons2 (a, b, rest)) =
  fun () -> Seq.Cons (a, fun () -> Seq.Cons (b, List.to_seq rest))

let of_seq d0 d1 s =
  match s () with
  | Seq.Nil -> Cons2 (d0, d1, [])
  | Seq.Cons (a, s) ->
    match s () with
    | Seq.Nil -> Cons2 (a, d1, [])
    | Seq.Cons (b, s) -> Cons2 (a, b, List.of_seq s)
