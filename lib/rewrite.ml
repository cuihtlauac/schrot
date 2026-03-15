type rule =
  | Split_h of int
  | Split_v of int
  | Close_h_l of int
  | Close_h_r of int
  | Close_v_l of int
  | Close_v_r of int
  | Swap of int
  | Promote of int
  | Demote of int
  | Rotate of int
  | Transpose of int

let max_leaf term =
  let rec go = function
    | Term.Leaf n -> n
    | Term.H (a, b) | Term.V (a, b) -> max (go a) (go b)
  in
  go term

let has_leaf n term =
  let rec go = function
    | Term.Leaf m -> m = n
    | Term.H (a, b) | Term.V (a, b) -> go a || go b
  in
  go term

let apply rule term =
  let fresh = max_leaf term + 1 in
  let rec go = function
    | Term.Leaf n ->
      (match rule with
       | Split_h m when m = n -> Term.H (Leaf n, Leaf fresh)
       | Split_v m when m = n -> Term.V (Leaf n, Leaf fresh)
       | _ -> Term.Leaf n)
    | Term.H (a, b) ->
      (match rule with
       | Close_h_l m when a = Leaf m -> b
       | Close_h_r m when b = Leaf m -> a
       | Swap m when a = Leaf m -> Term.H (b, Leaf m)
       | Swap m when b = Leaf m -> Term.H (Leaf m, a)
       | Transpose m when a = Leaf m -> Term.V (a, b)
       | Transpose m when b = Leaf m -> Term.V (a, b)
       | Promote m -> promote_in (Term.H (a, b)) m
       | Demote m -> demote_in (Term.H (a, b)) m
       | Rotate m -> rotate_in (Term.H (a, b)) m
       | Transpose m -> transpose_in (Term.H (a, b)) m
       | _ -> Term.H (go a, go b))
    | Term.V (a, b) ->
      (match rule with
       | Close_v_l m when a = Leaf m -> b
       | Close_v_r m when b = Leaf m -> a
       | Swap m when a = Leaf m -> Term.V (b, Leaf m)
       | Swap m when b = Leaf m -> Term.V (Leaf m, a)
       | Transpose m when a = Leaf m -> Term.H (a, b)
       | Transpose m when b = Leaf m -> Term.H (a, b)
       | Promote m -> promote_in (Term.V (a, b)) m
       | Demote m -> demote_in (Term.V (a, b)) m
       | Rotate m -> rotate_in (Term.V (a, b)) m
       | Transpose m -> transpose_in (Term.V (a, b)) m
       | _ -> Term.V (go a, go b))
  and promote_in node m =
    let mk, a, b = match node with
      | Term.H (a, b) -> (fun x y -> Term.H (x, y)), a, b
      | Term.V (a, b) -> (fun x y -> Term.V (x, y)), a, b
      | _ -> assert false
    in
    match a, b with
    (* op1(op2(Leaf n, B), C) → op1(Leaf n, op2(B, C)) *)
    | Term.H (Term.Leaf n, b2), _ when n = m ->
      mk (Leaf m) (Term.H (b2, b))
    | Term.V (Term.Leaf n, b2), _ when n = m ->
      mk (Leaf m) (Term.V (b2, b))
    (* op1(op2(B, Leaf n), C) → op1(op2(B, C), Leaf n) *)
    | Term.H (a2, Term.Leaf n), _ when n = m ->
      mk (Term.H (a2, b)) (Leaf m)
    | Term.V (a2, Term.Leaf n), _ when n = m ->
      mk (Term.V (a2, b)) (Leaf m)
    (* op1(A, op2(Leaf n, B)) → op1(op2(Leaf n, A), B) *)
    | _, Term.H (Term.Leaf n, b2) when n = m ->
      mk (Term.H (Leaf m, a)) b2
    | _, Term.V (Term.Leaf n, b2) when n = m ->
      mk (Term.V (Leaf m, a)) b2
    (* op1(A, op2(B, Leaf n)) → op1(B, op2(A, Leaf n)) *)
    | _, Term.H (a2, Term.Leaf n) when n = m ->
      mk a2 (Term.H (a, Leaf m))
    | _, Term.V (a2, Term.Leaf n) when n = m ->
      mk a2 (Term.V (a, Leaf m))
    (* Not at this level, recurse *)
    | _ ->
      if has_leaf m a then mk (go a) b
      else if has_leaf m b then mk a (go b)
      else node
  and demote_in node m =
    let mk, a, b = match node with
      | Term.H (a, b) -> (fun x y -> Term.H (x, y)), a, b
      | Term.V (a, b) -> (fun x y -> Term.V (x, y)), a, b
      | _ -> assert false
    in
    match a, b with
    (* n is direct left child, push into compound right sibling *)
    (* mk(Leaf n, op2(B, C)) → mk(op2(Leaf n, B), C) *)
    | Term.Leaf n, Term.H (b1, b2) when n = m ->
      mk (Term.H (Leaf m, b1)) b2
    | Term.Leaf n, Term.V (b1, b2) when n = m ->
      mk (Term.V (Leaf m, b1)) b2
    (* n is direct right child, push into compound left sibling *)
    (* mk(op2(A, B), Leaf n) → mk(A, op2(B, Leaf n)) *)
    | Term.H (a1, a2), Term.Leaf n when n = m ->
      mk a1 (Term.H (a2, Leaf m))
    | Term.V (a1, a2), Term.Leaf n when n = m ->
      mk a1 (Term.V (a2, Leaf m))
    (* n in left child's left: move inner split to right side *)
    (* mk(op2(Leaf n, X), B) → mk(X, op2(Leaf n, B)) *)
    | Term.H (Term.Leaf n, x), _ when n = m ->
      mk x (Term.H (Leaf m, b))
    | Term.V (Term.Leaf n, x), _ when n = m ->
      mk x (Term.V (Leaf m, b))
    (* n in left child's right: move inner split to right side *)
    (* mk(op2(X, Leaf n), B) → mk(X, op2(B, Leaf n)) *)
    | Term.H (x, Term.Leaf n), _ when n = m ->
      mk x (Term.H (b, Leaf m))
    | Term.V (x, Term.Leaf n), _ when n = m ->
      mk x (Term.V (b, Leaf m))
    (* n in right child's left: move inner split to left side *)
    (* mk(A, op2(Leaf n, X)) → mk(op2(Leaf n, A), X) *)
    | _, Term.H (Term.Leaf n, x) when n = m ->
      mk (Term.H (Leaf m, a)) x
    | _, Term.V (Term.Leaf n, x) when n = m ->
      mk (Term.V (Leaf m, a)) x
    (* n in right child's right: move inner split to left side *)
    (* mk(A, op2(X, Leaf n)) → mk(op2(A, Leaf n), X) *)
    | _, Term.H (x, Term.Leaf n) when n = m ->
      mk (Term.H (a, Leaf m)) x
    | _, Term.V (x, Term.Leaf n) when n = m ->
      mk (Term.V (a, Leaf m)) x
    (* Not at this level, recurse *)
    | _ ->
      if has_leaf m a then mk (go a) b
      else if has_leaf m b then mk a (go b)
      else node
  and rotate_in node m =
    (* Cross-type rotation: Op1(a, Op2(..n..)) or Op1(Op2(..n..), b)
       Only fires when Op1 ≠ Op2. *)
    let is_h = function Term.H _ -> true | _ -> false in
    let is_v = function Term.V _ -> true | _ -> false in
    let cross_type parent child =
      (is_h parent && is_v child) || (is_v parent && is_h child)
    in
    let mk_outer, a, b = match node with
      | Term.H (a, b) -> (fun x y -> Term.H (x, y)), a, b
      | Term.V (a, b) -> (fun x y -> Term.V (x, y)), a, b
      | _ -> assert false
    in
    let mk_inner child = match child with
      | Term.H _ -> (fun x y -> Term.H (x, y))
      | Term.V _ -> (fun x y -> Term.V (x, y))
      | _ -> assert false
    in
    (* Check right child *)
    (match b with
     | Term.H (Term.Leaf n, c) | Term.V (Term.Leaf n, c)
       when n = m && cross_type node b ->
       let mki = mk_inner b in
       mki (Leaf m) (mk_outer a c)
     | Term.H (c, Term.Leaf n) | Term.V (c, Term.Leaf n)
       when n = m && cross_type node b ->
       let mki = mk_inner b in
       mki (mk_outer a c) (Leaf m)
     | _ ->
    (* Check left child *)
    match a with
     | Term.H (Term.Leaf n, c) | Term.V (Term.Leaf n, c)
       when n = m && cross_type node a ->
       let mki = mk_inner a in
       mki (Leaf m) (mk_outer c b)
     | Term.H (c, Term.Leaf n) | Term.V (c, Term.Leaf n)
       when n = m && cross_type node a ->
       let mki = mk_inner a in
       mki (mk_outer c b) (Leaf m)
     | _ ->
       (* Recurse deeper *)
       if has_leaf m a then mk_outer (go a) b
       else if has_leaf m b then mk_outer a (go b)
       else node)
  and transpose_in node m =
    (* Recurse to find the node containing Leaf m as a direct child *)
    let mk, a, b = match node with
      | Term.H (a, b) -> (fun x y -> Term.H (x, y)), a, b
      | Term.V (a, b) -> (fun x y -> Term.V (x, y)), a, b
      | _ -> assert false
    in
    if has_leaf m a then mk (go a) b
    else if has_leaf m b then mk a (go b)
    else node
  in
  go term

let apply_all rules =
  List.fold_left (fun term rule -> apply rule term) (Term.Leaf 0) rules
