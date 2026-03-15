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
    | Term.H (a, b, _) | Term.V (a, b, _) -> max (go a) (go b)
  in
  go term

let has_leaf n term =
  let rec go = function
    | Term.Leaf m -> m = n
    | Term.H (a, b, _) | Term.V (a, b, _) -> go a || go b
  in
  go term

(* Compute area fraction of each leaf *)
let leaf_areas term =
  let rec go frac = function
    | Term.Leaf n -> [(n, frac)]
    | Term.H (a, b, r) | Term.V (a, b, r) ->
      go (Q.mul frac r) a @ go (Q.mul frac (Q.sub Q.one r)) b
  in
  go Q.one term

(* Sum of area fractions for leaves in a subtree *)
let subtree_area areas term =
  let rec go = function
    | Term.Leaf n -> List.assoc n areas
    | Term.H (a, b, _) | Term.V (a, b, _) -> Q.add (go a) (go b)
  in
  go term

(* Assign ratios top-down to preserve leaf areas *)
let fix_ratios areas term =
  let rec go = function
    | Term.Leaf n -> Term.Leaf n
    | Term.H (a, b, _) ->
      let sa = subtree_area areas a in
      let sb = subtree_area areas b in
      let r = Q.div sa (Q.add sa sb) in
      Term.H (go a, go b, r)
    | Term.V (a, b, _) ->
      let sa = subtree_area areas a in
      let sb = subtree_area areas b in
      let r = Q.div sa (Q.add sa sb) in
      Term.V (go a, go b, r)
  in
  go term

(* Structural rewrite: produces correct tree shape with placeholder ratios.
   fix_ratios is called afterwards to set area-preserving ratios. *)
let apply rule term =
  let fresh = max_leaf term + 1 in
  let h = Q.half in
  let areas = leaf_areas term in
  let result =
    let rec go = function
      | Term.Leaf n ->
        (match rule with
         | Split_h m when m = n -> Term.H (Leaf n, Leaf fresh, h)
         | Split_v m when m = n -> Term.V (Leaf n, Leaf fresh, h)
         | _ -> Term.Leaf n)
      | Term.H (a, b, r) ->
        (match rule with
         | Close_h_l m when a = Leaf m -> b
         | Close_h_r m when b = Leaf m -> a
         | Swap m when a = Leaf m -> Term.H (b, Leaf m, h)
         | Swap m when b = Leaf m -> Term.H (Leaf m, a, h)
         | Transpose m when a = Leaf m -> Term.V (a, b, h)
         | Transpose m when b = Leaf m -> Term.V (a, b, h)
         | Promote m -> promote_in (Term.H (a, b, r)) m
         | Demote m -> demote_in (Term.H (a, b, r)) m
         | Rotate m -> rotate_in (Term.H (a, b, r)) m
         | Transpose m -> transpose_in (Term.H (a, b, r)) m
         | _ -> Term.H (go a, go b, r))
      | Term.V (a, b, r) ->
        (match rule with
         | Close_v_l m when a = Leaf m -> b
         | Close_v_r m when b = Leaf m -> a
         | Swap m when a = Leaf m -> Term.V (b, Leaf m, h)
         | Swap m when b = Leaf m -> Term.V (Leaf m, a, h)
         | Transpose m when a = Leaf m -> Term.H (a, b, h)
         | Transpose m when b = Leaf m -> Term.H (a, b, h)
         | Promote m -> promote_in (Term.V (a, b, r)) m
         | Demote m -> demote_in (Term.V (a, b, r)) m
         | Rotate m -> rotate_in (Term.V (a, b, r)) m
         | Transpose m -> transpose_in (Term.V (a, b, r)) m
         | _ -> Term.V (go a, go b, r))
    and promote_in node m =
      let mk, a, b, _r = match node with
        | Term.H (a, b, r) -> (fun x y r -> Term.H (x, y, r)), a, b, r
        | Term.V (a, b, r) -> (fun x y r -> Term.V (x, y, r)), a, b, r
        | _ -> assert false
      in
      match a, b with
      | Term.H (Term.Leaf n, b2, _), _ when n = m ->
        mk (Leaf m) (Term.H (b2, b, h)) h
      | Term.V (Term.Leaf n, b2, _), _ when n = m ->
        mk (Leaf m) (Term.V (b2, b, h)) h
      | Term.H (a2, Term.Leaf n, _), _ when n = m ->
        mk (Term.H (a2, b, h)) (Leaf m) h
      | Term.V (a2, Term.Leaf n, _), _ when n = m ->
        mk (Term.V (a2, b, h)) (Leaf m) h
      | _, Term.H (Term.Leaf n, b2, _) when n = m ->
        mk (Term.H (Leaf m, a, h)) b2 h
      | _, Term.V (Term.Leaf n, b2, _) when n = m ->
        mk (Term.V (Leaf m, a, h)) b2 h
      | _, Term.H (a2, Term.Leaf n, _) when n = m ->
        mk a2 (Term.H (a, Leaf m, h)) h
      | _, Term.V (a2, Term.Leaf n, _) when n = m ->
        mk a2 (Term.V (a, Leaf m, h)) h
      | _ ->
        if has_leaf m a then mk (go a) b h
        else if has_leaf m b then mk a (go b) h
        else node
    and demote_in node m =
      let mk, a, b, _r = match node with
        | Term.H (a, b, r) -> (fun x y r -> Term.H (x, y, r)), a, b, r
        | Term.V (a, b, r) -> (fun x y r -> Term.V (x, y, r)), a, b, r
        | _ -> assert false
      in
      match a, b with
      | Term.Leaf n, Term.H (b1, b2, _) when n = m ->
        mk (Term.H (Leaf m, b1, h)) b2 h
      | Term.Leaf n, Term.V (b1, b2, _) when n = m ->
        mk (Term.V (Leaf m, b1, h)) b2 h
      | Term.H (a1, a2, _), Term.Leaf n when n = m ->
        mk a1 (Term.H (a2, Leaf m, h)) h
      | Term.V (a1, a2, _), Term.Leaf n when n = m ->
        mk a1 (Term.V (a2, Leaf m, h)) h
      | Term.H (Term.Leaf n, x, _), _ when n = m ->
        mk x (Term.H (Leaf m, b, h)) h
      | Term.V (Term.Leaf n, x, _), _ when n = m ->
        mk x (Term.V (Leaf m, b, h)) h
      | Term.H (x, Term.Leaf n, _), _ when n = m ->
        mk x (Term.H (b, Leaf m, h)) h
      | Term.V (x, Term.Leaf n, _), _ when n = m ->
        mk x (Term.V (b, Leaf m, h)) h
      | _, Term.H (Term.Leaf n, x, _) when n = m ->
        mk (Term.H (Leaf m, a, h)) x h
      | _, Term.V (Term.Leaf n, x, _) when n = m ->
        mk (Term.V (Leaf m, a, h)) x h
      | _, Term.H (x, Term.Leaf n, _) when n = m ->
        mk (Term.H (a, Leaf m, h)) x h
      | _, Term.V (x, Term.Leaf n, _) when n = m ->
        mk (Term.V (a, Leaf m, h)) x h
      | _ ->
        if has_leaf m a then mk (go a) b h
        else if has_leaf m b then mk a (go b) h
        else node
    and rotate_in node m =
      let is_h = function Term.H _ -> true | _ -> false in
      let is_v = function Term.V _ -> true | _ -> false in
      let cross_type parent child =
        (is_h parent && is_v child) || (is_v parent && is_h child)
      in
      let mk_outer, a, b, _r = match node with
        | Term.H (a, b, r) -> (fun x y r -> Term.H (x, y, r)), a, b, r
        | Term.V (a, b, r) -> (fun x y r -> Term.V (x, y, r)), a, b, r
        | _ -> assert false
      in
      let mk_inner child = match child with
        | Term.H _ -> (fun x y r -> Term.H (x, y, r))
        | Term.V _ -> (fun x y r -> Term.V (x, y, r))
        | _ -> assert false
      in
      (match b with
       | Term.H (Term.Leaf n, c, _) | Term.V (Term.Leaf n, c, _)
         when n = m && cross_type node b ->
         let mki = mk_inner b in
         mki (Leaf m) (mk_outer a c h) h
       | Term.H (c, Term.Leaf n, _) | Term.V (c, Term.Leaf n, _)
         when n = m && cross_type node b ->
         let mki = mk_inner b in
         mki (mk_outer a c h) (Leaf m) h
       | _ ->
      match a with
       | Term.H (Term.Leaf n, c, _) | Term.V (Term.Leaf n, c, _)
         when n = m && cross_type node a ->
         let mki = mk_inner a in
         mki (Leaf m) (mk_outer c b h) h
       | Term.H (c, Term.Leaf n, _) | Term.V (c, Term.Leaf n, _)
         when n = m && cross_type node a ->
         let mki = mk_inner a in
         mki (mk_outer c b h) (Leaf m) h
       | _ ->
         if has_leaf m a then mk_outer (go a) b h
         else if has_leaf m b then mk_outer a (go b) h
         else node)
    and transpose_in node m =
      let mk, a, b, _r = match node with
        | Term.H (a, b, r) -> (fun x y r -> Term.H (x, y, r)), a, b, r
        | Term.V (a, b, r) -> (fun x y r -> Term.V (x, y, r)), a, b, r
        | _ -> assert false
      in
      if has_leaf m a then mk (go a) b h
      else if has_leaf m b then mk a (go b) h
      else node
    in
    go term
  in
  (* Fix ratios to preserve leaf areas *)
  let areas = match rule with
    | Split_h n | Split_v n ->
      let old_area = List.assoc n areas in
      let half_area = Q.div old_area (Q.make 2 1) in
      (fresh, half_area) ::
      List.map (fun (k, v) -> if k = n then (k, half_area) else (k, v)) areas
    | _ -> areas
  in
  fix_ratios areas result

let apply_all rules =
  List.fold_left (fun term rule -> apply rule term) (Term.Leaf 0) rules
