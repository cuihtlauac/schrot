(* Cross-check Schrot.enum against the separable permutation bijection.

   Guillotine rectangulations biject with separable permutations Av(2413, 3142)
   via the substitution decomposition (YCCG03, ABP06a).  This checker verifies
   the bijection by round-tripping in both directions.

   Reference: Merino, Mütze. "Combinatorial generation via permutation
   languages. III. Rectangulations." arXiv:2103.09333v2, Table 1. *)

(* --- tree -> permutation --- *)

(* Assign values to leaves of an unlabeled Schrot tree.
   V (direct sum, ⊕): children get ascending value ranges.
   H (skew sum, ⊖): children get descending value ranges.
   Positions = DFS leaf order (0..n-1). *)
let tree_to_perm (is_h_root, tree) =
  let n = Schrot.size tree in
  let perm = Array.make n 0 in
  let rec go pos lo hi is_h = function
    | Schrot.Tile () ->
      assert (lo = hi);
      perm.(pos) <- lo
    | Schrot.Frame children ->
      let sizes = List.map (fun (_, c) -> Schrot.size c)
                    (List2.to_list children) in
      let k = List.length sizes in
      (* Cumulative position offsets *)
      let pos_off = Array.make (k + 1) pos in
      List.iteri (fun i s -> pos_off.(i + 1) <- pos_off.(i) + s) sizes;
      (* Value ranges per child *)
      let val_ranges = Array.make k (0, 0) in
      if is_h then begin
        (* Descending: first child gets highest values *)
        let v = ref hi in
        List.iteri (fun i s ->
          val_ranges.(i) <- (!v - s + 1, !v);
          v := !v - s
        ) sizes
      end else begin
        (* Ascending: first child gets lowest values *)
        let v = ref lo in
        List.iteri (fun i s ->
          val_ranges.(i) <- (!v, !v + s - 1);
          v := !v + s
        ) sizes
      end;
      List.iteri (fun i (_, child) ->
        let (vlo, vhi) = val_ranges.(i) in
        go pos_off.(i) vlo vhi (not is_h) child
      ) (List2.to_list children)
  in
  go 0 0 (n - 1) is_h_root tree;
  perm

(* --- permutation -> tree --- *)

(* Find the maximal direct-sum (⊕) block decomposition.
   Block boundary at position i iff max(perm[0..i]) = i + offset.
   Returns list of (start, length) pairs, or [] if only one block. *)
let direct_sum_blocks perm offset len =
  if len <= 1 then []
  else begin
    let blocks = ref [] in
    let block_start = ref 0 in
    let cur_max = ref (perm.(offset) - offset) in
    for i = 0 to len - 1 do
      cur_max := max !cur_max (perm.(offset + i) - offset);
      if !cur_max = i then begin
        blocks := (offset + !block_start, i - !block_start + 1) :: !blocks;
        block_start := i + 1
      end
    done;
    let blocks = List.rev !blocks in
    if List.length blocks >= 2 then blocks else []
  end

(* Find the maximal skew-sum (⊖) block decomposition.
   Block boundary at position i iff min(perm[0..i]) = (len-1-i) + offset. *)
let skew_sum_blocks perm offset len =
  if len <= 1 then []
  else begin
    let blocks = ref [] in
    let block_start = ref 0 in
    let cur_min = ref (perm.(offset) - offset) in
    for i = 0 to len - 1 do
      cur_min := min !cur_min (perm.(offset + i) - offset);
      if !cur_min = len - 1 - i then begin
        blocks := (offset + !block_start, i - !block_start + 1) :: !blocks;
        block_start := i + 1
      end
    done;
    let blocks = List.rev !blocks in
    if List.length blocks >= 2 then blocks else []
  end

(* Renormalize values in a subarray to 0..len-1 preserving relative order. *)
let renormalize perm offset len =
  let sub = Array.init len (fun i -> perm.(offset + i)) in
  let sorted = Array.copy sub in
  Array.sort compare sorted;
  let rank = Hashtbl.create len in
  Array.iteri (fun i v -> Hashtbl.replace rank v i) sorted;
  Array.map (Hashtbl.find rank) sub

let rec perm_to_tree perm offset len =
  if len = 1 then
    (false, Schrot.Tile (perm.(offset)))
  else
    let ds = direct_sum_blocks perm offset len in
    if ds <> [] then begin
      (* V root (⊕, direct sum) *)
      let children = List.map (fun (off, blen) ->
        let sub = renormalize perm off blen in
        let (_, child) = perm_to_tree sub 0 blen in
        ((), child)
      ) ds in
      match children with
      | a :: b :: rest ->
        (false, Schrot.Frame (List2.Cons2 (a, b, rest)))
      | _ -> assert false
    end else begin
      let ss = skew_sum_blocks perm offset len in
      assert (ss <> []);
      (* H root (⊖, skew sum) *)
      let children = List.map (fun (off, blen) ->
        let sub = renormalize perm off blen in
        let (_, child) = perm_to_tree sub 0 blen in
        ((), child)
      ) ss in
      match children with
      | a :: b :: rest ->
        (true, Schrot.Frame (List2.Cons2 (a, b, rest)))
      | _ -> assert false
    end

(* --- pattern avoidance --- *)

(* Does perm contain pattern as a subsequence (order-isomorphic)? *)
let contains_pattern perm pattern =
  let n = Array.length perm and k = Array.length pattern in
  if k > n then false
  else begin
    (* Brute-force: try all k-element subsequences *)
    let indices = Array.make k 0 in
    let rec search depth start =
      if depth = k then begin
        (* Check order-isomorphism *)
        let sub = Array.init k (fun i -> perm.(indices.(i))) in
        let rank = Array.make k 0 in
        let sorted = Array.init k (fun i -> (sub.(i), i)) in
        Array.sort (fun (a, _) (b, _) -> compare a b) sorted;
        Array.iteri (fun r (_, i) -> rank.(i) <- r) sorted;
        rank = pattern
      end else begin
        let found = ref false in
        let i = ref start in
        while !i <= n - k + depth && not !found do
          indices.(depth) <- !i;
          if search (depth + 1) (!i + 1) then found := true;
          incr i
        done;
        !found
      end
    in
    search 0 0
  end

let avoids perm pattern = not (contains_pattern perm pattern)

let is_separable perm =
  avoids perm [|1; 3; 0; 2|] &&  (* 2413 in 0-indexed *)
  avoids perm [|2; 0; 3; 1|]     (* 3142 in 0-indexed *)

(* --- all permutations of {0..n-1} --- *)

let all_permutations n =
  if n = 0 then [[||]]
  else begin
    let result = ref [] in
    let a = Array.init n Fun.id in
    (* Heap's algorithm *)
    let c = Array.make n 0 in
    result := [Array.copy a];
    let i = ref 0 in
    while !i < n do
      if c.(!i) < !i then begin
        if !i mod 2 = 0 then begin
          let t = a.(0) in a.(0) <- a.(!i); a.(!i) <- t
        end else begin
          let t = a.(c.(!i)) in a.(c.(!i)) <- a.(!i); a.(!i) <- t
        end;
        result := Array.copy a :: !result;
        c.(!i) <- c.(!i) + 1;
        i := 0
      end else begin
        c.(!i) <- 0;
        incr i
      end
    done;
    !result
  end

(* --- tiling canonical string (for comparison) --- *)

(* For unit-labeled trees from Schrot.enum *)
let unit_tiling_string (is_h, tree) =
  Tiling.to_string (Tiling.relabel (is_h, Schrot.map (fun () -> 0) tree))

(* For int-labeled trees from perm_to_tree *)
let int_tiling_string t =
  Tiling.to_string (Tiling.relabel t)

(* --- main --- *)

let () =
  let max_n = ref 8 in
  let max_brute = ref 7 in
  Arg.parse [
    "--max-leaves", Arg.Set_int max_n, "N Max leaves (default: 8)";
    "--max-brute", Arg.Set_int max_brute,
      "N Max leaves for brute-force reverse check (default: 7)";
  ] (fun _ -> ()) "perm_check [--max-leaves N] [--max-brute N]";
  let all_ok = ref true in
  let fail fmt = Printf.ksprintf (fun s ->
    all_ok := false; Printf.printf "FAIL: %s\n%!" s) fmt in
  Printf.printf "Separable permutation bijection cross-check\n\n";
  for n = 1 to !max_n do
    let tilings = Schrot.enum n in
    let ntil = List.length tilings in
    (* Forward: tree -> perm *)
    let perms = List.map (fun t -> (t, tree_to_perm t)) tilings in
    (* Check avoidance *)
    let avoidance_ok = List.for_all (fun (_, p) -> is_separable p) perms in
    if not avoidance_ok then
      fail "n=%d: some permutations are not separable" n;
    (* Check injectivity *)
    let perm_strings = List.map (fun (_, p) ->
      String.concat "," (Array.to_list (Array.map string_of_int p))
    ) perms in
    let unique = List.sort_uniq String.compare perm_strings in
    let injective = List.length unique = ntil in
    if not injective then
      fail "n=%d: tree_to_perm not injective (%d unique / %d total)"
        n (List.length unique) ntil;
    (* Round-trip: tree -> perm -> tree *)
    let round_trip_ok = List.for_all (fun (t, p) ->
      let (is_h', tree') = perm_to_tree p 0 (Array.length p) in
      let t_str = unit_tiling_string t in
      let rt_str = int_tiling_string (is_h', tree') in
      if t_str <> rt_str then begin
        Printf.printf "  round-trip fail: %s -> [%s] -> %s\n"
          t_str
          (String.concat "," (Array.to_list (Array.map string_of_int p)))
          rt_str;
        false
      end else true
    ) perms in
    if not round_trip_ok then
      fail "n=%d: round-trip tree->perm->tree failed" n;
    Printf.printf "n=%d: %d tilings, avoidance=%s injectivity=%s round-trip=%s\n%!"
      n ntil
      (if avoidance_ok then "OK" else "FAIL")
      (if injective then "OK" else "FAIL")
      (if round_trip_ok then "OK" else "FAIL");
    (* Brute-force reverse check for small n *)
    if n <= !max_brute then begin
      let all_perms = all_permutations n in
      let sep_perms = List.filter is_separable all_perms in
      let nsep = List.length sep_perms in
      let count_ok = nsep = ntil in
      if not count_ok then
        fail "n=%d: separable count %d != Schroder %d" n nsep ntil;
      (* perm -> tree for each separable perm *)
      let tree_strings_from_perms =
        List.map (fun p ->
          let t = perm_to_tree p 0 (Array.length p) in
          int_tiling_string t
        ) sep_perms
        |> List.sort_uniq String.compare
      in
      let tree_strings_from_enum =
        List.map unit_tiling_string tilings
        |> List.sort_uniq String.compare
      in
      let surjective = tree_strings_from_perms = tree_strings_from_enum in
      if not surjective then
        fail "n=%d: perm->tree image doesn't match Schrot.enum" n;
      Printf.printf "  brute-force: %d! = %d perms, %d separable, \
                     count=%s surjection=%s\n%!"
        n (List.length all_perms) nsep
        (if count_ok then "OK" else "FAIL")
        (if surjective then "OK" else "FAIL")
    end
  done;
  if !all_ok then
    Printf.printf "\nAll checks passed.\n"
  else begin
    Printf.printf "\nSome checks FAILED.\n";
    exit 1
  end
