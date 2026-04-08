(* Model checker for strong guillotine counting.
   Three independent methods, cross-validated:
   - Cross-junction resolutions: Σ 2^k(T) per weak tiling (existing)
   - Method A: Σ multiplicity(T) via tree-recursive boundary_tiles formula
   - Method B: §5.3 recurrence S(n, ℓ, t, r, b), independent of tree enum
   Plus: D4-reduced counting via full strong class enumeration. *)

let label_tiling (is_h, unit_tree) =
  let counter = ref 0 in
  let rec label = function
    | Schrot.Tile () -> let k = !counter in incr counter; Schrot.Tile k
    | Schrot.Frame ch -> Schrot.Frame (List2.map label ch)
  in
  ((is_h, label unit_tree) : Tiling.t)

(* Relabel a tiling to DFS order, returning (relabeled_tiling, perm)
   where perm.(old_label) = new_label *)
let relabel_dfs t =
  let n = List.length (Tiling.leaves t) in
  let perm = Array.make n 0 in
  let counter = ref 0 in
  let rec go = function
    | Schrot.Tile old_id ->
      let new_id = !counter in
      incr counter;
      perm.(old_id) <- new_id;
      Schrot.Tile new_id
    | Schrot.Frame ch -> Schrot.Frame (List2.map go ch)
  in
  let (is_h, tree) = t in
  let tree' = go tree in
  ((is_h, tree'), perm)

(* Apply a permutation to an edge set *)
let perm_edges perm edges =
  List.map (fun (a, b) ->
    let a' = perm.(a) and b' = perm.(b) in
    if a' < b' then (a', b') else (b', a')
  ) edges |> List.sort compare

(* Canonical D4 form for a strong class (tiling, edge_set). *)
let canonical_strong_d4 t edges =
  let images = Tiling.d4_orbit t in
  let fingerprints = List.map (fun t_d ->
    let (t_relabeled, perm) = relabel_dfs t_d in
    let edges_relabeled = perm_edges perm edges in
    let tree_str = Tiling.to_string t_relabeled in
    let edge_str = String.concat ";"
      (List.map (fun (a, b) -> Printf.sprintf "%d-%d" a b) edges_relabeled) in
    tree_str ^ "|" ^ edge_str
  ) images in
  let sorted = List.sort String.compare fingerprints in
  List.hd sorted

(* --- Method B: §5.3 recurrence --- *)

let binom = Tiling.binom

(* §5.3 recurrence parameterized by the interleaving weight function.
   Generic: binom(r'+l', r').  Non-generic: delannoy(r', l'). *)
let guillotine_recurrence ~weight max_n =
  let memo : (int * int * int * int * int * bool, int) Hashtbl.t =
    Hashtbl.create 4096 in
  let rec sv n l t r b =
    if n = 1 then
      (if l = 0 && t = 0 && r = 0 && b = 0 then 1 else 0)
    else if t < 1 || b < 1 then 0
    else
      let key = (n, l, t, r, b, true) in
      match Hashtbl.find_opt memo key with
      | Some v -> v
      | None ->
        let total = ref 0 in
        for n' = 1 to n - 1 do
          for t' = 0 to t - 1 do
            for b' = 0 to b - 1 do
              for r' = 0 to n' - 1 do
                for l' = 0 to n - n' - 1 do
                  let sh_left = sv n' t' r' b' l in
                  if sh_left > 0 then begin
                    let s_right = s (n - n') l' (t - 1 - t') r (b - 1 - b') in
                    if s_right > 0 then
                      total := !total + sh_left * s_right * weight r' l'
                  end
                done
              done
            done
          done
        done;
        Hashtbl.replace memo key !total;
        !total
  and sh n l t r b = sv n t r b l
  and s n l t r b =
    if n = 1 then
      (if l = 0 && t = 0 && r = 0 && b = 0 then 1 else 0)
    else
      sv n l t r b + sh n l t r b
  in
  let results = Array.make (max_n + 1) 0 in
  for n = 1 to max_n do
    let total = ref 0 in
    for l = 0 to n - 1 do
      for t = 0 to n - 1 do
        for r = 0 to n - 1 do
          for b = 0 to n - 1 do
            total := !total + s n l t r b
          done
        done
      done
    done;
    results.(n) <- !total
  done;
  results

let strong_guillotine_recurrence =
  guillotine_recurrence ~weight:(fun r' l' -> binom (r' + l') r')
let nongeneric_recurrence =
  guillotine_recurrence ~weight:Tiling.delannoy

let () =
  let max_leaves = ref 7 in
  Arg.parse [
    "--max-leaves", Arg.Set_int max_leaves, "N Max leaves (default: 7)";
  ] (fun _ -> ()) "strong_check [--max-leaves N]";
  let all_ok = ref true in
  let fail fmt = Printf.ksprintf (fun s ->
    all_ok := false; Printf.printf "FAIL: %s\n%!" s) fmt in
  Printf.printf "Strong guillotine counting checker\n%!";
  Printf.printf "==================================\n%!";
  let max_rec = max !max_leaves 10 in
  Printf.printf "Computing recurrences up to n=%d...\n%!" max_rec;
  let recurrence = strong_guillotine_recurrence max_rec in
  let ng_recurrence = nongeneric_recurrence max_rec in
  for n = 1 to !max_leaves do
    let unit_tilings = Schrot.enum n in
    let all_tilings = List.map label_tiling unit_tilings in
    let n_tilings = List.length all_tilings in
    let cross_count = ref 0 in
    let mult_count = ref 0 in
    let ng_mult_count = ref 0 in
    (* D4 orbit tables *)
    let d4_orbit_tbl = Hashtbl.create 256 in
    let ng_d4_tbl = Hashtbl.create 256 in
    List.iter (fun t ->
      let k = Tiling.count_cross_junctions t in
      cross_count := !cross_count + (1 lsl k);
      let m = Tiling.multiplicity t in
      mult_count := !mult_count + m;
      (* Check: multiplicity >= 2^k *)
      if m < (1 lsl k) then
        fail "n=%d: multiplicity %d < 2^k=%d for %s"
          n m (1 lsl k) (Tiling.to_string t);
      (* Enumerate ALL strong classes for D4 reduction *)
      let all_adjs = Tiling.enumerate_all_strong_adjacencies t in
      let n_adjs = List.length all_adjs in
      if n_adjs <> m then
        fail "n=%d: enumerate_all gave %d, multiplicity=%d for %s"
          n n_adjs m (Tiling.to_string t);
      (* Check distinctness *)
      let edge_strs = List.map (fun edges ->
        String.concat ";" (List.map (fun (a, b) ->
          Printf.sprintf "%d-%d" a b) edges)
      ) all_adjs in
      let unique = List.sort_uniq String.compare edge_strs in
      if List.length unique <> n_adjs then
        fail "n=%d: duplicate edge sets in %s" n (Tiling.to_string t);
      (* D4 canonical forms *)
      List.iter (fun edges ->
        let key = canonical_strong_d4 t edges in
        if not (Hashtbl.mem d4_orbit_tbl key) then
          Hashtbl.replace d4_orbit_tbl key ()
      ) all_adjs;
      (* Non-generic *)
      let m_ng = Tiling.multiplicity_nongeneric t in
      ng_mult_count := !ng_mult_count + m_ng;
      let all_ng = Tiling.enumerate_all_strong_adjacencies_nongeneric t in
      let n_ng = List.length all_ng in
      if n_ng <> m_ng then
        fail "n=%d: ng enumerate gave %d, ng multiplicity=%d for %s"
          n n_ng m_ng (Tiling.to_string t);
      (* Check ng distinctness *)
      let ng_strs = List.map (fun edges ->
        String.concat ";" (List.map (fun (a,b) ->
          Printf.sprintf "%d-%d" a b) edges)) all_ng in
      let ng_unique = List.sort_uniq String.compare ng_strs in
      if List.length ng_unique <> n_ng then
        fail "n=%d: ng duplicate edge sets in %s" n (Tiling.to_string t);
      List.iter (fun edges ->
        let key = canonical_strong_d4 t edges in
        if not (Hashtbl.mem ng_d4_tbl key) then
          Hashtbl.replace ng_d4_tbl key ()
      ) all_ng
    ) all_tilings;
    let rec_n = recurrence.(n) in
    let ng_rec_n = ng_recurrence.(n) in
    let d4_count = Hashtbl.length d4_orbit_tbl in
    let ng_d4_count = Hashtbl.length ng_d4_tbl in
    (* Cross-validate methods *)
    if !mult_count <> rec_n then
      fail "n=%d: generic mult (%d) ≠ rec (%d)" n !mult_count rec_n;
    if !ng_mult_count <> ng_rec_n then
      fail "n=%d: ng mult (%d) ≠ ng rec (%d)" n !ng_mult_count ng_rec_n;
    (* Burnside decomposition: for each D4 element, count fixed points *)
    let weak_fix = Array.make 8 0 in
    let strong_fix = Array.make 8 0 in
    let ng_fix = Array.make 8 0 in
    List.iter (fun t ->
      let (is_h_t, tree_t) = Tiling.erase t in
      let t_str = Tiling.unit_tree_to_string is_h_t tree_t in
      let all_adjs = Tiling.enumerate_all_strong_adjacencies t in
      let all_ng = Tiling.enumerate_all_strong_adjacencies_nongeneric t in
      Array.iteri (fun gi (_, action) ->
        (* Weak: does g fix this tree shape? *)
        let img = action t in
        let (is_h_img, tree_img) = Tiling.erase img in
        let img_str = Tiling.unit_tree_to_string is_h_img tree_img in
        if img_str = t_str then
          weak_fix.(gi) <- weak_fix.(gi) + 1;
        (* Strong: does g fix each (tree, edge_set)? *)
        let (_, perm) = relabel_dfs img in
        let t_labeled_str = Tiling.to_string t in
        let img_labeled_str = Tiling.to_string (fst (relabel_dfs img)) in
        if img_labeled_str = t_labeled_str then begin
          List.iter (fun edges ->
            let edges' = perm_edges perm edges in
            if edges' = edges then
              strong_fix.(gi) <- strong_fix.(gi) + 1
          ) all_adjs;
          List.iter (fun edges ->
            let edges' = perm_edges perm edges in
            if edges' = edges then
              ng_fix.(gi) <- ng_fix.(gi) + 1
          ) all_ng
        end
      ) Tiling.d4_actions
    ) all_tilings;
    (* Verify Burnside: Σ Fix_g / 8 = D4 count *)
    let weak_sum = Array.fold_left ( + ) 0 weak_fix in
    let strong_sum = Array.fold_left ( + ) 0 strong_fix in
    let ng_sum = Array.fold_left ( + ) 0 ng_fix in
    if weak_sum mod 8 <> 0 then
      fail "n=%d: weak Burnside sum %d not divisible by 8" n weak_sum;
    if strong_sum mod 8 <> 0 then
      fail "n=%d: strong Burnside sum %d not divisible by 8" n strong_sum;
    if ng_sum mod 8 <> 0 then
      fail "n=%d: ng Burnside sum %d not divisible by 8" n ng_sum;
    if strong_sum / 8 <> d4_count then
      fail "n=%d: strong Burnside %d/8=%d ≠ D4=%d" n strong_sum (strong_sum/8) d4_count;
    if ng_sum / 8 <> ng_d4_count then
      fail "n=%d: ng Burnside %d/8=%d ≠ D4=%d" n ng_sum (ng_sum/8) ng_d4_count;
    Printf.printf "n=%d: %d weak, strong=%d, ng=%d, D4=%d/%d\n%!"
      n n_tilings !mult_count !ng_mult_count d4_count ng_d4_count;
    Printf.printf "  Burnside weak:   [%s]\n%!"
      (String.concat ", " (Array.to_list (Array.map string_of_int weak_fix)));
    Printf.printf "  Burnside strong: [%s]\n%!"
      (String.concat ", " (Array.to_list (Array.map string_of_int strong_fix)));
    Printf.printf "  Burnside ng:     [%s]\n%!"
      (String.concat ", " (Array.to_list (Array.map string_of_int ng_fix)))
  done;
  (* Extended sequences from recurrence only *)
  Printf.printf "\nExtended sequences (recurrence only):\n%!";
  Printf.printf "  generic:     ";
  for n = 1 to max_rec do Printf.printf "%d%s" recurrence.(n)
    (if n < max_rec then ", " else "\n%!") done;
  Printf.printf "  non-generic: ";
  for n = 1 to max_rec do Printf.printf "%d%s" ng_recurrence.(n)
    (if n < max_rec then ", " else "\n%!") done;
  Printf.printf "\n%s\n"
    (if !all_ok then "All checks passed."
     else "*** SOME CHECKS FAILED ***")
