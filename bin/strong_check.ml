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
    | Schrot.Frame ch -> Schrot.Frame (List2.map (fun (w, c) -> (w, label c)) ch)
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
    | Schrot.Frame ch -> Schrot.Frame (List2.map (fun (w, c) -> (w, go c)) ch)
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

(* Sparse bottom-up recurrence.  Stores only non-zero entries, indexed
   for fast convolution.  Much faster for large n. *)
let guillotine_recurrence_sparse ~weight max_n =
  (* sv_by_nl.(n) is a map: ℓ -> list of (t, r, b, value) for S_V *)
  let sv_tbl : (int * int * int * int * int, int) Hashtbl.t =
    Hashtbl.create 65536 in
  let s_tbl : (int * int * int * int * int, int) Hashtbl.t =
    Hashtbl.create 65536 in
  (* S_V entries indexed by (n, ℓ) for left-part lookup *)
  let sv_by_nl : (int * int, (int * int * int * int) list) Hashtbl.t =
    Hashtbl.create 4096 in
  (* S entries indexed by (n, r) for right-part lookup *)
  let s_by_nr : (int * int, (int * int * int * int) list) Hashtbl.t =
    Hashtbl.create 4096 in
  let add_sv n l t r b v =
    if v > 0 then begin
      Hashtbl.replace sv_tbl (n, l, t, r, b) v;
      let key = (n, l) in
      let prev = try Hashtbl.find sv_by_nl key with Not_found -> [] in
      Hashtbl.replace sv_by_nl key ((t, r, b, v) :: prev)
    end
  in
  let add_s n l t r b v =
    if v > 0 then begin
      Hashtbl.replace s_tbl (n, l, t, r, b) v;
      let key = (n, r) in
      let prev = try Hashtbl.find s_by_nr key with Not_found -> [] in
      Hashtbl.replace s_by_nr key ((l, t, b, v) :: prev)
    end
  in
  (* Base: S_V(1, 0, 0, 0, 0) = 1, S(1, 0, 0, 0, 0) = 1 *)
  add_sv 1 0 0 0 0 1;
  add_s 1 0 0 0 0 1;
  let results = Array.make (max_n + 1) 0 in
  results.(1) <- 1;
  (* Build level by level *)
  for n = 2 to max_n do
    (* Compute S_V(n, ℓ, t, r, b) by convolving left-part (S_H) with right-part (S) *)
    (* S_H(n', ℓ, t', r', b') = S_V(n', t', r', b', ℓ) — look up sv_by_nl with key (n', t')
       and match the 4th component = ℓ *)
    (* More efficient: iterate over all non-zero S_V entries for left part,
       and all non-zero S entries for right part, accumulating results. *)
    let sv_new : (int * int * int * int, int) Hashtbl.t = Hashtbl.create 1024 in
    for n' = 1 to n - 1 do
      let n_right = n - n' in
      (* Left part: S_H(n', ℓ_target, t', r', b') = S_V(n', t', r', b', ℓ_target)
         So we iterate S_V entries for size n': for each (t', r', b', ℓ_target, val),
         the left part contributes S_H(n', ℓ_target, t', r', b') = val.
         S_V is indexed by (n, l) in sv_by_nl. *)
      (* Collect all S_V(n', l, t, r, b) entries for left part *)
      let left_entries = ref [] in
      for l_sv = 0 to n' - 1 do
        match Hashtbl.find_opt sv_by_nl (n', l_sv) with
        | None -> ()
        | Some entries ->
          List.iter (fun (t_sv, r_sv, b_sv, v_sv) ->
            (* This is S_V(n', l_sv, t_sv, r_sv, b_sv) = v_sv
               = S_H(n', b_sv, l_sv, t_sv, r_sv) [by S_H(n,l,t,r,b) = S_V(n,t,r,b,l)]
               Wait, S_H(n, l, t, r, b) = S_V(n, t, r, b, l).
               So S_V(n', l_sv, t_sv, r_sv, b_sv) = S_H(n', b_sv, l_sv, t_sv, r_sv)... no.
               Let me re-derive: sh n l t r b = sv n t r b l.
               So S_H(n', L, T, R, B) = S_V(n', T, R, B, L).
               If I have S_V(n', l_sv, t_sv, r_sv, b_sv) = v, then this equals
               S_H(n', b_sv, l_sv, t_sv, r_sv).
               The left part in the formula is S_H(n', ℓ, t', r', b'), so:
               ℓ = b_sv, t' = l_sv, r' = t_sv, b' = r_sv. *)
            left_entries := (b_sv, l_sv, t_sv, r_sv, v_sv) :: !left_entries
              (* (ℓ_target, t', r', b', value) *)
          ) entries
      done;
      (* Also add size-1 left part: S_H(1, 0, 0, 0, 0) = S_V(1, 0, 0, 0, 0) = 1 *)
      (* Already included if n'=1 and sv_by_nl has the entry. But S_V(1,0,0,0,0)
         should be in the table... we didn't add it. Let me add it. *)
      (* For right part: iterate S entries for size n_right, indexed by (n_right, r_target) *)
      List.iter (fun (l_target, t', r', b', h_val) ->
        (* Right part: S(n_right, ℓ', t-1-t', r_target, b-1-b')
           The target S_V(n, l_target, t, r_target, b) gets:
           t = 1 + t' + t_right, where t_right comes from right part
           b = 1 + b' + b_right
           r_target = r from right part
           So we need S(n_right, ℓ', t_right, r_target, b_right) entries. *)
        (* Iterate over all non-zero S entries for size n_right *)
        for r_target = 0 to n_right - 1 do
          match Hashtbl.find_opt s_by_nr (n_right, r_target) with
          | None -> ()
          | Some entries ->
            List.iter (fun (l', t_right, b_right, s_val) ->
              let t_out = 1 + t' + t_right in
              let b_out = 1 + b' + b_right in
              let w = weight r' l' in
              let contrib = h_val * s_val * w in
              if contrib > 0 then begin
                let key = (l_target, t_out, r_target, b_out) in
                let prev = try Hashtbl.find sv_new key with Not_found -> 0 in
                Hashtbl.replace sv_new key (prev + contrib)
              end
            ) entries
        done
      ) !left_entries
    done;
    (* Store the new S_V entries and compute S = S_V + S_H *)
    let total = ref 0 in
    Hashtbl.iter (fun (l, t, r, b) v ->
      add_sv n l t r b v;
      (* S_H(n, l, t, r, b) = S_V(n, t, r, b, l) — already in sv_tbl *)
      let sh_val = try Hashtbl.find sv_tbl (n, t, r, b, l) with Not_found -> 0 in
      let s_val = v + sh_val in
      (* But we also need S_H for states not covered by sv_new.
         S_H(n, l, t, r, b) = S_V(n, t, r, b, l). We just added all S_V(n, ...)
         entries, so sv_tbl has them. But we need to compute S for all (l,t,r,b)
         where either S_V or S_H is non-zero. *)
      ignore s_val
    ) sv_new;
    (* Compute S(n, l, t, r, b) = S_V(n, l, t, r, b) + S_H(n, l, t, r, b)
       = S_V(n, l, t, r, b) + S_V(n, t, r, b, l) *)
    let s_new : (int * int * int * int, int) Hashtbl.t = Hashtbl.create 1024 in
    (* Collect all (l, t, r, b) where S_V(n, l, t, r, b) > 0 *)
    Hashtbl.iter (fun (l, t, r, b) _ ->
      if not (Hashtbl.mem s_new (l, t, r, b)) then
        Hashtbl.replace s_new (l, t, r, b) 0;
      (* Also (t, r, b, l) contributes via S_H *)
      if not (Hashtbl.mem s_new (t, r, b, l)) then
        Hashtbl.replace s_new (t, r, b, l) 0
    ) sv_new;
    Hashtbl.iter (fun (l, t, r, b) _ ->
      let sv_val = try Hashtbl.find sv_tbl (n, l, t, r, b) with Not_found -> 0 in
      let sh_val = try Hashtbl.find sv_tbl (n, t, r, b, l) with Not_found -> 0 in
      let s_val = sv_val + sh_val in
      if s_val > 0 then begin
        add_s n l t r b s_val;
        total := !total + s_val
      end
    ) s_new;
    results.(n) <- !total;
    Printf.printf "  n=%d: %d (sv_entries=%d, s_entries=%d)\n%!"
      n !total (Hashtbl.length sv_new) (Hashtbl.length s_new)
  done;
  results

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
  let max_rec = !max_leaves in
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
  (* Extended sequences via sparse recurrence *)
  let max_ext = 20 in
  Printf.printf "\nExtending via sparse recurrence to n=%d...\n%!" max_ext;
  Printf.printf "Generic:\n%!";
  let ext_gen = guillotine_recurrence_sparse
    ~weight:(fun r' l' -> binom (r' + l') r') max_ext in
  Printf.printf "Non-generic:\n%!";
  let ext_ng = guillotine_recurrence_sparse
    ~weight:Tiling.delannoy max_ext in
  (* Cross-validate against dense recurrence for overlap *)
  for n = 1 to max_rec do
    if ext_gen.(n) <> recurrence.(n) then
      fail "sparse generic n=%d: %d ≠ dense %d" n ext_gen.(n) recurrence.(n);
    if ext_ng.(n) <> ng_recurrence.(n) then
      fail "sparse ng n=%d: %d ≠ dense %d" n ext_ng.(n) ng_recurrence.(n)
  done;
  Printf.printf "\nExtended generic:     ";
  for n = 1 to max_ext do Printf.printf "%d%s" ext_gen.(n)
    (if n < max_ext then ", " else "\n%!") done;
  Printf.printf "Extended non-generic: ";
  for n = 1 to max_ext do Printf.printf "%d%s" ext_ng.(n)
    (if n < max_ext then ", " else "\n%!") done;
  Printf.printf "\n%s\n"
    (if !all_ok then "All checks passed."
     else "*** SOME CHECKS FAILED ***")
