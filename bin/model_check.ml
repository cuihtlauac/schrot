(* TODO: Bring up to Schroder tilings *)

(* Generate all binary tree shapes (Catalan recurrence) with k leaves.
   Leaves are numbered 0..k-1 left-to-right as placeholders. *)
let rec shapes k =
  if k = 1 then [Term.Leaf 0]
  else
    let acc = ref [] in
    for i = 1 to k - 1 do
      let lefts = shapes i in
      let rights = shapes (k - i) in
      List.iter (fun l ->
        List.iter (fun r ->
          acc := Term.H (l, r) :: !acc
        ) rights
      ) lefts
    done;
    List.rev !acc

(* Assign H/V to each internal node. mask is a bitmask over internal nodes. *)
let assign_splits shape mask =
  let pos = ref 0 in
  let rec go = function
    | Term.Leaf n -> Term.Leaf n
    | Term.H (a, b) | Term.V (a, b) ->
      let bit = !pos in
      incr pos;
      let a' = go a in
      let b' = go b in
      if (mask lsr bit) land 1 = 0 then Term.H (a', b')
      else Term.V (a', b')
  in
  go shape

let count_internal = function
  | Term.Leaf _ -> 0
  | t ->
    let n = ref 0 in
    let rec go = function
      | Term.Leaf _ -> ()
      | Term.H (a, b) | Term.V (a, b) -> incr n; go a; go b
    in
    go t; !n

(* Place target leaf (label 0) at position `target_pos` (0-indexed left-to-right).
   All other leaves get labels 1..k-1 left-to-right.
   The non-target labels don't affect policy behavior for any command targeting leaf 0. *)
let place_target shape target_pos =
  let pos = ref 0 in
  let next_other = ref 1 in
  let rec go = function
    | Term.Leaf _ ->
      let p = !pos in
      incr pos;
      if p = target_pos then Term.Leaf 0
      else begin
        let n = !next_other in
        incr next_other;
        Term.Leaf n
      end
    | Term.H (a, b) -> Term.H (go a, go b)
    | Term.V (a, b) -> Term.V (go a, go b)
  in
  go shape

(* --- Brute-force enumeration (original, for Split/Close) --- *)

let assign_labels shape perm =
  let pos = ref 0 in
  let rec go = function
    | Term.Leaf _ ->
      let n = perm.(!pos) in
      incr pos;
      Term.Leaf n
    | Term.H (a, b) -> Term.H (go a, go b)
    | Term.V (a, b) -> Term.V (go a, go b)
  in
  go shape

let permutations n =
  let arr = Array.init n Fun.id in
  let results = ref [] in
  let rec gen k =
    if k = 1 then
      results := Array.copy arr :: !results
    else begin
      gen (k - 1);
      for i = 0 to k - 2 do
        if k mod 2 = 0 then begin
          let tmp = arr.(i) in arr.(i) <- arr.(k-1); arr.(k-1) <- tmp
        end else begin
          let tmp = arr.(0) in arr.(0) <- arr.(k-1); arr.(k-1) <- tmp
        end;
        gen (k - 1)
      done
    end
  in
  if n = 0 then [[||]]
  else begin gen n; List.rev !results end

let enumerate_trees k f =
  let all_shapes = shapes k in
  let perms = permutations k in
  List.iter (fun shape ->
    let n_internal = count_internal shape in
    let n_splits = 1 lsl n_internal in
    for mask = 0 to n_splits - 1 do
      let split_tree = assign_splits shape mask in
      List.iter (fun perm ->
        f (assign_labels split_tree perm)
      ) perms
    done
  ) all_shapes

(* --- Symmetry-reduced enumeration for Move --- *)

(* For Move(0, dir): policy behavior depends only on tree structure + position
   of leaf 0, not on labels of other leaves. Enumerate (shape × splits × position)
   instead of (shape × splits × k! permutations × k tiles). *)
let enumerate_reduced k f =
  let all_shapes = shapes k in
  List.iter (fun shape ->
    let n_internal = count_internal shape in
    let n_splits = 1 lsl n_internal in
    for mask = 0 to n_splits - 1 do
      let split_tree = assign_splits shape mask in
      for target_pos = 0 to k - 1 do
        f (place_target split_tree target_pos)
      done
    done
  ) all_shapes

(* --- Checking --- *)

let failures = ref 0
let checks = ref 0
let chain_checks = ref 0

let report_failure tag policy_name cmd tree after =
  incr failures;
  Printf.printf "FAIL [%s]: %s on %s -> %s (policy: %s)\n"
    tag
    (Command.to_string cmd)
    (Term.to_string tree)
    (Term.to_string after)
    policy_name

let apply_rules rules tree =
  List.fold_left (fun t r -> Rewrite.apply r t) tree rules

let check_cmd policy policy_name cmd tree =
  let rules = Policy.compile policy cmd tree in
  let after = apply_rules rules tree in
  incr checks;
  if not (Policy.predicate policy cmd tree after) then
    report_failure "single" policy_name cmd tree after

let check_move_reduced policy policy_name tree =
  let dirs = Command.[Left; Right; Up; Down] in
  List.iter (fun dir ->
    check_cmd policy policy_name (Command.Move (0, dir)) tree
  ) dirs

let check_split_close policy policy_name tree k =
  let dirs = Command.[Left; Right; Up; Down] in
  for tile = 0 to k - 1 do
    List.iter (fun dir ->
      check_cmd policy policy_name (Command.Split (tile, dir)) tree
    ) dirs;
    if k > 1 then
      check_cmd policy policy_name (Command.Close tile) tree
  done

(* Chain checking: repeatedly apply the same move command.
   Verify monotonicity (extent never decreases) and termination (fixpoint). *)
let check_chain policy policy_name max_steps tree dir =
  let wall : Path.wall = match dir with
    | Command.Left -> `Left | Command.Right -> `Right
    | Command.Up -> `Top | Command.Down -> `Bottom
  in
  let cmd = Command.Move (0, dir) in
  let rec go step prev prev_depth =
    if step > max_steps then ()
    else
      let rules = Policy.compile policy cmd prev in
      let after = apply_rules rules prev in
      incr chain_checks;
      if after = prev then () (* fixpoint, done *)
      else begin
        if not (Policy.predicate policy cmd prev after) then
          report_failure "chain-pred" policy_name cmd prev after;
        let path_after = Command.find_path 0 after in
        let depth_after =
          if Path.touches_wall wall path_after then
            Path.perp_depth wall path_after
          else max_int (* not touching = worse than any depth *)
        in
        if depth_after > prev_depth then
          report_failure "chain-mono" policy_name cmd prev after;
        go (step + 1) after depth_after
      end
  in
  let path0 = Command.find_path 0 tree in
  let depth0 =
    if Path.touches_wall wall path0 then
      Path.perp_depth wall path0
    else max_int
  in
  go 1 tree depth0

let check_chains_reduced policy policy_name max_steps tree =
  let dirs = Command.[Left; Right; Up; Down] in
  List.iter (fun dir ->
    check_chain policy policy_name max_steps tree dir
  ) dirs

(* Chain length budget: long chains for small k, short for large k.
   Total work per tree ≈ max_steps × 4 dirs × compile cost. *)
let chain_budget k =
  if k <= 4 then k        (* full depth: converge to fixpoint *)
  else if k <= 6 then 3
  else if k <= 8 then 2
  else 1

let () =
  let max_k = ref 10 in
  let policy_name = ref "territorial" in
  let brute_k = ref 6 in
  let args = Array.to_list Sys.argv |> List.tl in
  let rec parse_args = function
    | "--max-leaves" :: v :: rest ->
      max_k := int_of_string v; parse_args rest
    | "--policy" :: v :: rest ->
      policy_name := v; parse_args rest
    | "--brute" :: v :: rest ->
      brute_k := int_of_string v; parse_args rest
    | [] -> ()
    | arg :: _ ->
      Printf.eprintf "Unknown argument: %s\n" arg;
      exit 1
  in
  parse_args args;
  let policy = Policy.find !policy_name in
  let pname = Policy.name policy in
  Printf.printf "Model checking policy '%s' up to %d leaves \
                  (brute-force Split/Close up to %d)...\n%!"
    pname !max_k !brute_k;
  let t0 = Sys.time () in
  for k = 1 to !max_k do
    let tree_count = ref 0 in
    let budget = chain_budget k in
    (* Move: symmetry-reduced single-step + chain *)
    enumerate_reduced k (fun tree ->
      incr tree_count;
      check_move_reduced policy pname tree;
      check_chains_reduced policy pname budget tree
    );
    (* Split/Close: brute-force up to --brute *)
    if k <= !brute_k then begin
      enumerate_trees k (fun tree ->
        check_split_close policy pname tree k
      )
    end;
    let elapsed = Sys.time () -. t0 in
    Printf.printf "  k=%d: %d trees (reduced), %d single + %d chain checks, %.2fs\n%!"
      k !tree_count !checks !chain_checks elapsed
  done;
  let elapsed = Sys.time () -. t0 in
  Printf.printf "Done: %d single + %d chain checks, %d failures, %.2fs\n"
    !checks !chain_checks !failures elapsed;
  if !failures > 0 then exit 1
