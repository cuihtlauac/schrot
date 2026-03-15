open Nachum

(* Generate all binary tree shapes (Catalan recurrence) with k leaves *)
let rec shapes k =
  if k = 1 then [Term.Leaf 0]
  else
    let acc = ref [] in
    for i = 1 to k - 1 do
      let lefts = shapes i in
      let rights = shapes (k - i) in
      List.iter (fun l ->
        List.iter (fun r ->
          (* Use H as placeholder — split assignment replaces it *)
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

(* Assign leaf labels: permutation perm maps position -> label *)
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

(* Generate all permutations of [0..n-1] *)
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

(* Enumerate all trees with k leaves, call f on each *)
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

let failures = ref 0
let checks = ref 0

let report_failure policy_name cmd tree after =
  incr failures;
  Printf.printf "FAIL: %s on %s: %s -> %s (policy: %s)\n"
    (Command.to_string cmd)
    (Term.to_string tree)
    (Term.to_string tree)
    (Term.to_string after)
    policy_name

let check_move policy policy_name tree k =
  let dirs = Command.[Left; Right; Up; Down] in
  for tile = 0 to k - 1 do
    List.iter (fun dir ->
      let cmd = Command.Move (tile, dir) in
      let rules = Policy.compile policy cmd tree in
      let after = List.fold_left (fun t r -> Rewrite.apply r t) tree rules in
      incr checks;
      if not (Policy.predicate policy cmd tree after) then
        report_failure policy_name cmd tree after
    ) dirs
  done

let check_split policy policy_name tree k =
  let dirs = Command.[Left; Right; Up; Down] in
  for tile = 0 to k - 1 do
    List.iter (fun dir ->
      let cmd = Command.Split (tile, dir) in
      let rules = Policy.compile policy cmd tree in
      let after = List.fold_left (fun t r -> Rewrite.apply r t) tree rules in
      incr checks;
      if not (Policy.predicate policy cmd tree after) then
        report_failure policy_name cmd tree after
    ) dirs
  done

let check_close policy policy_name tree k =
  for tile = 0 to k - 1 do
    let cmd = Command.Close tile in
    let rules = Policy.compile policy cmd tree in
    let after = List.fold_left (fun t r -> Rewrite.apply r t) tree rules in
    incr checks;
    if not (Policy.predicate policy cmd tree after) then
      report_failure policy_name cmd tree after
  done

let () =
  let max_k = ref 6 in
  let policy_name = ref "positional" in
  let args = Array.to_list Sys.argv |> List.tl in
  let rec parse_args = function
    | "--max-leaves" :: v :: rest ->
      max_k := int_of_string v; parse_args rest
    | "--policy" :: v :: rest ->
      policy_name := v; parse_args rest
    | [] -> ()
    | arg :: _ ->
      Printf.eprintf "Unknown argument: %s\n" arg;
      exit 1
  in
  parse_args args;
  let policy = Policy.find !policy_name in
  let pname = Policy.name policy in
  Printf.printf "Model checking policy '%s' up to %d leaves...\n%!" pname !max_k;
  let t0 = Sys.time () in
  for k = 1 to !max_k do
    let tree_count = ref 0 in
    enumerate_trees k (fun tree ->
      incr tree_count;
      check_move policy pname tree k;
      check_split policy pname tree k;
      if k > 1 then check_close policy pname tree k
    );
    Printf.printf "  k=%d: %d trees checked\n%!" k !tree_count
  done;
  let elapsed = Sys.time () -. t0 in
  Printf.printf "Done: %d checks, %d failures, %.2fs\n" !checks !failures elapsed;
  if !failures > 0 then exit 1
