(* TODO: Bring up to Schroder tilings *)

(* --- Tree enumeration (shared with model_check.ml) --- *)

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

(* --- State table --- *)

let build_states k =
  let tbl = Hashtbl.create 1024 in
  let states = ref [] in
  let next_id = ref 0 in
  let all_shapes = shapes k in
  let perms = permutations k in
  List.iter (fun shape ->
    let n_internal = count_internal shape in
    let n_splits = 1 lsl n_internal in
    for mask = 0 to n_splits - 1 do
      let split_tree = assign_splits shape mask in
      List.iter (fun perm ->
        let tree = assign_labels split_tree perm in
        if not (Hashtbl.mem tbl tree) then begin
          Hashtbl.add tbl tree !next_id;
          states := tree :: !states;
          incr next_id
        end
      ) perms
    done
  ) all_shapes;
  let n = !next_id in
  let arr = Array.make n (Term.Leaf 0) in
  List.iteri (fun i t -> arr.(n - 1 - i) <- t) !states;
  (tbl, arr)

(* --- Move successors --- *)

let apply_rules rules tree =
  List.fold_left (fun t r -> Rewrite.apply r t) tree rules

let successors policy k tree =
  let dirs = [Path.Left; Path.Right; Path.Up; Path.Down] in
  let results = ref [] in
  for n = 0 to k - 1 do
    List.iter (fun dir ->
      let rules = Policy.compile policy (Command.Move (n, dir)) tree in
      if rules <> [] then begin
        let after = apply_rules rules tree in
        if after <> tree then
          results := after :: !results
      end
    ) dirs
  done;
  !results

(* --- BFS --- *)

let[@warning "-32"] bfs adj n start =
  let visited = Array.make n false in
  let queue = Queue.create () in
  visited.(start) <- true;
  Queue.push start queue;
  let count = ref 1 in
  while not (Queue.is_empty queue) do
    let u = Queue.pop queue in
    Array.iter (fun v ->
      if not visited.(v) then begin
        visited.(v) <- true;
        incr count;
        Queue.push v queue
      end
    ) adj.(u)
  done;
  (visited, !count)

(* --- Kosaraju's SCC --- *)

let kosaraju fwd rev n =
  (* Pass 1: DFS on forward graph, record finish order *)
  let visited = Array.make n false in
  let order = Array.make n 0 in
  let pos = ref 0 in
  let rec dfs1 u =
    visited.(u) <- true;
    Array.iter (fun v ->
      if not visited.(v) then dfs1 v
    ) fwd.(u);
    order.(!pos) <- u;
    incr pos
  in
  for i = 0 to n - 1 do
    if not visited.(i) then dfs1 i
  done;
  (* Pass 2: DFS on reverse graph in reverse finish order *)
  let comp = Array.make n (-1) in
  let next_comp = ref 0 in
  let rec dfs2 u c =
    comp.(u) <- c;
    Array.iter (fun v ->
      if comp.(v) = -1 then dfs2 v c
    ) rev.(u)
  in
  for i = n - 1 downto 0 do
    let u = order.(i) in
    if comp.(u) = -1 then begin
      dfs2 u !next_comp;
      incr next_comp
    end
  done;
  (comp, !next_comp)

(* --- Split skeleton: tree shape with H/V labels, leaves replaced by () --- *)

type skeleton = SLeaf | SH of skeleton * skeleton | SV of skeleton * skeleton

let rec skeleton_of = function
  | Term.Leaf _ -> SLeaf
  | Term.H (a, b) -> SH (skeleton_of a, skeleton_of b)
  | Term.V (a, b) -> SV (skeleton_of a, skeleton_of b)

let[@warning "-32"] rec skeleton_to_string = function
  | SLeaf -> "_"
  | SH (a, b) -> "h(" ^ skeleton_to_string a ^ ", " ^ skeleton_to_string b ^ ")"
  | SV (a, b) -> "v(" ^ skeleton_to_string a ^ ", " ^ skeleton_to_string b ^ ")"

(* --- Main --- *)

let () =
  let max_k = ref 5 in
  let policy_name = ref "all" in
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
  let policies =
    if !policy_name = "all" then Policy.all
    else [Policy.find !policy_name]
  in
  for k = 2 to !max_k do
    let t0 = Sys.time () in
    Printf.printf "k=%d: enumerating states...%!" k;
    let (tbl, states) = build_states k in
    let n = Array.length states in
    Printf.printf " %d states\n%!" n;
    List.iter (fun policy ->
      let pname = Policy.name policy in
      Printf.printf "  %s: building graph...%!" pname;
      let fwd = Array.make n [||] in
      let rev_edges = Array.make n [] in
      let edge_count = ref 0 in
      Array.iteri (fun u tree ->
        let succs = successors policy k tree in
        let ids = List.filter_map (fun s ->
          match Hashtbl.find_opt tbl s with
          | Some v when v <> u -> Some v
          | _ -> None
        ) succs in
        (* Deduplicate *)
        let ids = List.sort_uniq compare ids in
        fwd.(u) <- Array.of_list ids;
        edge_count := !edge_count + List.length ids;
        List.iter (fun v ->
          rev_edges.(v) <- u :: rev_edges.(v)
        ) ids
      ) states;
      let rev = Array.map (fun es -> Array.of_list (List.sort_uniq compare es)) rev_edges in
      Printf.printf " %d edges\n%!" !edge_count;
      Printf.printf "  %s: computing SCCs...%!" pname;
      let (comp, n_scc) = kosaraju fwd rev n in
      if n_scc = 1 then
        Printf.printf " 1 SCC — universally reachable\n%!"
      else begin
        (* Compute SCC sizes *)
        let sizes = Array.make n_scc 0 in
        Array.iter (fun c -> sizes.(c) <- sizes.(c) + 1) comp;
        let sorted_sizes = Array.copy sizes in
        Array.sort (fun a b -> compare b a) sorted_sizes;
        let max_size = sorted_sizes.(0) in
        let min_size = sorted_sizes.(n_scc - 1) in
        Printf.printf " %d SCCs (largest: %d, smallest: %d)\n%!" n_scc max_size min_size;
        (* Check if split skeleton is the invariant *)
        let skel_matches = ref true in
        let skel_tbl = Hashtbl.create 64 in
        Array.iteri (fun u _tree ->
          let skel = skeleton_of states.(u) in
          match Hashtbl.find_opt skel_tbl skel with
          | None -> Hashtbl.add skel_tbl skel comp.(u)
          | Some c -> if c <> comp.(u) then skel_matches := false
        ) states;
        let n_skels = Hashtbl.length skel_tbl in
        if !skel_matches && n_skels = n_scc then
          Printf.printf "  %s: SCC invariant = split skeleton (%d distinct skeletons)\n%!" pname n_skels
        else
          Printf.printf "  %s: split skeleton does NOT explain SCCs (%d skeletons vs %d SCCs)\n%!" pname n_skels n_scc;
        (* Show a few example SCCs *)
        let shown = Hashtbl.create 8 in
        let examples = ref 0 in
        Array.iteri (fun u _tree ->
          let c = comp.(u) in
          if not (Hashtbl.mem shown c) && !examples < 5 then begin
            Hashtbl.add shown c true;
            incr examples;
            Printf.printf "    SCC %d (size %d): e.g. %s\n" c sizes.(c) (Term.to_string states.(u))
          end
        ) states
      end;
      let elapsed = Sys.time () -. t0 in
      Printf.printf "  %s: %.2fs\n%!" pname elapsed
    ) policies
  done
