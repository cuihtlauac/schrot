module type S = sig
  val name : string
  val compile : Command.t -> Term.t -> Rewrite.rule list
  val predicate : Command.t -> Term.t -> Term.t -> bool
end

type t = (module S)

let name (module P : S) = P.name
let compile (module P : S) = P.compile
let predicate (module P : S) = P.predicate

(* Helpers *)

let leaf_set term =
  let rec go acc = function
    | Term.Leaf n -> n :: acc
    | Term.H (a, b) | Term.V (a, b) -> go (go acc a) b
  in
  List.sort compare (go [] term)

let leaf_count term = List.length (leaf_set term)

let apply_rules rules term =
  List.fold_left (fun t r -> Rewrite.apply r t) term rules

let dir_to_wall : Command.dir -> Path.wall = function
  | Command.Left -> `Left
  | Command.Right -> `Right
  | Command.Up -> `Top
  | Command.Down -> `Bottom

(* Positional policy: wraps existing Command.compile *)
module Positional : S = struct
  let name = "positional"

  let compile = Command.compile

  let predicate cmd before after =
    match cmd with
    | Command.Move (n, dir) ->
      let rules = compile cmd before in
      if rules = [] then
        before = after
      else
        let path_before = Command.find_path n before in
        let path_after = Command.find_path n after in
        Path.center_moved dir path_before path_after
    | Command.Split (n, _) ->
      leaf_count after = leaf_count before + 1
      && List.mem n (leaf_set after)
    | Command.Close n ->
      leaf_count after = leaf_count before - 1
      && not (List.mem n (leaf_set after))
end

(* Dominance cascade: positional first, then rotate for edge extent *)
module Dominance : S = struct
  let name = "dominance"

  let compile cmd term =
    match cmd with
    | Command.Move (n, dir) ->
      let pos_rules = Command.compile cmd term in
      let pos_ok = pos_rules <> [] &&
        let after = apply_rules pos_rules term in
        Positional.predicate cmd term after
      in
      if pos_ok then pos_rules
      else
        let wall = dir_to_wall dir in
        let path_before = Command.find_path n term in
        (* Try Rotate alone *)
        let rotate_rules = [Rewrite.Rotate n] in
        let after_rotate = apply_rules rotate_rules term in
        if after_rotate <> term then
          let path_after = Command.find_path n after_rotate in
          if Path.extent_increased wall path_before path_after then rotate_rules
          else
            (* Try Rotate + Swap *)
            let rs_rules = [Rewrite.Rotate n; Rewrite.Swap n] in
            let after_rs = apply_rules rs_rules term in
            if after_rs <> term then
              let path_after_rs = Command.find_path n after_rs in
              if Path.extent_increased wall path_before path_after_rs then rs_rules
              else []
            else []
        else []
    | _ -> Command.compile cmd term

  let predicate cmd before after =
    match cmd with
    | Command.Move (n, dir) ->
      let rules = compile cmd before in
      if rules = [] then
        before = after
      else
        let path_before = Command.find_path n before in
        let path_after = Command.find_path n after in
        let center_moved = Path.center_moved dir path_before path_after in
        let wall = dir_to_wall dir in
        let extent_grew = Path.extent_increased wall path_before path_after in
        center_moved || extent_grew
    | Command.Split (n, _) ->
      leaf_count after = leaf_count before + 1
      && List.mem n (leaf_set after)
    | Command.Close n ->
      leaf_count after = leaf_count before - 1
      && not (List.mem n (leaf_set after))
end

(* Territorial: one-bite-at-a-time edge growth.
   Each move removes at most one perpendicular constraint from the tile's path,
   growing extent from (1/2)^n to (1/2)^(n-1). Compensating transposes
   preserve aspect ratios of non-target tiles. *)
module Territorial : S = struct
  let name = "territorial"

  (* Compute perp_depth after applying candidate, or None if extent didn't increase *)
  let extent_after dir n term after =
    let wall = dir_to_wall dir in
    let path_before = Command.find_path n term in
    let path_after = Command.find_path n after in
    if Path.extent_increased wall path_before path_after then
      Some (Path.perp_depth wall path_after)
    else
      None

  (* Try appending Transpose(m) for each non-target leaf m.
     Only accept if extent still increased and perp_depth_after matches target.
     Return the variant with lowest aspect distortion, or base if none helps. *)
  let with_best_compensation dir n target_depth base_rules term =
    let base_after = apply_rules base_rules term in
    let base_cost = Path.aspect_distortion ~skip:n term base_after in
    let leaves = leaf_set base_after in
    List.fold_left (fun (best_rules, best_cost) m ->
      if m = n then (best_rules, best_cost)
      else
        let candidate = base_rules @ [Rewrite.Transpose m] in
        let after = apply_rules candidate term in
        match extent_after dir n term after with
        | Some d when d >= target_depth ->
          let cost = Path.aspect_distortion ~skip:n term after in
          if cost < best_cost then (candidate, cost)
          else (best_rules, best_cost)
        | _ -> (best_rules, best_cost)
    ) (base_rules, base_cost) leaves
    |> fst

  let compile cmd term =
    match cmd with
    | Command.Move (n, dir) ->
      (* 1. Positional *)
      let pos_rules = Command.compile cmd term in
      let pos_ok = pos_rules <> [] &&
        let after = apply_rules pos_rules term in
        Positional.predicate cmd term after
      in
      if pos_ok then pos_rules
      else
        (* 2. Try base candidates for edge extent *)
        let base_candidates = [
          [Rewrite.Rotate n];
          [Rewrite.Rotate n; Rewrite.Swap n];
          [Rewrite.Transpose n];
          [Rewrite.Transpose n; Rewrite.Swap n];
          [Rewrite.Rotate n; Rewrite.Swap n; Rewrite.Transpose n];
          [Rewrite.Rotate n; Rewrite.Transpose n];
        ] in
        (* Score each candidate: (rules, perp_depth_after) *)
        let scored = List.filter_map (fun c ->
          let after = apply_rules c term in
          if after = term then None
          else match extent_after dir n term after with
            | Some depth -> Some (c, depth)
            | None -> None
        ) base_candidates in
        (match scored with
        | [] -> []
        | _ ->
          (* One bite: pick highest perp_depth (= smallest extent increase) *)
          let max_depth = List.fold_left (fun acc (_, d) -> max acc d) 0 scored in
          let minimal = List.filter (fun (_, d) -> d = max_depth) scored in
          (* Among minimal-growth candidates, pick best aspect after compensation *)
          let compensated = List.map (fun (c, _) ->
            let comp = with_best_compensation dir n max_depth c term in
            let after = apply_rules comp term in
            (comp, Path.aspect_distortion ~skip:n term after)
          ) minimal in
          let best = List.fold_left (fun (br, bc) (r, c) ->
            if c < bc then (r, c) else (br, bc)
          ) (List.hd compensated) (List.tl compensated) in
          fst best)
    | Command.Split _ | Command.Close _ -> Command.compile cmd term

  let predicate cmd before after =
    match cmd with
    | Command.Move (n, dir) ->
      let rules = compile cmd before in
      if rules = [] then
        before = after
      else
        let path_before = Command.find_path n before in
        let path_after = Command.find_path n after in
        let center_moved = Path.center_moved dir path_before path_after in
        let wall = dir_to_wall dir in
        let extent_grew = Path.extent_increased wall path_before path_after in
        center_moved || extent_grew
    | Command.Split (n, _) ->
      leaf_count after = leaf_count before + 1
      && List.mem n (leaf_set after)
    | Command.Close n ->
      leaf_count after = leaf_count before - 1
      && not (List.mem n (leaf_set after))
end

let positional : t = (module Positional)
let dominance : t = (module Dominance)
let territorial : t = (module Territorial)

let all = [positional; dominance; territorial]

let find s =
  match List.find_opt (fun p -> name p = s) all with
  | Some p -> p
  | None -> failwith (Printf.sprintf "Policy.find: unknown policy '%s'" s)
