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
        let rects_before = Geometry.interpret before in
        let rects_after = Geometry.interpret after in
        (match List.assoc_opt n rects_before, List.assoc_opt n rects_after with
         | Some rb, Some ra ->
           (match dir with
            | Left  -> Geometry.center_x ra < Geometry.center_x rb -. 1e-9
            | Right -> Geometry.center_x ra > Geometry.center_x rb +. 1e-9
            | Up    -> Geometry.center_y ra < Geometry.center_y rb -. 1e-9
            | Down  -> Geometry.center_y ra > Geometry.center_y rb +. 1e-9)
         | _ -> false)
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
        let rects_before = Geometry.interpret before in
        let rects_after = Geometry.interpret after in
        (match List.assoc_opt n rects_before, List.assoc_opt n rects_after with
         | Some rb, Some ra ->
           (* Center moved OR edge extent increased *)
           let center_moved = match dir with
             | Left  -> Geometry.center_x ra < Geometry.center_x rb -. 1e-9
             | Right -> Geometry.center_x ra > Geometry.center_x rb +. 1e-9
             | Up    -> Geometry.center_y ra < Geometry.center_y rb -. 1e-9
             | Down  -> Geometry.center_y ra > Geometry.center_y rb +. 1e-9
           in
           let edge = dir_to_wall dir in
           let ext_before = Geometry.edge_extent edge n before in
           let ext_after = Geometry.edge_extent edge n after in
           center_moved || ext_after > ext_before +. 1e-9
         | _ -> false)
    | Command.Split (n, _) ->
      leaf_count after = leaf_count before + 1
      && List.mem n (leaf_set after)
    | Command.Close n ->
      leaf_count after = leaf_count before - 1
      && not (List.mem n (leaf_set after))
end

(* Territorial: dominance cascade + transpose for edge extent,
   with compensating transposes to minimize aspect ratio distortion. *)
module Territorial : S = struct
  let name = "territorial"

  let extent_ok dir n term after =
    let wall = dir_to_wall dir in
    let path_before = Command.find_path n term in
    let path_after = Command.find_path n after in
    Path.extent_increased wall path_before path_after

  (* Try appending Transpose(m) for each non-target leaf m.
     Only accept if edge extent is still increased.
     Return the variant with lowest aspect distortion, or base if none helps. *)
  let with_best_compensation dir n base_rules term =
    let base_after = apply_rules base_rules term in
    let base_cost = Path.aspect_distortion ~skip:n term base_after in
    let leaves = leaf_set base_after in
    List.fold_left (fun (best_rules, best_cost) m ->
      if m = n then (best_rules, best_cost)
      else
        let candidate = base_rules @ [Rewrite.Transpose m] in
        let after = apply_rules candidate term in
        if not (extent_ok dir n term after) then (best_rules, best_cost)
        else
          let cost = Path.aspect_distortion ~skip:n term after in
          if cost < best_cost then (candidate, cost)
          else (best_rules, best_cost)
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
        let valid = List.filter_map (fun c ->
          let after = apply_rules c term in
          if after <> term && extent_ok dir n term after then Some c
          else None
        ) base_candidates in
        (match valid with
        | [] -> []
        | _ ->
          (* Pick the base with best aspect distortion after compensation *)
          let scored = List.map (fun c ->
            let compensated = with_best_compensation dir n c term in
            let after = apply_rules compensated term in
            (compensated, Path.aspect_distortion ~skip:n term after)
          ) valid in
          let best = List.fold_left (fun (br, bc) (r, c) ->
            if c < bc then (r, c) else (br, bc)
          ) (List.hd scored) (List.tl scored) in
          fst best)
    | Command.Split _ | Command.Close _ -> Command.compile cmd term

  let predicate cmd before after =
    match cmd with
    | Command.Move (n, dir) ->
      let rules = compile cmd before in
      if rules = [] then
        before = after
      else
        let rects_before = Geometry.interpret before in
        let rects_after = Geometry.interpret after in
        (match List.assoc_opt n rects_before, List.assoc_opt n rects_after with
         | Some rb, Some ra ->
           let center_moved = match dir with
             | Left  -> Geometry.center_x ra < Geometry.center_x rb -. 1e-9
             | Right -> Geometry.center_x ra > Geometry.center_x rb +. 1e-9
             | Up    -> Geometry.center_y ra < Geometry.center_y rb -. 1e-9
             | Down  -> Geometry.center_y ra > Geometry.center_y rb +. 1e-9
           in
           let edge = dir_to_wall dir in
           let ext_before = Geometry.edge_extent edge n before in
           let ext_after = Geometry.edge_extent edge n after in
           center_moved || ext_after > ext_before +. 1e-9
         | _ -> false)
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
