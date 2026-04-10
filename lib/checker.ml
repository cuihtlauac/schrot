type enum_mode =
  | Orbit_representatives
  | All_tilings
  | Progressive

type counter_policy =
  | Stop_at_first
  | Collect_all

(* --- Property --- *)

type property_kind =
  | Atomic of (Tiling.t -> bool)
  | Conj of property list
  | Disj of property list
  | Imply of property * property
  | Neg of property
  | For_all_tiles of (int -> property)
  | For_all_dirs of (Tiling.arrow -> property)

and property = {
  label : string;
  kind : property_kind;
}

let property label pred = { label; kind = Atomic pred }
let conj ps = { label = "conj"; kind = Conj ps }
let disj ps = { label = "disj"; kind = Disj ps }
let imply pre post = { label = "imply"; kind = Imply (pre, post) }
let neg p = { label = "not(" ^ p.label ^ ")"; kind = Neg p }
let for_all_tiles f = { label = "for_all_tiles"; kind = For_all_tiles f }
let for_all_dirs f = { label = "for_all_dirs"; kind = For_all_dirs f }

type eval_result = Pass | Fail of string | Skip

let rec eval t p =
  match p.kind with
  | Atomic pred ->
    if pred t then Pass else Fail p.label
  | Conj ps ->
    eval_conj t ps
  | Disj ps ->
    eval_disj t ps
  | Imply (pre, post) ->
    (match eval t pre with
     | Pass -> eval t post
     | Fail _ | Skip -> Skip)
  | Neg inner ->
    (match eval t inner with
     | Pass -> Fail p.label
     | Fail _ -> Pass
     | Skip -> Skip)
  | For_all_tiles f ->
    let n = Tiling.size t in
    eval_for_all t 0 n (fun i ->
      let sub = f i in
      { sub with label = Printf.sprintf "for_all_tiles(%d): %s" i sub.label })
  | For_all_dirs f ->
    let dirs : Tiling.arrow list = [Left; Right; Up; Down] in
    let dir_name (d : Tiling.arrow) = match d with
      | Left -> "Left" | Right -> "Right"
      | Up -> "Up" | Down -> "Down"
    in
    eval_for_all_list t dirs (fun d ->
      let sub = f d in
      { sub with label = Printf.sprintf "for_all_dirs(%s): %s" (dir_name d) sub.label })

and eval_conj t = function
  | [] -> Pass
  | p :: rest ->
    match eval t p with
    | Fail _ as f -> f
    | Skip -> Skip
    | Pass -> eval_conj t rest

and eval_disj t = function
  | [] -> Fail "disj(empty)"
  | p :: rest ->
    match eval t p with
    | Pass -> Pass
    | Skip -> eval_disj t rest
    | Fail _ ->
      match eval_disj t rest with
      | Pass -> Pass
      | Skip -> Skip
      | Fail _ -> Fail p.label

and eval_for_all t i n f =
  if i >= n then Pass
  else
    match eval t (f i) with
    | Fail _ as fl -> fl
    | Skip -> Skip
    | Pass -> eval_for_all t (i + 1) n f

and eval_for_all_list : 'a. Tiling.t -> 'a list -> ('a -> property) -> eval_result =
  fun t xs f ->
  match xs with
  | [] -> Pass
  | x :: rest ->
    match eval t (f x) with
    | Fail _ as fl -> fl
    | Skip -> Skip
    | Pass -> eval_for_all_list t rest f

(* --- Results --- *)

type counterexample = {
  tiling : Tiling.t;
  size : int;
  label : string;
}

type size_stats = {
  n : int;
  checked : int;
  passed : int;
  failed : int;
  skipped : int;
  elapsed : float;
}

type result = {
  property_label : string;
  enum_mode_used : enum_mode;
  counterexamples : counterexample list;
  per_size : size_stats list;
  total_checked : int;
  total_passed : int;
  total_failed : int;
  total_skipped : int;
  elapsed : float;
}

let passed r = r.total_failed = 0

(* --- Enumeration --- *)

let enumerate_size n mode =
  let all = Schrot.enum n |> List.map Tiling.relabel in
  match mode with
  | All_tilings | Progressive -> all
  | Orbit_representatives ->
    let seen = Hashtbl.create 64 in
    List.filter (fun t ->
      let key = Tiling.canonical_d4 t in
      if Hashtbl.mem seen key then false
      else (Hashtbl.add seen key (); true)
    ) all

exception Stop_early of counterexample

let check_size n mode policy prop =
  let tilings = enumerate_size n mode in
  let t0 = Sys.time () in
  let checked = ref 0 in
  let pass = ref 0 in
  let fail = ref 0 in
  let skip = ref 0 in
  let cxs = ref [] in
  (try
     List.iter (fun t ->
       incr checked;
       begin match eval t prop with
       | Pass -> incr pass
       | Skip -> incr skip
       | Fail lbl ->
         incr fail;
         let cx = { tiling = t; size = n; label = lbl } in
         cxs := cx :: !cxs;
         (match policy with
          | Stop_at_first -> raise (Stop_early cx)
          | Collect_all -> ())
       end
     ) tilings
   with Stop_early _ -> ());
  let elapsed = Sys.time () -. t0 in
  let stats = {
    n; checked = !checked; passed = !pass;
    failed = !fail; skipped = !skip; elapsed;
  } in
  (stats, List.rev !cxs)

let make_result label mode per_size counterexamples t0 =
  let total f = List.fold_left (fun acc s -> acc + f s) 0 per_size in
  {
    property_label = label;
    enum_mode_used = mode;
    counterexamples;
    per_size;
    total_checked = total (fun s -> s.checked);
    total_passed = total (fun s -> s.passed);
    total_failed = total (fun s -> s.failed);
    total_skipped = total (fun s -> s.skipped);
    elapsed = Sys.time () -. t0;
  }

let check ?(mode = Progressive) ?(policy = Stop_at_first)
    ?on_progress ~max_tiles (p : property) =
  let t0 = Sys.time () in
  let label = p.label in
  let run enum_mode =
    let per_size = ref [] in
    let all_cxs = ref [] in
    let stopped = ref false in
    for n = 1 to max_tiles do
      if not !stopped then begin
        let stats, cxs = check_size n enum_mode policy p in
        per_size := stats :: !per_size;
        all_cxs := !all_cxs @ cxs;
        (match on_progress with Some f -> f stats | None -> ());
        if cxs <> [] && policy = Stop_at_first then
          stopped := true
      end
    done;
    (List.rev !per_size, !all_cxs)
  in
  match mode with
  | All_tilings ->
    let per_size, cxs = run All_tilings in
    make_result label All_tilings per_size cxs t0
  | Orbit_representatives ->
    let per_size, cxs = run Orbit_representatives in
    make_result label Orbit_representatives per_size cxs t0
  | Progressive ->
    let per_size, cxs = run Orbit_representatives in
    if cxs <> [] then
      make_result label Orbit_representatives per_size cxs t0
    else
      let per_size, cxs = run All_tilings in
      make_result label All_tilings per_size cxs t0

(* --- Witness finding --- *)

let find_witness ?(max_tiles = 10) prop =
  let exception Found of Tiling.t in
  try
    for n = 1 to max_tiles do
      let tilings = enumerate_size n All_tilings in
      List.iter (fun t ->
        match eval t prop with
        | Pass -> raise (Found t)
        | Fail _ | Skip -> ()
      ) tilings
    done;
    None
  with Found t -> Some t

let find_all_witnesses ?(max_tiles = 10) prop =
  let acc = ref [] in
  for n = 1 to max_tiles do
    let tilings = enumerate_size n All_tilings in
    List.iter (fun t ->
      match eval t prop with
      | Pass -> acc := t :: !acc
      | Fail _ | Skip -> ()
    ) tilings
  done;
  List.rev !acc

(* --- Shrinking --- *)

let shrink prop cx =
  let rec go cx =
    if cx.size <= 1 then cx
    else
      let candidates = List.init cx.size Fun.id in
      let smaller =
        List.filter_map (fun k ->
          let t' = Tiling.close k cx.tiling in
          let t' = Tiling.relabel t' in
          let sz = Tiling.size t' in
          if sz < cx.size then
            match eval t' prop with
            | Fail lbl -> Some { tiling = t'; size = sz; label = lbl }
            | Pass | Skip -> None
          else None
        ) candidates
      in
      match smaller with
      | [] -> cx
      | cxs ->
        let best = List.fold_left
            (fun a b -> if b.size < a.size then b else a)
            (List.hd cxs) (List.tl cxs) in
        go best
  in
  go cx

(* --- Formatting --- *)

let pp_counterexample fmt cx =
  Format.fprintf fmt "%s  [%s]" (Tiling.to_string cx.tiling) cx.label

let pp_result fmt r =
  Format.fprintf fmt "Property: %s@." r.property_label;
  Format.fprintf fmt "Mode: %s@."
    (match r.enum_mode_used with
     | Orbit_representatives -> "orbit_representatives"
     | All_tilings -> "all_tilings"
     | Progressive -> "progressive");
  Format.fprintf fmt "@[<v 0>";
  Format.fprintf fmt "  n  checked  passed  failed  skipped  elapsed@.";
  List.iter (fun s ->
    Format.fprintf fmt "%3d  %7d  %6d  %6d  %7d  %.3fs@."
      s.n s.checked s.passed s.failed s.skipped s.elapsed
  ) r.per_size;
  Format.fprintf fmt "@]";
  Format.fprintf fmt "Total: %d checked, %d passed, %d failed, %d skipped, %.3fs@."
    r.total_checked r.total_passed r.total_failed r.total_skipped r.elapsed;
  if r.counterexamples <> [] then begin
    Format.fprintf fmt "Counterexamples:@.";
    List.iter (fun cx ->
      Format.fprintf fmt "  %a@." pp_counterexample cx
    ) r.counterexamples
  end
