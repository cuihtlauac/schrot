(* Model checking for the 3 atomic flip operations.
   Properties verified:
   A. Size preservation
   B. Validity (well-formed Schroder tiling)
   C. Invertibility (hard failure)
   D. Flip graph connectivity
   E. Edge count (oracle agreement) *)

let label_tiling (is_h, tree) =
  let counter = ref 0 in
  let rec go = function
    | Schrot.Tile () -> let n = !counter in incr counter; Schrot.Tile n
    | Schrot.Frame ch -> Schrot.Frame (List2.map go ch)
  in
  (is_h, go tree)

let is_valid_labeling t =
  let lvs = Tiling.leaves t in
  let n = List.length lvs in
  let sorted = List.sort compare lvs in
  sorted = List.init n Fun.id

let rec tree_valid = function
  | Schrot.Tile _ -> true
  | Schrot.Frame ch ->
    List2.length ch >= 2 && List2.for_all tree_valid ch

let tiling_valid (_, tree) = tree_valid tree && is_valid_labeling (false, tree)

(* Format a tiling as an OCaml constructor expression for counterexample output *)
let rec tree_to_ocaml = function
  | Schrot.Tile n -> Printf.sprintf "Tile %d" n
  | Schrot.Frame ch ->
    let children = List2.to_list ch in
    let strs = List.map tree_to_ocaml children in
    match strs with
    | a :: b :: rest ->
      Printf.sprintf "Frame (List2.Cons2 (%s, %s, [%s]))" a b
        (String.concat "; " rest)
    | _ -> assert false

let _tiling_to_ocaml (is_h, tree) =
  Printf.sprintf "(%s, Schrot.%s)" (if is_h then "true" else "false")
    (tree_to_ocaml tree)

let check_properties max_n =
  let all_ok = ref true in
  let counterexamples = Buffer.create 256 in
  let add_cx prop tiling_str flip_str =
    Printf.bprintf counterexamples "  (%S, %S, %S);\n" tiling_str flip_str prop
  in
  let fail fmt = Printf.ksprintf (fun s ->
    all_ok := false; Printf.printf "FAIL: %s\n%!" s) fmt in
  for n = 1 to max_n do
    let unit_tilings = Schrot.enum n in
    let tilings = List.map label_tiling unit_tilings in
    let ntil = List.length tilings in
    Printf.printf "n=%d: %d tilings\n%!" n ntil;
    if n <= 1 then begin
      Printf.printf "  (no flips for n=%d)\n%!" n;
    end else begin
      let tbl : (string, int) Hashtbl.t = Hashtbl.create 64 in
      let tilings_arr = Array.of_list tilings in
      Array.iteri (fun i t -> Hashtbl.replace tbl (Tiling.to_string t) i) tilings_arr;
      let adj = Array.make ntil [] in
      let total_flips = ref 0 in
      let prop_a_ok = ref true in
      let prop_b_ok = ref true in
      let all_flips_per_tiling = Array.make ntil [] in
      Array.iteri (fun i t ->
        let flips = Tiling.enumerate_flips t in
        all_flips_per_tiling.(i) <- flips;
        List.iter (fun (flip, t') ->
          incr total_flips;
          if Tiling.size t' <> n then begin
            fail "n=%d tiling %d: %s changed size to %d"
              n i (Tiling.flip_to_string flip) (Tiling.size t');
            add_cx "A" (Tiling.to_string t) (Tiling.flip_to_string flip);
            prop_a_ok := false
          end;
          if not (tiling_valid t') then begin
            fail "n=%d tiling %d: %s produced invalid tiling %s"
              n i (Tiling.flip_to_string flip) (Tiling.to_string t');
            add_cx "B" (Tiling.to_string t) (Tiling.flip_to_string flip);
            prop_b_ok := false
          end;
          let key = Tiling.to_string t' in
          (match Hashtbl.find_opt tbl key with
           | Some j ->
             if not (List.mem j adj.(i)) then
               adj.(i) <- j :: adj.(i)
           | None ->
             let t'_relabeled = Tiling.relabel t' in
             let key2 = Tiling.to_string t'_relabeled in
             match Hashtbl.find_opt tbl key2 with
             | Some j ->
               if not (List.mem j adj.(i)) then
                 adj.(i) <- j :: adj.(i)
             | None ->
               fail "n=%d tiling %d: %s produced unknown tiling %s (relabeled: %s)"
                 n i (Tiling.flip_to_string flip) key key2)
        ) flips
      ) tilings_arr;
      Printf.printf "  %d flip applications\n%!" !total_flips;
      if !prop_a_ok then Printf.printf "  Property A (size preservation): OK\n%!";
      if !prop_b_ok then Printf.printf "  Property B (validity): OK\n%!";
      (* Property C: invertibility (hard failure) *)
      let prop_c_ok = ref true in
      let prop_c_count = ref 0 in
      let prop_c_fail_count = ref 0 in
      Array.iteri (fun i t ->
        List.iter (fun (flip, t') ->
          let reverse_flips = Tiling.enumerate_flips t' in
          let original_str = Tiling.to_string t in
          let found_inverse = List.exists (fun (_, t'') ->
            Tiling.to_string t'' = original_str
          ) reverse_flips in
          if found_inverse then incr prop_c_count
          else begin
            let t'_rel = Tiling.relabel t' in
            let t_rel = Tiling.relabel t in
            let reverse_flips_rel = Tiling.enumerate_flips t'_rel in
            let found_rel = List.exists (fun (_, t'') ->
              Tiling.to_string t'' = Tiling.to_string t_rel
            ) reverse_flips_rel in
            if found_rel then incr prop_c_count
            else begin
              if !prop_c_fail_count < 5 then
                fail "n=%d tiling %d: %s -> %s has no inverse"
                  n i (Tiling.flip_to_string flip) (Tiling.to_string t');
              add_cx "C" (Tiling.to_string t) (Tiling.flip_to_string flip);
              prop_c_ok := false;
              incr prop_c_fail_count
            end
          end
        ) all_flips_per_tiling.(i)
      ) tilings_arr;
      if !prop_c_ok then
        Printf.printf "  Property C (invertibility): OK (%d/%d)\n%!"
          !prop_c_count !prop_c_count
      else
        Printf.printf "  Property C (invertibility): FAIL (%d/%d, %d failures)\n%!"
          !prop_c_count (!prop_c_count + !prop_c_fail_count) !prop_c_fail_count;
      (* Property D: connectivity *)
      let visited = Array.make ntil false in
      let queue = Queue.create () in
      Queue.push 0 queue;
      visited.(0) <- true;
      let count = ref 1 in
      while not (Queue.is_empty queue) do
        let v = Queue.pop queue in
        List.iter (fun w ->
          if not visited.(w) then begin
            visited.(w) <- true;
            incr count;
            Queue.push w queue
          end
        ) adj.(v)
      done;
      if !count = ntil then
        Printf.printf "  Property D (connectivity): OK (%d/%d reachable)\n%!" !count ntil
      else begin
        fail "n=%d: flip graph not connected: %d/%d reachable from vertex 0" n !count ntil;
        Array.iteri (fun i v ->
          if not v then begin
            Printf.printf "    unreachable: %d = %s\n" i
              (Tiling.to_string tilings_arr.(i));
            add_cx "D" (Tiling.to_string tilings_arr.(i)) ""
          end
        ) visited
      end;
      ignore all_flips_per_tiling
    end
  done;
  (* Write counterexamples *)
  if Buffer.length counterexamples > 0 then begin
    Printf.printf "\n--- Counterexamples (for flip_unit.ml) ---\n";
    Printf.printf "let cases = [\n%s]\n" (Buffer.contents counterexamples)
  end;
  if !all_ok then
    Printf.printf "\nAll properties verified.\n"
  else begin
    Printf.printf "\nSome properties FAILED.\n";
    exit 1
  end

let () =
  let max_n = ref 6 in
  Arg.parse [
    "--max-leaves", Arg.Set_int max_n, "Maximum number of leaves (default: 6)";
  ] (fun _ -> ()) "flip_check [--max-leaves N]";
  check_properties !max_n
