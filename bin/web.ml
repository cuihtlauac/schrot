
type state = {
  mutable tiling : Tiling.t;
  mutable weighted : (int, float) Schrot.tiling;
  mutable segments : Tiling.segment list;
  mutable selected : int option;
  mutable selected_segment : int option;
  mutable last_command : string option;
  mutable prev_tiling : Tiling.t option;
  mutable history : (Tiling.t * int option) list;
}

let init_weighted t =
  Tiling.resolve_splits t

let state =
  let t = (false, Schrot.Tile 0) in
  let wt = init_weighted t in
  {
    tiling = t;
    weighted = wt;
    segments = Tiling.enumerate_segments wt;
    selected = Some 0;
    selected_segment = None;
    last_command = None;
    prev_tiling = None;
    history = [];
  }

let update_geometry () =
  state.weighted <- init_weighted state.tiling;
  state.segments <- Tiling.enumerate_segments state.weighted;
  state.selected_segment <- None

let update_segments () =
  state.segments <- Tiling.enumerate_segments state.weighted

let push_history () =
  state.history <- (state.tiling, state.selected) :: state.history

let escape_json_string s =
  let buf = Buffer.create (String.length s) in
  String.iter (fun c ->
    match c with
    | '"' -> Buffer.add_string buf "\\\""
    | '\\' -> Buffer.add_string buf "\\\\"
    | '\n' -> Buffer.add_string buf "\\n"
    | '\r' -> Buffer.add_string buf "\\r"
    | '\t' -> Buffer.add_string buf "\\t"
    | c -> Buffer.add_char buf c
  ) s;
  Buffer.contents buf

let json_str s = Printf.sprintf "\"%s\"" (escape_json_string s)
let json_str_opt = function None -> "null" | Some s -> json_str s
let json_int_opt = function None -> "null" | Some n -> string_of_int n

let svg_of_tiling_weighted ?selected ?selected_segment ~weighted ~segments tiling =
  let base = Svg.render_tiling_group ~x:0. ~y:0. ~width:500. ~height:400. ~margin:20.
    ?selected ~interactive:true ~weighted tiling in
  let overlays = Svg.render_segment_overlays ~x:0. ~y:0. ~width:500. ~height:400. ~margin:20.
    ?selected_segment segments tiling in
  base ^ overlays

let svg_of_tiling_small tiling =
  Svg.render_tiling_group ~x:0. ~y:0. ~width:240. ~height:192. ~margin:10. tiling

let hasse_svg_of_state () =
  let g = Geom.of_weighted ~min_overlap:1e-2 state.weighted state.tiling in
  Svg.arrow_defs ^
  Svg.render_hasse_diagram ~x:0. ~y:0. ~width:250. ~height:250. g

let state_json () =
  let svg = svg_of_tiling_weighted
    ?selected:state.selected
    ?selected_segment:state.selected_segment
    ~weighted:state.weighted ~segments:state.segments
    state.tiling in
  let hasse = hasse_svg_of_state () in
  let prev_svg = match state.prev_tiling with
    | Some t -> json_str (svg_of_tiling_small t)
    | None -> "null"
  in
  let prev_term = match state.prev_tiling with
    | Some t -> json_str (Tiling.to_string t)
    | None -> "null"
  in
  let tiles = List.sort compare (Tiling.leaves state.tiling) in
  Printf.sprintf
    {|{"svg":%s,"term":%s,"prev_svg":%s,"prev_term":%s,"tiles":[%s],"selected":%s,"selected_segment":%s,"command":%s,"n_tiles":%d,"hasse_svg":%s}|}
    (json_str svg)
    (json_str (Tiling.to_string state.tiling))
    prev_svg
    prev_term
    (String.concat "," (List.map string_of_int tiles))
    (json_int_opt state.selected)
    (json_int_opt state.selected_segment)
    (json_str_opt state.last_command)
    (Tiling.size state.tiling)
    (json_str hasse)

(* Minimal JSON value parser *)
let parse_json_field body key =
  let pat = Printf.sprintf "\"%s\"" key in
  let pat_len = String.length pat in
  let len = String.length body in
  let idx = ref (-1) in
  for j = 0 to len - pat_len do
    if !idx = -1 && String.sub body j pat_len = pat then
      idx := j
  done;
  if !idx = -1 then None
  else
    let after_key = !idx + pat_len in
    let rec skip_ws j =
      if j >= len then j
      else match body.[j] with
        | ' ' | '\t' | '\n' | '\r' | ':' -> skip_ws (j + 1)
        | _ -> j
    in
    let vstart = skip_ws after_key in
    if vstart >= len then None
    else if body.[vstart] = '"' then begin
      let rec find_end j =
        if j >= len then j
        else if body.[j] = '"' then j
        else find_end (j + 1)
      in
      let vend = find_end (vstart + 1) in
      Some (String.sub body (vstart + 1) (vend - vstart - 1))
    end else begin
      let rec find_end j =
        if j >= len then j
        else match body.[j] with
          | ',' | '}' | ' ' | '\n' -> j
          | _ -> find_end (j + 1)
      in
      let vend = find_end vstart in
      Some (String.sub body vstart (vend - vstart))
    end

(* Generate a random Schroder tiling with n leaves, numbered 0..n-1 *)
let random_tiling n =
  if n <= 0 then (false, Schrot.Tile 0)
  else begin
    Random.self_init ();
    let rec build leaves =
      match leaves with
      | [] -> assert false
      | [x] -> Schrot.Tile x
      | _ ->
        (* Random number of children: 2 to List.length leaves *)
        let max_k = List.length leaves in
        let k = 2 + Random.int (max_k - 1) in
        let k = min k max_k in
        (* Random partition of leaves into k non-empty groups *)
        let arr = Array.of_list leaves in
        (* Fisher-Yates shuffle *)
        let len = Array.length arr in
        for i = len - 1 downto 1 do
          let j = Random.int (i + 1) in
          let tmp = arr.(i) in
          arr.(i) <- arr.(j);
          arr.(j) <- tmp
        done;
        (* Place k-1 cut points among len-1 gaps *)
        let gaps = Array.init (len - 1) (fun i -> i + 1) in
        for i = Array.length gaps - 1 downto 1 do
          let j = Random.int (i + 1) in
          let tmp = gaps.(i) in
          gaps.(i) <- gaps.(j);
          gaps.(j) <- tmp
        done;
        let cuts = Array.sub gaps 0 (k - 1) in
        Array.sort compare cuts;
        let groups = ref [] in
        let prev = ref 0 in
        Array.iter (fun c ->
          groups := Array.to_list (Array.sub arr !prev (c - !prev)) :: !groups;
          prev := c
        ) cuts;
        groups := Array.to_list (Array.sub arr !prev (len - !prev)) :: !groups;
        let children = List.rev !groups |> List.map build in
        match children with
        | a :: b :: rest -> Schrot.unit_frame (List2.Cons2 (a, b, rest))
        | _ -> assert false
    in
    (Random.bool (), build (List.init n Fun.id))
  end

let split_of_string = function
  | "left"  -> Some (Tiling.V, Tiling.Before)
  | "right" -> Some (Tiling.V, Tiling.After)
  | "up"    -> Some (Tiling.H, Tiling.Before)
  | "down"  -> Some (Tiling.H, Tiling.After)
  | _ -> None

let arrow_of_string s : Tiling.arrow option = match s with
  | "left"  -> Some Left
  | "right" -> Some Right
  | "up"    -> Some Up
  | "down"  -> Some Down
  | _ -> None

let html_page = {|<!DOCTYPE html>
<html>
<head>
<meta charset="utf-8">
<title>Schrot — Schroder Tilings</title>
<style>
* { margin: 0; padding: 0; box-sizing: border-box; }
body { font-family: monospace; background: #1a1a2e; color: #e0e0e0; display: flex;
       flex-direction: column; align-items: center; min-height: 100vh; padding: 20px; }
.header { display: flex; align-items: center; gap: 12px; margin-bottom: 16px;
  flex-wrap: wrap; justify-content: center; }
.header select, .header button { font-family: monospace; font-size: 14px;
  padding: 4px 12px; background: #16213e; color: #e0e0e0; border: 1px solid #0f3460;
  border-radius: 4px; cursor: pointer; }
.header select:hover, .header button:hover { background: #0f3460; }
.header span { font-size: 14px; }
.panels { display: flex; gap: 16px; margin-bottom: 16px; align-items: flex-start; }
.panel { background: #16213e; border: 1px solid #0f3460; border-radius: 8px;
  padding: 10px; }
.panel-label { font-size: 11px; color: #666; text-align: center; margin-bottom: 4px; }
.panel svg { display: block; }
.info { font-size: 14px; margin-bottom: 6px; min-height: 1.4em; }
.info .label { color: #888; }
.info-row { display: flex; align-items: center; gap: 8px; }
.copy-btn { font-family: monospace; font-size: 11px; padding: 1px 6px;
  background: #16213e; color: #888; border: 1px solid #0f3460; border-radius: 3px;
  cursor: pointer; }
.copy-btn:hover { background: #0f3460; color: #e0e0e0; }
.help { font-size: 12px; color: #666; line-height: 1.8; text-align: center;
  border-top: 1px solid #0f3460; padding-top: 12px; margin-top: 8px; max-width: 820px; }
.help kbd { background: #16213e; border: 1px solid #0f3460; border-radius: 3px;
  padding: 1px 5px; font-size: 11px; }
.dim { opacity: 0.4; }
</style>
</head>
<body>
<div class="header">
  <span>Tiles:</span>
  <select id="n-tiles"></select>
  <button id="random">Random</button>
  <button id="reset">Reset</button>
  <button id="undo">Undo</button>
</div>
<div class="panels">
  <div>
    <div class="panel-label" id="prev-label">previous</div>
    <div class="panel" id="prev-container"><svg xmlns="http://www.w3.org/2000/svg" width="240" height="192"></svg></div>
  </div>
  <div>
    <div class="panel-label">current</div>
    <div class="panel" id="svg-container"></div>
  </div>
  <div>
    <div class="panel-label">adjacency poset</div>
    <div class="panel" id="hasse-container"><svg xmlns="http://www.w3.org/2000/svg" width="250" height="250"></svg></div>
  </div>
</div>
<div class="info"><span class="label">Before:</span> <span id="prev-term"></span></div>
<div class="info"><span class="label">After: </span> <span id="term"></span></div>
<div class="info-row">
  <div class="info"><span class="label">Action:</span> <span id="action"></span></div>
  <button class="copy-btn" id="copy" title="Copy transition to clipboard">copy</button>
</div>
<div id="mode" style="font-size:14px; min-height:1.4em; color:#f0c040; text-align:center;"></div>
<div class="help">
  Click tile to select &middot;
  Click cut to select segment &middot;
  Arrows navigate / slide segment &middot;
  <kbd>Alt</kbd>+Arrow split &middot;
  <kbd>Alt</kbd>+<kbd>Del</kbd> close &middot;
  <kbd>Shift</kbd>+Arrow wall slide &middot;
  <kbd>d</kbd> dissolve &middot;
  <kbd>x</kbd> exit frame &middot;
  <kbd>f</kbd>+Arrow 2-subframe &middot;
  <kbd>e</kbd>+Arrow enter frame &middot;
  <kbd>Esc</kbd> deselect &middot;
  <kbd>Ctrl+Z</kbd> undo
</div>
<script>
let selectedTile = null;
let selectedSegment = null;
let currentData = {};
let pendingMode = null; // 'subframe' or 'enter'

async function api(method, path, body) {
  const opts = { method, headers: { 'Content-Type': 'application/json' } };
  if (body) opts.body = JSON.stringify(body);
  const r = await fetch(path, opts);
  return r.json();
}

function makeSvg(svgContent, w, h) {
  return '<svg xmlns="http://www.w3.org/2000/svg" width="' + w + '" height="' + h + '">' +
    '<rect width="' + w + '" height="' + h + '" fill="#16213e"/>' + svgContent + '</svg>';
}

function attachHandlers(container) {
  container.querySelectorAll('g[data-tile]').forEach(g => {
    g.addEventListener('click', () => {
      selectedTile = parseInt(g.dataset.tile);
      selectedSegment = null;
      refreshState();
    });
  });
  container.querySelectorAll('g[data-segment]').forEach(g => {
    g.addEventListener('click', (e) => {
      e.stopPropagation();
      selectedSegment = parseInt(g.dataset.segment);
      api('POST', '/api/select_segment', { id: String(selectedSegment) })
        .then(data => updateDisplay(data));
    });
  });
}

function updateDisplay(data) {
  currentData = data;
  const c = document.getElementById('svg-container');
  c.innerHTML = makeSvg(data.svg, 500, 400);
  document.getElementById('term').textContent = data.term;

  // Hasse diagram
  const hc = document.getElementById('hasse-container');
  hc.innerHTML = makeSvg(data.hasse_svg, 250, 250);

  const pc = document.getElementById('prev-container');
  const pl = document.getElementById('prev-label');
  if (data.prev_svg) {
    pc.innerHTML = makeSvg(data.prev_svg, 240, 192);
    pc.classList.remove('dim');
    pl.classList.remove('dim');
    document.getElementById('prev-term').textContent = data.prev_term;
  } else {
    pc.innerHTML = '<svg xmlns="http://www.w3.org/2000/svg" width="240" height="192"></svg>';
    pc.classList.add('dim');
    pl.classList.add('dim');
    document.getElementById('prev-term').textContent = '';
  }

  document.getElementById('action').textContent = data.command || '';

  const nSel = document.getElementById('n-tiles');
  if (nSel.value !== String(data.n_tiles)) {
    nSel.value = String(data.n_tiles);
  }

  if (data.selected !== null && data.selected !== undefined) {
    selectedTile = data.selected;
  }
  if (data.selected_segment !== null && data.selected_segment !== undefined) {
    selectedSegment = data.selected_segment;
  } else {
    selectedSegment = null;
  }

  attachHandlers(c);
  updateMode();
}

function initTilesDropdown() {
  const sel = document.getElementById('n-tiles');
  for (let i = 1; i <= 12; i++) {
    const o = document.createElement('option');
    o.value = i; o.textContent = i;
    sel.appendChild(o);
  }
  sel.value = '1';
}

async function refreshState() {
  const url = selectedTile !== null ? '/api/state?selected=' + selectedTile : '/api/state';
  const data = await api('GET', url);
  updateDisplay(data);
}

async function postCommand(action, tile, dir) {
  const body = { action, tile: String(tile) };
  if (dir) body.dir = dir;
  const data = await api('POST', '/api/command', body);
  if (data.selected !== null && data.selected !== undefined) {
    selectedTile = data.selected;
  }
  selectedSegment = null;
  updateDisplay(data);
}

async function slideSegment(id, dir) {
  const data = await api('POST', '/api/slide_segment', { id: String(id), dir });
  updateDisplay(data);
}

document.getElementById('reset').addEventListener('click', async () => {
  selectedTile = 0;
  selectedSegment = null;
  const data = await api('POST', '/api/reset');
  selectedTile = data.selected;
  updateDisplay(data);
});

document.getElementById('random').addEventListener('click', async () => {
  const n = parseInt(document.getElementById('n-tiles').value) || 3;
  selectedSegment = null;
  const data = await api('POST', '/api/random', { n });
  selectedTile = data.selected;
  updateDisplay(data);
});

document.getElementById('undo').addEventListener('click', async () => {
  selectedSegment = null;
  const data = await api('POST', '/api/undo');
  selectedTile = data.selected;
  updateDisplay(data);
});

document.getElementById('copy').addEventListener('click', () => {
  const d = currentData;
  const lines = [];
  if (d.prev_term) lines.push('before: ' + d.prev_term);
  lines.push('after:  ' + (d.term || ''));
  if (d.command) lines.push('action: ' + d.command);
  navigator.clipboard.writeText(lines.join('\n')).then(() => {
    const btn = document.getElementById('copy');
    btn.textContent = 'copied!';
    setTimeout(() => btn.textContent = 'copy', 1200);
  });
});

function updateMode() {
  const el = document.getElementById('mode');
  if (selectedSegment !== null) {
    el.textContent = 'segment ' + selectedSegment + ' — arrows slide';
    el.style.color = '#f0c040';
  } else if (pendingMode) {
    el.textContent = pendingMode + ' \u2192 arrow';
    el.style.color = '#f0c040';
  } else {
    el.textContent = '';
  }
}

document.addEventListener('keydown', (e) => {
  if (e.key === 'Escape') {
    if (selectedSegment !== null) {
      selectedSegment = null;
      api('POST', '/api/select_segment', { id: null })
        .then(data => updateDisplay(data));
      return;
    }
    if (pendingMode) { pendingMode = null; updateMode(); return; }
    selectedTile = null; refreshState(); return;
  }
  if (e.key === 'z' && (e.ctrlKey || e.metaKey)) {
    e.preventDefault();
    selectedSegment = null;
    document.getElementById('undo').click();
    return;
  }

  const dir = { ArrowLeft: 'left', ArrowRight: 'right',
                 ArrowUp: 'up', ArrowDown: 'down' }[e.key];

  // Segment mode: arrows slide the selected segment
  if (selectedSegment !== null && dir) {
    e.preventDefault();
    slideSegment(selectedSegment, dir);
    return;
  }

  if (selectedTile === null) return;

  // Pending mode: f/e + arrow
  if (pendingMode && dir) {
    e.preventDefault();
    postCommand(pendingMode, selectedTile, dir);
    pendingMode = null; updateMode();
    return;
  }
  if (pendingMode && !dir) { pendingMode = null; updateMode(); }

  // Alt+Arrow: split.  Alt+Del: close.
  if (e.altKey) {
    if (e.key === 'Delete' || e.key === 'Backspace') {
      e.preventDefault(); postCommand('close', selectedTile); return;
    }
    if (dir) { e.preventDefault(); postCommand('split', selectedTile, dir); }
    return;
  }
  // Shift+Arrow: wall slide
  if (e.shiftKey && dir) {
    e.preventDefault(); postCommand('slide', selectedTile, dir); return;
  }
  // d: dissolve
  if (e.key === 'd') { e.preventDefault(); postCommand('dissolve', selectedTile); return; }
  // x: exit frame
  if (e.key === 'x') { e.preventDefault(); postCommand('exit', selectedTile); return; }
  // f: 2-subframe mode (then arrow)
  if (e.key === 'f') { e.preventDefault(); pendingMode = 'subframe'; updateMode(); return; }
  // e: enter frame mode (then arrow)
  if (e.key === 'e') { e.preventDefault(); pendingMode = 'enter'; updateMode(); return; }
  // Plain arrow: navigate
  if (dir) {
    e.preventDefault();
    api('POST', '/api/navigate', { tile: String(selectedTile), dir })
      .then(data => {
        if (data.selected !== null && data.selected !== undefined) {
          selectedTile = data.selected;
        }
        updateDisplay(data);
      });
  }
});

(async () => {
  initTilesDropdown();
  const data = await api('GET', '/api/state?selected=0');
  updateDisplay(data);
})();
</script>
</body>
</html>
|}

let get_selected req =
  match Dream.query req "selected" with
  | Some s -> (try Some (int_of_string s) with _ -> None)
  | None -> None

let () =
  Dream.run ~port:8080
  @@ Dream.logger
  @@ Dream.router [
    Dream.get "/" (fun _req ->
      Dream.html html_page);

    Dream.get "/api/state" (fun req ->
      state.selected <- get_selected req;
      Dream.json (state_json ()));

    Dream.post "/api/command" (fun req ->
      Lwt.bind (Dream.body req) (fun body ->
        let action = parse_json_field body "action" in
        let tile = parse_json_field body "tile" in
        let split_info = Option.bind (parse_json_field body "dir") split_of_string in
        match action, tile with
        | Some action, Some tile_s ->
          let tile = int_of_string tile_s in
          let prev = state.tiling in
          let cmd_name, new_tiling = match action with
            | "split" ->
              let d, side = match split_info with
                | Some (d, s) -> d, s
                | None -> Tiling.H, Tiling.After
              in
              let dir_s = match d with Tiling.H -> "H" | Tiling.V -> "V" in
              let side_s = match side with Tiling.Before -> "before" | Tiling.After -> "after" in
              Printf.sprintf "split %d %s %s" tile dir_s side_s,
              Tiling.split ~side tile d state.tiling
            | "close" ->
              Printf.sprintf "close %d" tile,
              (if Tiling.size state.tiling > 1
               then Tiling.close tile state.tiling
               else state.tiling)
            | "slide" ->
              let arrow = Option.bind (parse_json_field body "dir") arrow_of_string in
              (match Option.bind arrow (fun a -> Tiling.neighbor tile a state.tiling) with
               | Some nb ->
                 let desc = Printf.sprintf "slide %d %d" tile nb in
                 (match Tiling.wall_slide tile nb state.tiling with
                  | Some t' -> desc, t'
                  | None -> desc ^ " (no effect)", state.tiling)
               | None -> "slide (no neighbor)", state.tiling)
            | "dissolve" ->
              let desc = Printf.sprintf "dissolve %d" tile in
              (match Tiling.simple_dissolve tile state.tiling with
               | Some t' -> desc, t'
               | None -> desc ^ " (no effect)", state.tiling)
            | "subframe" ->
              let arrow = Option.bind (parse_json_field body "dir") arrow_of_string in
              (match Option.bind arrow (fun a -> Tiling.neighbor tile a state.tiling) with
               | Some nb ->
                 let a, b = min tile nb, max tile nb in
                 let desc = Printf.sprintf "2-subframe %d %d" a b in
                 (match Tiling.simple_create a b state.tiling with
                  | Some t' -> desc, t'
                  | None -> desc ^ " (no effect)", state.tiling)
               | None -> "2-subframe (no neighbor)", state.tiling)
            | "exit" ->
              let desc = Printf.sprintf "exit %d" tile in
              (match Tiling.pivot_out tile state.tiling with
               | Some t' -> desc, t'
               | None -> desc ^ " (no effect)", state.tiling)
            | "enter" ->
              let arrow = Option.bind (parse_json_field body "dir") arrow_of_string in
              (match Option.bind arrow (fun a -> Tiling.neighbor tile a state.tiling) with
               | Some nb ->
                 let desc = Printf.sprintf "enter %d %d" tile nb in
                 (match Tiling.pivot_in tile nb state.tiling with
                  | Some t' -> desc, t'
                  | None -> desc ^ " (no effect)", state.tiling)
               | None -> "enter (no neighbor)", state.tiling)
            | _ -> "?", state.tiling
          in
          push_history ();
          state.prev_tiling <- Some prev;
          state.tiling <- new_tiling;
          update_geometry ();
          state.last_command <- Some cmd_name;
          (* Selection logic *)
          let new_leaves = List.sort compare (Tiling.leaves new_tiling) in
          (match action with
           | "split" ->
             let old_leaves = List.sort compare (Tiling.leaves prev) in
             let created = List.find_opt
               (fun n -> not (List.mem n old_leaves)) new_leaves in
             state.selected <- (match created with Some n -> Some n | None -> Some tile)
           | "close" ->
             if not (List.mem tile new_leaves) then
               state.selected <- (match new_leaves with n :: _ -> Some n | [] -> None)
             else
               state.selected <- Some tile
           | _ ->
             state.selected <- Some tile);
          Dream.json (state_json ())
        | _ ->
          Dream.json ~status:`Bad_Request {|{"error":"missing action or tile"}|}));

    Dream.post "/api/reset" (fun _req ->
      state.tiling <- (false, Schrot.Tile 0);
      update_geometry ();
      state.prev_tiling <- None;
      state.selected <- Some 0;
      state.last_command <- None;
      state.history <- [];
      Dream.json (state_json ()));

    Dream.post "/api/undo" (fun _req ->
      (match state.history with
       | (prev, prev_sel) :: rest ->
         state.prev_tiling <- Some state.tiling;
         state.tiling <- prev;
         update_geometry ();
         state.selected <- prev_sel;
         state.history <- rest;
         state.last_command <- Some "undo"
       | [] -> ());
      Dream.json (state_json ()));

    Dream.post "/api/random" (fun _req ->
      Lwt.bind (Dream.body _req) (fun body ->
        let n = match parse_json_field body "n" with
          | Some s -> (try int_of_string s with _ -> 3)
          | None -> 3
        in
        let t = random_tiling n in
        state.tiling <- t;
        update_geometry ();
        state.prev_tiling <- None;
        state.selected <- Some 0;
        state.last_command <- None;
        state.history <- [];
        Dream.json (state_json ())));

    Dream.post "/api/navigate" (fun _req ->
      Lwt.bind (Dream.body _req) (fun body ->
        let tile = parse_json_field body "tile" in
        let dir = Option.bind (parse_json_field body "dir") arrow_of_string in
        match tile, dir with
        | Some tile_s, Some arrow ->
          let tile = int_of_string tile_s in
          (match Tiling.neighbor tile arrow state.tiling with
           | Some n -> state.selected <- Some n
           | None -> ());
          Dream.json (state_json ())
        | _ ->
          Dream.json ~status:`Bad_Request {|{"error":"missing tile or dir"}|}));

    Dream.post "/api/select_segment" (fun _req ->
      Lwt.bind (Dream.body _req) (fun body ->
        let seg_id = Option.bind (parse_json_field body "id")
          (fun s -> try Some (int_of_string s) with _ -> None) in
        state.selected_segment <- seg_id;
        Dream.json (state_json ())));

    Dream.post "/api/slide_segment" (fun _req ->
      Lwt.bind (Dream.body _req) (fun body ->
        let seg_id = Option.bind (parse_json_field body "id")
          (fun s -> try Some (int_of_string s) with _ -> None) in
        let dir = Option.bind (parse_json_field body "dir") arrow_of_string in
        let quantum = 0.03 in
        (match seg_id, dir with
         | Some id, Some arrow ->
           (match List.find_opt
              (fun (s : Tiling.segment) -> s.seg_id = id) state.segments with
            | Some seg ->
              let delta = match arrow, seg.seg_is_h with
                | (Up | Left), _ -> -. quantum
                | (Down | Right), _ -> quantum
              in
              (match Tiling.adjust_weight
                       seg.seg_path seg.seg_cut delta state.weighted with
               | Some wt' ->
                 state.weighted <- wt';
                 update_segments ();
                 state.last_command <-
                   Some (Printf.sprintf "slide segment %d" id)
               | None -> ())
            | None -> ())
         | _ -> ());
        Dream.json (state_json ())));
  ]
