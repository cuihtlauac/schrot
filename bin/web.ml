open Nachum

type state = {
  mutable term : Term.t;
  mutable prev_term : Term.t option;
  mutable policy_name : string;
  mutable selected : int option;
  mutable last_command : string option;
  mutable last_rules : string option;
  mutable origin : Term.t;
  mutable history : (Term.t * int option) list;
}

let state = {
  term = Term.Leaf 0;
  prev_term = None;
  policy_name = "territorial";
  selected = Some 0;
  last_command = None;
  last_rules = None;
  origin = Term.Leaf 0;
  history = [];
}

let push_history () =
  state.history <- (state.term, state.selected) :: state.history

let leaf_set term =
  let rec go acc = function
    | Term.Leaf n -> n :: acc
    | Term.H (a, b) | Term.V (a, b) -> go (go acc a) b
  in
  List.sort compare (go [] term)

let leaf_count term = List.length (leaf_set term)

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

let svg_of_term ?(selected = None) term =
  let color_of n =
    if Some n = selected then "#1a3a6b"
    else Svg.default_color_of n
  in
  let text_color_of n =
    if Some n = selected then "white"
    else "black"
  in
  Svg.render_interactive ~x:0. ~y:0. ~width:500. ~height:400. ~margin:20.
    ~color_of ~text_color_of term

let svg_of_term_small term =
  Svg.render_interactive ~x:0. ~y:0. ~width:240. ~height:192. ~margin:10. term

let state_json () =
  let svg = svg_of_term ~selected:state.selected state.term in
  let prev_svg = match state.prev_term with
    | Some t -> json_str (svg_of_term_small t)
    | None -> "null"
  in
  let prev_term = match state.prev_term with
    | Some t -> json_str (Term.to_string t)
    | None -> "null"
  in
  let tiles = leaf_set state.term in
  let policies = List.map Policy.name Policy.all in
  Printf.sprintf
    {|{"svg":%s,"term":%s,"prev_svg":%s,"prev_term":%s,"policy":"%s","policies":[%s],"tiles":[%s],"selected":%s,"command":%s,"rules":%s,"n_tiles":%d}|}
    (json_str svg)
    (json_str (Term.to_string state.term))
    prev_svg
    prev_term
    state.policy_name
    (String.concat "," (List.map (Printf.sprintf "\"%s\"") policies))
    (String.concat "," (List.map string_of_int tiles))
    (json_int_opt state.selected)
    (json_str_opt state.last_command)
    (json_str_opt state.last_rules)
    (leaf_count state.term)

let dir_of_string = function
  | "left" -> Some Command.Left
  | "right" -> Some Command.Right
  | "up" -> Some Command.Up
  | "down" -> Some Command.Down
  | _ -> None

(* Minimal JSON value parser — just enough for our flat request objects *)
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

(* Generate a random tree with n leaves, numbered 0..n-1 *)
let random_term n =
  if n <= 0 then Term.Leaf 0
  else begin
    Random.self_init ();
    (* Fisher-Yates shuffle *)
    let shuffle arr =
      let len = Array.length arr in
      for i = len - 1 downto 1 do
        let j = Random.int (i + 1) in
        let tmp = arr.(i) in
        arr.(i) <- arr.(j);
        arr.(j) <- tmp
      done
    in
    let rec build leaves =
      match leaves with
      | [] -> assert false
      | [x] -> Term.Leaf x
      | _ ->
        let arr = Array.of_list leaves in
        shuffle arr;
        let split_at = 1 + Random.int (Array.length arr - 1) in
        let left = Array.to_list (Array.sub arr 0 split_at) in
        let right = Array.to_list (Array.sub arr split_at (Array.length arr - split_at)) in
        let a = build left in
        let b = build right in
        if Random.bool () then Term.H (a, b) else Term.V (a, b)
    in
    build (List.init n Fun.id)
  end

let html_page = {|<!DOCTYPE html>
<html>
<head>
<meta charset="utf-8">
<title>Nachum — Tiling Window Manager</title>
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
  border-top: 1px solid #0f3460; padding-top: 12px; margin-top: 8px; }
.help kbd { background: #16213e; border: 1px solid #0f3460; border-radius: 3px;
  padding: 1px 5px; font-size: 11px; }
.dim { opacity: 0.4; }
</style>
</head>
<body>
<div class="header">
  <span>Policy:</span>
  <select id="policy"></select>
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
</div>
<div class="info"><span class="label">Before:</span> <span id="prev-term"></span></div>
<div class="info"><span class="label">After: </span> <span id="term"></span></div>
<div class="info-row">
  <div class="info"><span class="label">Action:</span> <span id="action"></span></div>
</div>
<div class="info-row">
  <div class="info"><span class="label">Rules: </span> <span id="rules"></span></div>
  <button class="copy-btn" id="copy" title="Copy transition to clipboard">copy</button>
</div>
<div class="help">
  Click tile to select &middot;
  <kbd>&larr;</kbd><kbd>&uarr;</kbd><kbd>&darr;</kbd><kbd>&rarr;</kbd> move &middot;
  <kbd>Alt</kbd>+Arrow split &middot;
  <kbd>Del</kbd> close &middot;
  <kbd>Ctrl+Z</kbd> undo &middot;
  <kbd>Esc</kbd> deselect
</div>
<script>
let selectedTile = null;
let currentData = {};

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

function updateDisplay(data) {
  currentData = data;
  const c = document.getElementById('svg-container');
  c.innerHTML = makeSvg(data.svg, 500, 400);
  document.getElementById('term').textContent = data.term;

  // Previous panel
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
  document.getElementById('rules').textContent = data.rules || '';

  // Update tiles dropdown
  const nSel = document.getElementById('n-tiles');
  if (nSel.value !== String(data.n_tiles)) {
    nSel.value = String(data.n_tiles);
  }

  if (data.selected !== null && data.selected !== undefined) {
    selectedTile = data.selected;
  }

  // Attach click handlers
  c.querySelectorAll('g[data-tile]').forEach(g => {
    g.addEventListener('click', () => {
      selectedTile = parseInt(g.dataset.tile);
      refreshState();
    });
  });
}

function initPolicies(data) {
  const sel = document.getElementById('policy');
  sel.innerHTML = '';
  data.policies.forEach(p => {
    const o = document.createElement('option');
    o.value = p; o.textContent = p;
    if (p === data.policy) o.selected = true;
    sel.appendChild(o);
  });
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
  const body = { action, tile };
  if (dir) body.dir = dir;
  const data = await api('POST', '/api/command' +
    (selectedTile !== null ? '?selected=' + selectedTile : ''), body);
  if (data.selected !== null && data.selected !== undefined) {
    selectedTile = data.selected;
  }
  // Re-fetch with correct selection to get highlighted SVG
  const s = await api('GET', '/api/state?selected=' + selectedTile);
  // Preserve the command/rules from the command response
  s.command = data.command;
  s.rules = data.rules;
  s.prev_svg = data.prev_svg;
  s.prev_term = data.prev_term;
  updateDisplay(s);
}

document.getElementById('policy').addEventListener('change', async (e) => {
  const url = selectedTile !== null
    ? '/api/policy?selected=' + selectedTile
    : '/api/policy';
  const data = await api('POST', url, { policy: e.target.value });
  updateDisplay(data);
});

document.getElementById('reset').addEventListener('click', async () => {
  selectedTile = 0;
  const data = await api('POST', '/api/reset');
  selectedTile = data.selected;
  updateDisplay(data);
});

document.getElementById('random').addEventListener('click', async () => {
  const n = parseInt(document.getElementById('n-tiles').value) || 3;
  const data = await api('POST', '/api/random', { n });
  selectedTile = data.selected;
  updateDisplay(data);
});


document.getElementById('undo').addEventListener('click', async () => {
  const data = await api('POST', '/api/undo');
  selectedTile = data.selected;
  updateDisplay(data);
});

document.getElementById('copy').addEventListener('click', () => {
  const d = currentData;
  const lines = [];
  lines.push('policy: ' + document.getElementById('policy').value);
  if (d.prev_term) lines.push('before: ' + d.prev_term);
  lines.push('after:  ' + (d.term || ''));
  if (d.command) lines.push('action: ' + d.command);
  if (d.rules) lines.push('rules:  ' + d.rules);
  navigator.clipboard.writeText(lines.join('\n')).then(() => {
    const btn = document.getElementById('copy');
    btn.textContent = 'copied!';
    setTimeout(() => btn.textContent = 'copy', 1200);
  });
});

document.addEventListener('keydown', (e) => {
  if (e.key === 'Escape') { selectedTile = null; refreshState(); return; }
  if (e.key === 'z' && (e.ctrlKey || e.metaKey)) {
    e.preventDefault(); document.getElementById('undo').click(); return;
  }
  if (selectedTile === null) return;
  const dir = { ArrowLeft: 'left', ArrowRight: 'right',
                 ArrowUp: 'up', ArrowDown: 'down' }[e.key];
  if (!dir) {
    if (e.key === 'Delete' || e.key === 'Backspace') {
      e.preventDefault(); postCommand('close', selectedTile); return;
    }
    return;
  }
  e.preventDefault();
  if (e.altKey) postCommand('split', selectedTile, dir);
  else postCommand('move', selectedTile, dir);
});

// Init
(async () => {
  initTilesDropdown();
  const data = await api('GET', '/api/state?selected=0');
  initPolicies(data);
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
        let dir = Option.bind (parse_json_field body "dir") dir_of_string in
        match action, tile with
        | Some action, Some tile_s ->
          let tile = int_of_string tile_s in
          let policy = Policy.find state.policy_name in
          let cmd = match action with
            | "move" ->
              (match dir with Some d -> Some (Command.Move (tile, d)) | None -> None)
            | "split" ->
              (match dir with Some d -> Some (Command.Split (tile, d)) | None -> None)
            | "close" -> Some (Command.Close tile)
            | _ -> None
          in
          (match cmd with
           | Some cmd ->
             push_history ();
             let prev = state.term in
             let rules = Policy.compile policy cmd state.term in
             let new_term = List.fold_left (fun t r -> Rewrite.apply r t) state.term rules in
             state.prev_term <- Some prev;
             state.term <- new_term;
             state.last_command <- Some (Command.to_string cmd);
             state.last_rules <- Some (Command.rule_list_to_string rules);
             (* Selection logic *)
             let new_leaves = leaf_set new_term in
             (match action with
              | "split" ->
                (* Select the newly created tile *)
                let old_leaves = leaf_set prev in
                let created = List.find_opt
                  (fun n -> not (List.mem n old_leaves)) new_leaves in
                state.selected <- (match created with Some n -> Some n | None -> Some tile)
              | "close" ->
                (* After close, select tile 0 if it exists, else first *)
                if not (List.mem tile new_leaves) then
                  state.selected <- (match new_leaves with n :: _ -> Some n | [] -> None)
                else
                  state.selected <- Some tile
              | _ ->
                if List.mem tile new_leaves then
                  state.selected <- Some tile
                else
                  state.selected <- (match new_leaves with n :: _ -> Some n | [] -> None));
             (* Get selected for query param override *)
             let sel_override = get_selected req in
             if sel_override <> None then state.selected <- sel_override;
             Dream.json (state_json ())
           | None ->
             Dream.json ~status:`Bad_Request {|{"error":"invalid command"}|})
        | _ ->
          Dream.json ~status:`Bad_Request {|{"error":"missing action or tile"}|}));

    Dream.post "/api/policy" (fun req ->
      Lwt.bind (Dream.body req) (fun body ->
        match parse_json_field body "policy" with
        | Some p ->
          state.policy_name <- p;
          state.selected <- get_selected req;
          Dream.json (state_json ())
        | None ->
          Dream.json ~status:`Bad_Request {|{"error":"missing policy"}|}));

    Dream.post "/api/reset" (fun _req ->
      state.term <- state.origin;
      state.prev_term <- None;
      state.selected <- Some 0;
      state.last_command <- None;
      state.last_rules <- None;
      state.history <- [];
      Dream.json (state_json ()));

    Dream.post "/api/undo" (fun _req ->
      (match state.history with
       | (prev_term, prev_sel) :: rest ->
         state.prev_term <- Some state.term;
         state.term <- prev_term;
         state.selected <- prev_sel;
         state.history <- rest;
         state.last_command <- Some "undo";
         state.last_rules <- Some "[]"
       | [] -> ());
      Dream.json (state_json ()));

    Dream.post "/api/random" (fun _req ->
      Lwt.bind (Dream.body _req) (fun body ->
        let n = match parse_json_field body "n" with
          | Some s -> (try int_of_string s with _ -> 3)
          | None -> 3
        in
        let t = random_term n in
        state.term <- t;
        state.origin <- t;
        state.prev_term <- None;
        state.selected <- Some 0;
        state.last_command <- None;
        state.last_rules <- None;
        state.history <- [];
        Dream.json (state_json ())));
  ]
