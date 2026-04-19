(** Tiling operations on Schroder trees.

    Layer 1 (operad): {!split}, {!close}, {!neighbor}.
    Layer 2 (quotientope): {!simple_dissolve}, {!simple_create},
    {!pivot_out}, {!pivot_in}, {!wall_slide}, {!enumerate_flips}.
    Layer 3 (2D lattice): {!resolve_splits}, {!rects_of_weighted},
    {!cuts_of_weighted}. *)

type dir = H | V
type side = Before | After
type t = (int, unit) Schrot.tiling

val is_h : t -> bool
val tree : t -> (int, unit) Schrot.t
val size : t -> int
val leaves : t -> int list
val max_leaf : t -> int
val tree_to_string : bool -> (int, 'a) Schrot.t -> string
val to_string : t -> string

(** {1 Tree-walk utilities} *)

val path_to_leaf : (int, 'a) Schrot.t -> int -> int list option
(** Index path from the root of a tree to the leaf labeled [n], or
    [None] if absent.  The empty list means [n] is the root tile.
    When you have a {!t}, pass [tree t]. *)

val lcp : 'a list -> 'a list -> 'a list
(** Longest common prefix of two lists. *)

val descend : (int, 'a) Schrot.t -> int list -> (int, 'a) Schrot.t option
(** Descend into a tree along a path of child indices.  Returns [None]
    if the path exits a leaf or is out of bounds. *)

(** {1 Layer 1 — operad (split/close)} *)

val split : ?side:side -> int -> dir -> t -> t
val close : int -> t -> t

type arrow = Left | Right | Up | Down

val first_leaf : ('a, 'b) Schrot.t -> 'a
val last_leaf : ('a, 'b) Schrot.t -> 'a
val neighbor : int -> arrow -> t -> int option

(** {1 Layer 3 — geometry (junction resolution)} *)

type rect = { rx : float; ry : float; rw : float; rh : float }

val resolve_splits :
  ?max_iter:int -> t -> (int, float) Schrot.tiling

val rects_of_weighted :
  (int, float) Schrot.tiling -> (int * rect) list

val cuts_of_weighted :
  (int, float) Schrot.tiling ->
  (float * float * float * float) list array

(** {1 Segment enumeration and weight adjustment} *)

type segment = {
  seg_id : int;
  seg_path : int list;
  seg_cut : int;
  seg_is_h : bool;
  seg_depth : int;
  seg_x1 : float; seg_y1 : float;
  seg_x2 : float; seg_y2 : float;
}

val enumerate_segments : (int, float) Schrot.tiling -> segment list

val adjust_weight :
  int list -> int -> float ->
  ('a, float) Schrot.tiling -> ('a, float) Schrot.tiling option

(** {1 Degenerate vertex detection} *)

type rational_point = { px : int; pd : int; qx : int; qd : int }

val compare_rpoint : rational_point -> rational_point -> int
val normalize_rpoint : rational_point -> rational_point

val degenerate_corners : t -> (rational_point * int) list
val degenerate_cuts : t -> rational_point list
val cross_junction_tiles : t -> (rational_point * int list) list
val count_cross_junctions : t -> int

(** {1 Cut depth} *)

val cut_depth : int -> int -> t -> int

(** {1 Tabstop adjacency} *)

type adjacency = North | South | East | West
type tab = int
type bounds = { left : tab; right : tab; top : tab; bottom : tab }

val tabstop_extract : t -> (int * bounds) list
val tabstop_adjacent : int -> int -> (int * bounds) list -> adjacency option
val tabstop_neighbors : int -> (int * bounds) list -> (adjacency * int) list
val tabstop_all_adjacencies : t -> (int * int) list

(** {1 Strong guillotine counting} *)

val binom : int -> int -> int
val delannoy : int -> int -> int
val perp_cuts : ('a, 'b) Schrot.t -> int
val multiplicity : t -> int
val multiplicity_nongeneric : t -> int

module EdgeSet : Set.S with type elt = int * int

val enumerate_all_strong_adjacencies : t -> EdgeSet.elt list list
val enumerate_all_strong_adjacencies_nongeneric : t -> EdgeSet.elt list list
val diagonal_pairs : t -> ((int * int) * (int * int)) list
val enumerate_strong_adjacencies :
  geo_edges:EdgeSet.elt list -> t -> EdgeSet.elt list list

(** {1 D4 / V4 symmetry} *)

val rot90 : t -> t
val rot180 : t -> t
val flip_h : t -> t

val d4_orbit : t -> t list
val d4_actions : (string * (t -> t)) array
val v4_orbit : t -> t list

val erase : t -> (unit, unit) Schrot.tiling
val to_perm : ('a, 'f) Schrot.tiling -> int array
val unit_tree_to_string : bool -> (unit, 'a) Schrot.t -> string
val canonical_d4 : t -> string
val canonical_v4 : t -> string
val canonical_planar : ('a, 'b) Schrot.t -> (unit, 'b) Schrot.t
val canonical_unordered : ('a, 'b) Schrot.t -> (unit, 'b) Schrot.t
val relabel : ('a, 'b) Schrot.tiling -> (int, 'b) Schrot.tiling

(** {1 Graph utilities} *)

val graphs_isomorphic : int -> (int * int) list -> (int * int) list -> bool
val adjacency_fingerprint : int -> (int * int) list -> (int * int list) list

(** {1 Layer 2 — quotientope (flips)} *)

type flip =
  | Simple_dissolve of int
  | Simple_create of int * int
  | Pivot_out of int
  | Pivot_in of int * int
  | Wall_slide of int * int
  | T_flip of int * int  (** (stem_tile, bar_tile) *)

val simple_dissolve : int -> t -> t option
val simple_create : int -> int -> t -> t option
val pivot_out : int -> t -> t option
val pivot_out_root : int -> side -> t -> t option
val pivot_in : int -> int -> t -> t option
val pivot_in_root : int -> side -> t -> t option
val pivot_in_wrap : int -> int -> t -> t option
val wall_slide : int -> int -> t -> t option

val apply_t_flip_symbolic : stem:int -> bar:int -> t -> t option
(** Round 6 symbolic T-flip: LCA rewrite on the Schroder tree.
    Applies the tree transformation assuming the (stem, bar) pair
    names a valid Asinowski-pivoting cover relation.

    Returns [None] when (stem, bar) is structurally invalid — not
    both leaves, LCA's stem_branch is a Tile, stem is in a middle
    child of stem_branch (arity >= 3, not the first or last), etc.

    This function does NOT verify guillotine-admissibility of the
    output.  For 150/1782 T-flip candidates at n=7 (M-M flips whose
    post-flip geometry is a windmill), this function returns
    [Some t'] where [t'] does not correspond to any guillotine
    rectangulation.  Use {!Geom.t_flip} for the filtered production
    API, or compose this with {!Geom.is_asinowski_admissible}. *)

val enumerate_flips : t -> (flip * t) list
val flip_to_string : flip -> string
val count_flip_sites : 'a -> int

(** {1 Exact-rational cut extraction} *)

type cut = {
  pos_num : int;
  pos_den : int;
  span_lo : int;
  span_hi : int;
  span_den : int;
}

val collect_cuts : t -> cut list * cut list
