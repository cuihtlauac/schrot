(** Tiling geometry in normalized coordinates on [0,1]².

    All positions and dimensions are relative to the unit square.
    SVG renderers scale these to pixel coordinates at render time.
    Adjacency excludes point contact (Eppstein et al. 2009). *)

type rect = { x: float; y: float; w: float; h: float }

type t = {
  tiling: Tiling.t;
  rects: (int * rect) list;
  adjacency: (int * int) list;  (** sorted pairs (a, b) with a < b *)
}

val of_tiling : Tiling.t -> t
val of_weighted : ?min_overlap:float -> (int, float) Schrot.tiling -> Tiling.t -> t
val of_tiling_equal : Tiling.t -> t

val rect_of : t -> int -> rect
val center_of : t -> int -> float * float
val edges : t -> (int * int) list
val neighbors : t -> int -> int list

(** {1 T-joint enumeration} *)

type corner = NW | NE | SW | SE

(** A T-joint: a vertex where exactly 3 tile closures meet.
    A wall [t] terminates at a through-wall [w], splitting [w] into
    sub-walls [w'] and [w''].  The {b bar tile} spans across [t] on one
    side of [w]; the two {b stem tiles} are separated by [t] on the other
    side.  [through_h] is the orientation of [w]. *)
type t_joint = {
  jx : float;
  jy : float;
  bar_tile : int;
  stem_tiles : int * int;
  stem_corners : corner * corner;
  through_h : bool;
}

val t_joints : ?eps:float -> t -> t_joint list
(** Enumerate T-joints in the resolved geometry.  Works on floating-point
    rectangles from {!of_tiling} (which uses [resolve_splits]).
    Cross junctions (multiplicity 4) are excluded. *)

val subwall_simplicity : ?eps:float -> t -> (t_joint * bool * bool) list
(** For each T-joint (same order as {!t_joints}), whether the sub-wall
    toward the smaller (lo) and larger (hi) coordinate along the
    through-wall is simple.  A sub-wall is simple iff no other T-joint
    lies between this joint and the wall boundary on that side.

    Assumes resolved geometry (from {!of_tiling}). *)

(** {1 Equivalence predicates} *)

val is_generic : ?eps:float -> t -> bool
(** A rectangulation is generic iff all interior vertices are T-joints
    (no cross junctions / no 4-way vertices). *)

val weak_equivalent : ?eps:float -> t -> t -> bool
(** Two rectangulations are weakly equivalent iff they share at least one
    common Schroder tree shape (ignoring tile labels and split ratios).
    Returns [false] if either input is non-guillotine. *)

val strong_equivalent : t -> t -> bool
(** Two rectangulations are strongly equivalent iff their adjacency graphs
    are isomorphic as abstract graphs. *)

(** {1 Geometric T-flips} *)

type flip_side = Lo | Hi

val apply_t_flip : ?eps:float -> t_joint -> flip_side -> t -> (int * rect) list option
(** Apply a geometric T-flip at a T-joint.  Returns modified rect list.
    The stem tile on the given side extends perpendicular to the through-wall,
    absorbing the adjacent portion of the bar tile. *)

val enumerate_t_flips : ?eps:float -> t -> (Tiling.flip * Tiling.t * (int * rect) list) list
(** All valid geometric T-flips for a tiling.  Returns [(flip, result_tree,
    new_rects)] for each valid flip.  [new_rects] is the post-flip geometry
    (before tree reconstruction); use it for reverse-flip enumeration at
    cross junctions where [resolve_splits] produces incompatible geometry. *)

val enumerate_t_flips_from_rects :
  ?eps:float -> (int * rect) list -> (Tiling.flip * Tiling.t) list
(** Like {!enumerate_t_flips} but on a raw rect list (no tree needed).
    Used for reverse-flip enumeration from post-flip geometry. *)

(** {1 Geometry to tree reconstruction} *)

(** An irreducible sub-rectangulation with no wall-to-wall cut. *)
type block = {
  block_tiles : (int * rect) list;
  block_bbox : rect;
}

(** Partial decomposition tree.  Leaves are single tiles or irreducible
    blocks (sub-rectangulations containing windmills). *)
type partial_tree =
  | PTile of int
  | PBlock of block
  | PFrame of bool * partial_tree list
  (** [PFrame (is_h, children)]: [is_h] = orientation, list has >= 2 elements. *)

(** Diagnosis returned when a rectangulation is not guillotine. *)
type non_guillotine = {
  decomposition : partial_tree;
  blocks : block list;
  (** All irreducible blocks, extracted from [decomposition]. *)
}

val tree_of_rects :
  ?eps:float ->
  (int * rect) list ->
  (Tiling.t list, non_guillotine) result
(** Reconstruct Schroder tree(s) from tile rectangles on [0,1]².

    @return [Ok tilings] for guillotine input.  The list has exactly one
    element for generic input; up to [2^k] for non-generic input with [k]
    cross junctions (both H and V wall-to-wall cuts at the same level).

    @return [Error info] when some sub-region has no wall-to-wall cut
    (contains a windmill).  The error carries the maximal partial
    decomposition together with the list of irreducible blocks. *)

val tree_of_geom :
  ?eps:float -> t -> (Tiling.t list, non_guillotine) result
(** Convenience wrapper: [tree_of_geom g = tree_of_rects g.rects]. *)

(** {1 Asinowski-admissible T-flips} *)

val is_asinowski_admissible :
  ?eps:float -> t -> t_joint -> flip_side -> bool
(** Whether a T-flip's post-flip geometry is a strong (guillotine and
    generic) rectangulation.  This is Asinowski et al. 2024 §4.5's
    admissibility criterion; it restricts the Merino-Mütze T-flips of
    {!apply_t_flip} to exactly those that stay in the strong poset.

    A flip passes iff {!apply_t_flip} returns [Some rects] and
    [tree_of_rects rects] returns [Ok [t']] with generic [t']
    (no cross junctions).  Other outcomes — windmill, ambiguous
    reconstruction (multi-tree [Ok] from cross junctions created by the
    flip), or [apply_t_flip] rejecting the input — fail.

    Additive to the existing M-M flip machinery; {!apply_t_flip} and
    {!subwall_simplicity} are unchanged. *)
