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
