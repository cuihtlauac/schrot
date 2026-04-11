(** Schroeder tilings: rectangle tilings counted by large Schroeder numbers.

    Each child in a [Frame] is paired with a weight ['f]. Cut ratios are
    computed proportionally from children's weights. With ['f = unit], the
    tree carries topology only (equal splits by default). *)

type ('a, 'f) t = Tile of 'a | Frame of ('f * ('a, 'f) t) List2.t

type ('a, 'f) tiling = bool * ('a, 'f) t

(** {1 Catamorphism} *)

val fold : ('a -> 'b) -> (('f * 'b) List2.t -> 'b) -> ('a, 'f) t -> 'b

(** {1 Anamorphism} *)

val unfold : ('seed -> ('a, ('f * 'seed) List2.t) Either.t) -> 'seed -> ('a, 'f) t

(** {1 Map} *)

val map : ('a -> 'b) -> ('a, 'f) t -> ('b, 'f) t

val map_weights : ('f -> 'g) -> ('a, 'f) t -> ('a, 'g) t

(** {1 Observers} *)

val equal : ('a -> 'a -> bool) -> ('f -> 'f -> bool) -> ('a, 'f) t -> ('a, 'f) t -> bool
val size : ('a, 'f) t -> int
val height : ('a, 'f) t -> int
val leaves : ('a, 'f) t -> 'a list

(** {1 Construction} *)

val unit_frame : ('a, unit) t List2.t -> ('a, unit) t
(** [unit_frame children] wraps each child with a [()] weight. *)

(** {1 Enumeration} *)

val enum : int -> (unit, unit) tiling list
