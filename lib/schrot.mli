(** Schroeder tilings: rectangle tilings counted by large Schroeder numbers. *)

type 'a t = Tile of 'a | Frame of 'a t List2.t

type 'a tiling = bool * 'a t

(** {1 Catamorphism} *)

val fold : ('a -> 'b) -> ('b List2.t -> 'b) -> 'a t -> 'b

(** {1 Anamorphism} *)

val unfold : ('seed -> ('a, 'seed List2.t) Either.t) -> 'seed -> 'a t

(** {1 Map} *)

val map : ('a -> 'b) -> 'a t -> 'b t

(** {1 Observers} *)

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
val size : 'a t -> int
val height : 'a t -> int
val leaves : 'a t -> 'a list

(** {1 Enumeration} *)

val enum : int -> unit tiling list
