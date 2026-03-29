type 'a t = Cons2 of 'a * 'a * 'a list

(** {1 Conversions} *)

val to_list : 'a t -> 'a list
val of_list : 'a -> 'a -> 'a list -> 'a t
val of_list_opt : 'a list -> 'a t option

(** {1 Basic operations} *)

val length : 'a t -> int
val compare_lengths : 'a t -> 'b t -> int
val compare_length_with : 'a t -> int -> int
val cons : 'a -> 'a t -> 'a t
val hd : 'a t -> 'a
val nth : 'a t -> int -> 'a
val nth_opt : 'a t -> int -> 'a option

(** {1 Reversal and concatenation} *)

val rev : 'a t -> 'a t
val append : 'a t -> 'a list -> 'a t
val rev_append : 'a t -> 'a list -> 'a t
val init : int -> (int -> 'a) -> 'a t

(** {1 Comparison} *)

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

(** {1 Iterators} *)

val iter : ('a -> unit) -> 'a t -> unit
val iteri : (int -> 'a -> unit) -> 'a t -> unit
val map : ('a -> 'b) -> 'a t -> 'b t
val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
val rev_map : ('a -> 'b) -> 'a t -> 'b t
val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
val fold_right : ('a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
val fold_left_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a t -> 'acc * 'b t

(** {1 Iterators on two lists} *)

val iter2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit
val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
val rev_map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
val fold_left2 : ('acc -> 'a -> 'b -> 'acc) -> 'acc -> 'a t -> 'b t -> 'acc
val fold_right2 : ('a -> 'b -> 'acc -> 'acc) -> 'a t -> 'b t -> 'acc -> 'acc

(** {1 Scanning} *)

val for_all : ('a -> bool) -> 'a t -> bool
val exists : ('a -> bool) -> 'a t -> bool
val for_all2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
val exists2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
val mem : 'a -> 'a t -> bool
val memq : 'a -> 'a t -> bool

(** {1 Searching} *)

val find : ('a -> bool) -> 'a t -> 'a
val find_opt : ('a -> bool) -> 'a t -> 'a option
val find_index : ('a -> bool) -> 'a t -> int option
val find_map : ('a -> 'b option) -> 'a t -> 'b option
val find_mapi : (int -> 'a -> 'b option) -> 'a t -> 'b option

(** {1 Lists of pairs} *)

val split : ('a * 'b) t -> 'a t * 'b t
val combine : 'a t -> 'b t -> ('a * 'b) t

(** {1 Sorting} *)

val sort : ('a -> 'a -> int) -> 'a t -> 'a t
val stable_sort : ('a -> 'a -> int) -> 'a t -> 'a t
val fast_sort : ('a -> 'a -> int) -> 'a t -> 'a t
val merge : ('a -> 'a -> int) -> 'a t -> 'a t -> 'a t

(** {1 Sequences} *)

val to_seq : 'a t -> 'a Seq.t
val of_seq : 'a -> 'a -> 'a Seq.t -> 'a t
