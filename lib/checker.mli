(** Exhaustive model checker on Schroder tilings.

    Enumerates tilings up to a given size and checks a property
    against each one. Supports D4 orbit reduction and progressive
    enumeration (orbit-first, then all if no counterexample).

    {2 Labeling model}

    Enumeration produces unlabeled tilings via {!Schrot.enum}, then
    assigns each one a single canonical labeling (0..n-1 left-to-right
    in tree order) via {!Tiling.relabel}. Label permutations are
    {e not} explored: each topology is checked under exactly one
    labeling.

    This is sufficient for {b structural properties} — properties
    whose truth value is invariant under relabeling (e.g., "closing
    any tile preserves tile count"). The {!for_all_tiles} combinator
    quantifies over all tile labels within each canonical labeling,
    so "for every tile n, P(n)" is fully tested.

    For {b positional properties} — properties that depend on which
    tile occupies a specific position in the tree (e.g., "tile 0's
    move behavior under a policy") — the canonical labeling tests
    tile 0 in only one position per topology. Such properties are
    undertested compared to a full permutation enumeration. *)

(** {1 Configuration} *)

type enum_mode =
  | Orbit_representatives  (** One canonical representative per D4 orbit *)
  | All_tilings            (** Every tiling from {!Schrot.enum} *)
  | Progressive            (** Orbit representatives first; all tilings
                               only if no counterexample found *)

type counter_policy =
  | Stop_at_first  (** Halt as soon as one counterexample is found *)
  | Collect_all    (** Gather every counterexample *)

(** {1 Properties} *)

(** A property to check, with a label for diagnostics.
    The predicate receives a labeled tiling ([int Schrot.tiling])
    where leaves are numbered 0..n-1 left-to-right. *)
type property

val property : string -> (Tiling.t -> bool) -> property
(** [property label pred] creates a named property. *)

(** {2 Combinators} *)

val conj : property list -> property
(** [conj ps] holds when all [ps] hold. The label of the first
    failing sub-property is reported in the counterexample. *)

val disj : property list -> property
(** [disj ps] holds when at least one of [ps] holds. *)

val imply : property -> property -> property
(** [imply precondition conclusion] checks [conclusion] only on
    tilings where [precondition] holds. Tilings where [precondition]
    fails are counted as skipped (for acceptance ratio). *)

val neg : property -> property
(** [neg p] holds when [p] does not hold. *)

val for_all_tiles : (int -> property) -> property
(** [for_all_tiles (fun n -> p n)] checks [p n] for every tile label
    [n] in the tiling (0..size-1). Short-circuits on first failure. *)

val for_all_dirs : (Tiling.arrow -> property) -> property
(** [for_all_dirs (fun d -> p d)] checks [p d] for each of
    [Left], [Right], [Up], [Down]. Short-circuits on first failure. *)

(** {1 Results} *)

type counterexample = {
  tiling : Tiling.t;
  size : int;
  label : string;  (** Label of the failing sub-property *)
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

val passed : result -> bool
(** [passed r] is [true] iff [r.total_failed = 0]. *)

(** {1 Core checking} *)

val check :
  ?mode:enum_mode ->
  ?policy:counter_policy ->
  ?on_progress:(size_stats -> unit) ->
  max_tiles:int ->
  property ->
  result
(** [check ~max_tiles prop] enumerates all labeled tilings with 1 to
    [max_tiles] tiles and checks [prop] on each.

    @param mode Default {!Progressive}.
    @param policy Default {!Stop_at_first}.
    @param on_progress Called after completing each size. *)

(** {1 Witness finding} *)

val find_witness :
  ?max_tiles:int ->
  property ->
  Tiling.t option
(** [find_witness ~max_tiles prop] returns the first tiling (smallest
    size first) satisfying [prop], or [None]. Always enumerates all
    tilings (not orbit-reduced). *)

val find_all_witnesses :
  ?max_tiles:int ->
  property ->
  Tiling.t list
(** [find_all_witnesses ~max_tiles prop] returns every tiling up to
    [max_tiles] satisfying [prop]. *)

(** {1 Counterexample shrinking} *)

val shrink : property -> counterexample -> counterexample
(** [shrink prop cx] attempts to find a smaller tiling that still
    violates [prop], by closing each tile in turn and re-checking.
    Returns the smallest failing tiling found (may be [cx] itself). *)

(** {1 Formatting} *)

val pp_result : Format.formatter -> result -> unit
val pp_counterexample : Format.formatter -> counterexample -> unit
