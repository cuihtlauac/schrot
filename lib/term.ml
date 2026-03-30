(* TODO: Bring up to Schroder tilings *)
type t =
  | Leaf of int
  | H of t * t
  | V of t * t

let rec to_string = function
  | Leaf n -> string_of_int n
  | H (a, b) -> "h(" ^ to_string a ^ ", " ^ to_string b ^ ")"
  | V (a, b) -> "v(" ^ to_string a ^ ", " ^ to_string b ^ ")"
