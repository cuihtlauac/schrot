type t =
  | Leaf of int
  | H of t * t * Q.t   (* ratio: fraction of space for first child *)
  | V of t * t * Q.t

let rec to_string = function
  | Leaf n -> string_of_int n
  | H (a, b, r) ->
    if Q.equal r Q.half then
      "h(" ^ to_string a ^ ", " ^ to_string b ^ ")"
    else
      "h[" ^ Q.to_string r ^ "](" ^ to_string a ^ ", " ^ to_string b ^ ")"
  | V (a, b, r) ->
    if Q.equal r Q.half then
      "v(" ^ to_string a ^ ", " ^ to_string b ^ ")"
    else
      "v[" ^ Q.to_string r ^ "](" ^ to_string a ^ ", " ^ to_string b ^ ")"
