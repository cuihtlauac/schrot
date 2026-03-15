(* Rational numbers: num/den, den > 0, reduced *)
type t = { n: int; d: int }

let rec gcd a b = if b = 0 then a else gcd b (a mod b)

let make n d =
  if d = 0 then invalid_arg "Q.make: zero denominator";
  let sign = if d < 0 then -1 else 1 in
  let n = sign * n and d = sign * d in
  let g = gcd (abs n) d in
  { n = n / g; d = d / g }

let zero = { n = 0; d = 1 }
let one = { n = 1; d = 1 }
let half = { n = 1; d = 2 }

let add a b = make (a.n * b.d + b.n * a.d) (a.d * b.d)
let sub a b = make (a.n * b.d - b.n * a.d) (a.d * b.d)
let mul a b = make (a.n * b.n) (a.d * b.d)
let div a b = make (a.n * b.d) (a.d * b.n)

let equal a b = a.n = b.n && a.d = b.d
let compare a b = Int.compare (a.n * b.d) (b.n * a.d)

let to_float q = float_of_int q.n /. float_of_int q.d

let to_string q =
  if q.d = 1 then string_of_int q.n
  else Printf.sprintf "%d/%d" q.n q.d
