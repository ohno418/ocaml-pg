(* input  : m >= n >= 0
   output : GCD of m and n *)
(* gcd : int -> int -> int *)
let rec gcd m n =
  if n = 0
  then m
  else gcd n (m mod n)

let test1 = gcd 6 0 = 6
let test2 = gcd 6 2 = 2
let test3 = gcd 8 5 = 1
let test4 = gcd 54 24 = 6
