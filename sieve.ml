(* non_divisible : int -> int list -> int list *)
let rec non_divisible n lst = match lst with
    [] -> []
  | first :: rest ->
      if first mod n = 0 then non_divisible n rest
                         else first :: non_divisible n rest

(* Takes integer (2 <= element <= n) list,
   and returns list of prime numbers. *)
(* sieve : int list -> int list *)
let rec sieve lst = match lst with
    [] -> []
  | first :: rest -> first :: sieve (non_divisible first rest)

let test1 = sieve [] = []
let test2 = sieve [2] = [2]
let test3 = sieve [2; 3; 4; 5; 6; 7; 8; 9] = [2; 3; 5; 7]
