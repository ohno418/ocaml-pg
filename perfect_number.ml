(* Returns list from 1 to n. *)
(* enumerate : int -> int list *)
let rec enumerate n =
  if n = 0 then []
           else n :: enumerate (n - 1)

(* Returns list of divisors. *)
(* divisor : int -> int list *)
let divisor n = List.filter (fun d -> n mod d = 0) (enumerate n)

let test1 = divisor 6 = [6; 3; 2; 1]
let test2 = divisor 14 = [14; 7; 2; 1]
let test3 = divisor 17 = [17; 1]

(* Returns list of perfect numbers, less than equal to m. *)
(* perfect : int -> int list *)
let perfect m =
  List.filter
    (fun n -> List.fold_right (+) (divisor n) 0 - n = n)
    (enumerate m)

let test4 = perfect 5 = []
let test5 = perfect 6 = [6]
let test6 = perfect 7 = [6]
let test7 = perfect 30 = [28; 6]
let test8 = perfect 500 = [496; 28; 6]
let test9 = perfect 10000 = [8128; 496; 28; 6]
