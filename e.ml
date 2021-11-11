(* dai_n_kou : int -> float *)
let rec dai_n_kou n =
  if n = 0 then 1.0
           else dai_n_kou (n - 1) /. float_of_int n

(* Returns approximation of e (Euler's number). *)
(* e : int -> float *)
let rec e n =
  let d = dai_n_kou n in
  if d < 0.00001 then d
                 else d +. e (n + 1)

(* To execute, `e 0`. *)
