(* fib_array : int array -> int array *)
let fib_array arr =
  let rec fib n =
    if n < 2 then n
             else fib (n - 2) + fib (n - 1) in
  let len = Array.length arr in
  let rec loop i =
    if i < len then (arr.(i) <- fib i;
                     loop (i + 1))
               else () in
  (loop 0;
   arr)

let test0 = fib_array [||] = [||]
let test1 = fib_array [|0; 0; 0; 0; 0; 0; 0|] = [|0; 1; 1; 2; 3; 5; 8|]
