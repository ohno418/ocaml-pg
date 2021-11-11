(* quick_sort : int list -> int list *)
let rec quick_sort lst =
  let take_less n lst = List.filter (fun x -> x < n) lst in
  let take_greater n lst = List.filter (fun x -> x > n) lst in
  let take_equal n lst = List.filter (fun x -> x = n) lst in
  match lst with
      [] -> []
    | first :: rest -> quick_sort (take_less first rest)
                       @ take_equal first lst
                       @ quick_sort (take_greater first rest)

let test1 = quick_sort [] = []
let test2 = quick_sort [2] = [2]
let test3 = quick_sort [2; 3] = [2; 3]
let test4 = quick_sort [3; 2] = [2; 3]
let test5 = quick_sort [5; 4; 9; 8; 2; 3] = [2; 3; 4; 5; 8; 9]
let test6 = quick_sort [5; 4; 9; 5; 8; 2; 3] = [2; 3; 4; 5; 5; 8; 9]
let test7 = quick_sort [5; 4; 9; 5; 8; 9; 2; 3] = [2; 3; 4; 5; 5; 8; 9; 9]
