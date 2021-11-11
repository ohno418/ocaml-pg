(* Take elements from first to mid. *)
(* take_left : int -> int lst -> int lst *)
let take_left mid lst = List.filteri (fun i ele -> i < mid) lst

(* Take elements from next of mid to end. *)
(* take_right : int -> int lst -> int lst *)
let take_right mid lst = List.filteri (fun i ele -> i >= mid) lst

(* Take 2 sorted lists, and return merged list. *)
(* merge : int list -> int list -> int list *)
let rec merge l r = match l with
    [] -> r
  | l_first :: l_rest ->
      match r with
          [] -> l
        | r_first :: r_rest ->
            if l_first <= r_first
            then l_first :: (merge l_rest r)
            else r_first :: (merge l r_rest)

(* merge_sort : int lst -> int lst *)
let rec merge_sort lst =
  let len = List.length lst in
  if len <= 1
  then lst
  else
    let mid = len / 2 in
    let left = take_left mid lst in
    let right = take_right mid lst in
    let sorted_left = merge_sort left in
    let sorted_right = merge_sort right in
    merge sorted_left sorted_right

let test1 = merge_sort [] = []
let test2 = merge_sort [2] = [2]
let test3 = merge_sort [2; 3] = [2; 3]
let test4 = merge_sort [3; 2] = [2; 3]
let test5 = merge_sort [3; 4; 1; 7; 9; 8; 2] = [1; 2; 3; 4; 7; 8; 9]
let test6 = merge_sort [3; 4; 1; 7; 1; 9; 8; 2] = [1; 1; 2; 3; 4; 7; 8; 9]
