(* fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a *)
let rec fold_left f init lst = match lst with
    [] -> init
  | first :: rest -> fold_left f (f init first) rest

let test1 = fold_left (fun a b -> a * b) 0 [] = 0
let test2 = fold_left (fun a b -> a * b) 1 [2; 6; 3] = 36
