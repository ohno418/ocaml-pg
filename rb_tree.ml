(* Red-black Tree *)

(* color type *)
type color_t = Red | Black

(* Red-black tree type
   that has key ('a), value ('b), and color type. *)
type ('a, 'b) rb_tree_t = Empty
               | Node of ('a, 'b) rb_tree_t * 'a * 'b * color_t * ('a, 'b) rb_tree_t

(* Blance tree. *)
(* blance : ('a, 'b) rb_tree_t -> ('a, 'b) rb_tree_t *)
let blance tree = match tree with
  | Node (Node (Node (t0, k0, v0, Red, t1), k1, v1, Red, t2), k2, v2, Black, t3)
  | Node (Node (t0, k0, v0, Red, Node (t1, k1, v1, Red, t2)), k2, v2, Black, t3)
  | Node (t0, k0, v0, Black, Node (Node (t1, k1, v1, Red, t2), k2, v2, Red, t3))
  | Node (t0, k0, v0, Black, Node (t1, k1, v1, Red, Node (t2, k2, v2, Red, t3)))
      -> Node(Node(t0, k0, v0, Black, t1), k1, v1, Red, Node(t2, k2, v2, Black, t3))
  | _ -> tree

let t0 = Node (Empty, 3, 4, Red, Empty)
let t1 = Node (Node(Empty, 2, 3, Red, Empty),
               3, 4, Black,
               Empty)
let t2 = Node (Node(Node (Empty, 1, 2, Red, Empty),
                    2, 3, Red,
                    Empty),
               3, 4, Black,
               Empty)
let test0 = blance Empty = Empty
let test1 = blance t0 = t0
let test2 = blance t1 = t1
let test3 = blance t2 = Node (Node (Empty, 1, 2, Black, Empty),
                              2, 3, Red,
                              Node (Empty, 3, 4, Black, Empty))

(* Insert a key and value pair into the tree. *)
(* insert : ('a, 'b) rb_tree_t -> 'a -> 'b -> ('a, 'b) rb_tree_t *)
let insert tree key value =
  let rec insert0 t0 k0 v0 = match t0 with
      Empty -> Node (Empty, k0, v0, Red, Empty)
    | Node (left, k, v, c, right) ->
        let t0 =
          if k0 < k then Node ((insert0 left k0 v0), k, v, c, right)
          else if k < k0 then Node (left, k, v, c, (insert0 right k0 v0))
          else Node (left, k, v0, c, right) in
        blance t0 in
  let t0 = insert0 tree key value in
  match t0 with
      Empty
    | Node (_, _, _, Black, _) -> t0
    | Node (left, k, v, Red, right) ->
        match (left, right) with
            (Node (_, _, _, Red, _), _)
          | (_, Node (_, _, _, Red, _)) -> Node (left, k, v, Black, right)
          | _ -> t0

let t0 = insert Empty "ab" 12
let t1 = insert t0 "bc" 23
let t2 = insert t1 "cd" 34
let t3 = insert t2 "de" 45
let t4 = insert t3 "cb" 89
let t5 = insert t4 "ca" 90
let test0 = t0 = Node (Empty, "ab", 12, Red, Empty)
let test1 = t1 = Node (Empty, "ab", 12, Black, Node (Empty, "bc", 23, Red, Empty))
let test2 = t2 = Node (Node (Empty, "ab", 12, Black, Empty), "bc", 23, Red, Node (Empty, "cd", 34, Black, Empty))
let test3 = t3 = Node (Node (Empty, "ab", 12, Black, Empty), "bc", 23, Red, Node (Empty, "cd", 34, Black, Node (Empty, "de", 45, Red, Empty)))
let test4 = t4 = Node (Node (Empty, "ab", 12, Black, Empty), "bc", 23, Red, Node (Node (Empty, "cb", 89, Red, Empty), "cd", 34, Black, Node (Empty, "de", 45, Red, Empty)))
let test5 = t5 = Node (Node (Empty, "ab", 12, Black, Empty),
                       "bc", 23, Black,
                       Node (Node (Empty, "ca", 90, Black, Empty), "cb", 89, Red, Node (Empty, "cd", 34, Black, Node (Empty, "de", 45, Red, Empty))))
