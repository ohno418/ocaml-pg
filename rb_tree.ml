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
      -> Node(Node(t0, k0, v0, Red, t1), k1, v1, Black, Node(t2, k2, v2, Red, t3))
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
let test3 = blance t2 = Node (Node (Empty, 1, 2, Red, Empty),
                                 2, 3, Black,
                                 Node (Empty, 3, 4, Red, Empty))
