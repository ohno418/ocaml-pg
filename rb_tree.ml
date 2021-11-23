(* Red-black Tree *)

(* color type *)
type color_t = Red | Black

(* Red-black tree type
   that has key ('a), value ('b), and color type. *)
type ('a, 'b) rb_tree_t = Empty
               | Node of ('a, 'b) rb_tree_t * 'a * 'b * color_t * ('a, 'b) rb_tree_t
