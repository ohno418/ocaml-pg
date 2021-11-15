(* Tree type *)
type 'a tree_t = Empty                              (* empty tree *)
               | Leaf of 'a                         (* leaf *)
               | Node of 'a tree_t * 'a * 'a tree_t (* node *)

(* A tree is either of these:
     - `Empty`            : empty tree
     - `Leaf (n)`         : leaf of value n
     - `Node (t1, n, t2)` : node which has t1 as left tree,
                            n as its value, t2 as right tree *)

let tree1 = Empty
let tree2 = Leaf (3)
let tree3 = Node (tree1, 4, tree2)
let tree4 = Node (tree2, 5, tree3)

(* Sum all the integers in a tree. *)
(* sum_tree : int tree_t -> int *)
let rec sum_tree tree = match tree with
    Empty -> 0
  | Leaf (n) -> n
  | Node (t1, n, t2) -> sum_tree t1 + n + sum_tree t2

let test1 = sum_tree tree1 = 0
let test2 = sum_tree tree2 = 3
let test3 = sum_tree tree3 = 7
let test4 = sum_tree tree4 = 15

(* Returns a tree which has double values. *)
(* tree_double : int tree_t -> int tree_t *)
let rec tree_double tree = match tree with
    Empty -> Empty
  | Leaf (n) -> Leaf (n * 2)
  | Node (t1, n, t2) -> Node (tree_double t1, n * 2, tree_double t2)

let test5 = tree_double tree1 = Empty
let test6 = tree_double tree2 = Leaf (6)
let test7 = tree_double tree3 = Node (tree_double tree1, 8, tree_double tree2)
let test8 = tree_double tree4 = Node (tree_double tree2, 10, tree_double tree3)

(* tree_map : ('a -> 'b) -> 'a tree_t -> 'b tree_t *)
let rec tree_map f tree = match tree with
    Empty -> Empty
  | Leaf (n) -> Leaf (f n)
  | Node (t1, n, t2) -> Node (tree_map f t1, f n, tree_map f t2)

let test9 = tree_map (fun a -> a + 2) tree1 = Empty
let test10 = tree_map (fun a -> a + 2) tree2 = Leaf (5)
let test11 = tree_map (fun a -> a + 2) tree3
  = Node (tree_map (fun a -> a + 2) tree1, 6, tree_map (fun a -> a + 2) tree2)
let test12 = tree_map (fun a -> a + 2) tree4
  = Node (tree_map (fun a -> a + 2) tree2, 7, tree_map (fun a -> a + 2) tree3)

(* Returns sum of the number of nodes and leafs. *)
(* tree_length : 'a tree_t -> int *)
let rec tree_length tree = match tree with
    Empty -> 0
  | Leaf (_) -> 1
  | Node (t1, _, t2) -> tree_length t1 + 1 + tree_length t2

let test13 = tree_length tree1 = 0
let test14 = tree_length tree2 = 1
let test15 = tree_length tree3 = 2
let test16 = tree_length tree4 = 4

(* tree_depth : 'a tree_t -> int *)
let rec tree_depth tree = match tree with
    Empty -> 0
  | Leaf (_) -> 0
  | Node (t1, _, t2) -> 1 + max (tree_depth t1) (tree_depth t2)

let test17 = tree_depth tree1 = 0
let test18 = tree_depth tree2 = 0
let test19 = tree_depth tree3 = 1
let test20 = tree_depth tree4 = 2
