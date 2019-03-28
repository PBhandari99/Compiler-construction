type ('k, 'v) avlnode =
  | Leaf
  | Node of int * 'k * 'v * ('k, 'v) avlnode * ('k, 'v) avlnode

(* Given a key return a value for that key *)
let rec get (n : ('k, 'v) avlnode) (key : 'k) : 'v option =
  match n with
  | Leaf -> None
  | Node (_, k, v, left, right) ->
      if k = key then Some v
      else if key < k then get left key
      else get right key

(* A useful helper function (you may need to write more) *)
let balance (n : ('k, 'v) avlnode) : ('k, 'v) avlnode = Leaf

(* Produce a new avltree that contains the given key. If the key already
   exists, update the value to the new value *)
let rec set (n : ('k, 'v) avlnode) (key : 'k) (value : 'v) : ('k, 'v) avlnode =
  match n with
  | Leaf -> Node (0, key, value, Leaf, Leaf)
  | Node (h, k, v, left, right) ->
      if k = key then Node (h, k, value, left, right)
      else if key < k then Node (h, k, v, set left key value, right)
      else Node (h, k, v, left, set right key value)

(* Return a list of tuples containing the elements of the tree *)
let rec inorder (n : ('k, 'v) avlnode) : ('k * 'v) list =
  match n with
  | Leaf -> []
  | Node (_, k, v, left, right) -> inorder left @ [(k, v)] @ inorder right

(* Write the functions below (contains, height, add_all, sum) without using
any recursion.  Use the functions above, and map/filter/fold if necessary, to
build up the behavior from existing functions. *)
let contains (n : ('k, 'v) avlnode) (key : 'k) : bool =
  let ls = List.map (fun a -> match a with k, v -> k) (inorder n) in
  List.fold_left (fun x acc -> acc || x == key) false ls

let height (n : ('k, 'v) avlnode) : int =
  match n with Leaf -> 0 | Node (h, _, _, _, _) -> h

(* Given an AVL tree and a list of key/value tuples, set all the given keys
(first pair components) to the corresponding value (second pair components) *)
let add_all (n : ('k, 'v) avlnode) (keys : ('k * 'v) list) : ('k, 'v) avlnode =
  let new_set (key : 'k * 'v) : ('k, 'v) avlnode = set n (fst key) (snd key) in
  let mutated_nodes = List.map new_set keys in
  match mutated_nodes with [] -> Leaf | f :: l -> f

(* let a = List.map (set n) keys in *)

(* Return the total value of all the integers in a tree that has int values *)
let sum (n : ('k, int) avlnode) : int =
  let ls = List.map (fun a -> match a with k, v -> v) (inorder n) in
  List.fold_left (fun x acc -> acc + x) 0 ls
