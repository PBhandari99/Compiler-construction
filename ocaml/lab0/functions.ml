(* Fill in the functions you need to write here *)

open Printf

let rec fibonacci (n : int) : int = 
  if n <= 1 then n
  else ((fibonacci (n-1)) + (fibonacci (n-2)));;

type btnode =
  | Leaf
  | Node of string * btnode * btnode;;

let rec inorder (root : btnode) : string = 
  match root with
  | Leaf -> ""
  | Node(s, left, right) ->
    (inorder left) ^ s ^ (inorder right);;

let rec size_nodes (node : btnode) : int =
  match node with
  | Leaf -> 0
  | Node(s, left, right) ->
    1 + (size_nodes left) + (size_nodes right);;

let tree1 = Leaf;;
let tree2 = Node("a", Leaf, Leaf);;

(printf "5th fib is: %d\n" (fibonacci 6));;


