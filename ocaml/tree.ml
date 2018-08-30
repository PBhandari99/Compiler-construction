(* A binary tree node is either a leaf of the tree, which has no fields,
 * or a node, which has three fields: a string, a binary tree node,
 * and another binary tree node. *)
open Printf

type btnode =
    | Leaf
    | Node of int * btnode * btnode

let tree = Node(4,
                Node(2,
                    Node(1, Leaf, Leaf),
                    Node(3, Leaf, Leaf)
                    ),
                Node(6,
                    Node(5, Leaf, Leaf),
                    Node(7, Leaf, Leaf)
                    )
                )

let rec inorder (root : btnode) : 'int list =
    match root with
    | Leaf -> []
    | Node(num, left, right) ->
            (inorder left) @ num :: [] @ (inorder right);;

let rec preorder (root : btnode) : 'int list =
    match root with
    | Leaf -> []
    | Node(num, left, right) ->
            num :: [] @ (preorder left) @ (preorder right);;

let rec postorder (root : btnode) : 'int list =
    match root with
    | Leaf -> []
    | Node(num, left, right) ->
            (postorder left) @ (postorder right) @ num :: [];;

(printf "%s\n" "In-order Traversal: ");;
let () = List.iter (printf "%d ") (inorder tree);;
(printf "\n");;
(printf "%s\n" "Pre-order Traversal: ");;
let () = List.iter (printf "%d ") (preorder tree);;
(printf "\n");;
(printf "%s\n" "Post-order Traversal: ");;
let () = List.iter (printf "%d ") (postorder tree);;
(printf "\n");;
