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
            (inorder left) @ num :: [] @ (inorder right)

let () = List.iter (printf "%d ") (inorder tree)
