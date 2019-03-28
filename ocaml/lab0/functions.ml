open Printf

let rec fibonacci (n : int) : int =
  if n <= 1 then n else fibonacci (n - 1) + fibonacci (n - 2)

type btnode = Leaf | Node of string * btnode * btnode

let rec inorder (root : btnode) : string =
  match root with
  | Leaf -> ""
  | Node (s, left, right) -> inorder left ^ s ^ inorder right

let rec size_nodes (node : btnode) : int =
  match node with
  | Leaf -> 0
  | Node (s, left, right) -> 1 + size_nodes left + size_nodes right

let rec height (node : btnode) : int =
  match node with
  | Leaf -> 0
  | Node (s, left, right) -> 1 + max (height left) (height right)

let rec increment_all (l : int list) : int list =
  match l with [] -> [] | first :: rest -> [first + 1] @ increment_all rest

let rec long_strings (l : string list) (length : int) : string list =
  match l with
  | [] -> []
  | first :: rest ->
      if String.length first > length then [first] @ long_strings rest length
      else long_strings rest length

let rec every_other (l : 'a list) : 'a list =
  match l with
  | [] -> []
  | [first] -> [first]
  | first :: second :: rest -> [first] @ every_other rest

let rec sum_all (l : int list list) : int list =
  match l with
  | [] -> []
  | first :: rest ->
      let rec sum_list (int_list : int list) : int =
        match int_list with [] -> 0 | head :: last -> head + sum_list last
      in
      [sum_list first] @ sum_all rest
