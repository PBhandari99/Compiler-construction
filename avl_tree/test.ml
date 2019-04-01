open OUnit2
open Avl
open Printf
open Expr
open ExtLib

(* A helper for testing primitive values (won't print datatypes well) *)
let t_any name value expected =
  name >:: fun _ -> assert_equal expected value ~printer:dump

let tuple_to_string (ls : ('k * 'v) list) : string =
  let str =
    List.fold_left
      (fun acc x -> acc ^ "(" ^ fst x ^ ", " ^ string_of_int (snd x) ^ "); ")
      "[" ls
  in
  str ^ "]"

let t_inorder name value expected =
  name >:: fun _ -> assert_equal expected value ~printer:tuple_to_string

let a_tree = Node (0, "a", 5, Leaf, Leaf)

(* this tree has structure:
 *                          4
 *                        /   \
 *                       2     6
 *                     /  \   /  \
 *                    1    3 5    7
 *)
let b_tree =
  Node
    ( 2
    , "d"
    , 4
    , Node
        (1, "b", 2, Node (0, "a", 1, Leaf, Leaf), Node (0, "c", 3, Leaf, Leaf))
    , Node
        (1, "f", 6, Node (0, "e", 5, Leaf, Leaf), Node (0, "g", 7, Leaf, Leaf))
    )

let key_value_list = [(3, "c"); (8, "h"); (12, "l")]

(* It can be useful to aggregate tests into lists if they test separate
functions, and put them together at the end *)

let get_tests =
  [ t_any "get1" (get a_tree "a") (Some 5)
  ; t_any "get2" (get (Node (0, "b", 15, a_tree, Leaf)) "a") (Some 5)
  ; t_any "get3" (get (Node (0, "b", 15, a_tree, Leaf)) "c") None ]

let inorder_tests =
  [ t_inorder "inorder1" (inorder a_tree) [("a", 5)]
  ; t_inorder "inorder2" (inorder b_tree)
      [("a", 1); ("b", 2); ("c", 3); ("d", 4); ("e", 5); ("f", 6); ("g", 7)] ]

let contains_tests =
  [ t_any "contains1" (contains a_tree "c") false
  ; t_any "constains2" (contains b_tree "e") true
  ; t_any "constains3" (contains b_tree "b") true
  ; t_any "constains4" (contains b_tree "n") false ]

let evaluate_tests =
  [t_any "evaluate1" (evaluate (Times (Num 0, Num 5)) Leaf) 0]

(* let add_all_tests = [t_any "add_all1" (inorder (add_all b_tree key_value_list))] *)

let sum_tests =
  [ t_any "sum1" (sum Leaf) 0
  ; t_any "sum2" (sum a_tree) 5
  ; t_any "sum3" (sum b_tree) 28 ]

let all_tests =
  get_tests @ contains_tests @ evaluate_tests @ sum_tests @ inorder_tests

(* Your additional tests go here *)

let suite = "suite" >::: all_tests

;;
run_test_tt_main suite
