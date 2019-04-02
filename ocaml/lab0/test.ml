open OUnit2
open Functions

(* This file contains some example tests.  Feel free to delete and reorganize
   the unnecessary parts of this file; it is provided to match up with the given
   writeup initially. *)

let check_fun _ =
  (* a function of one argument containing a successful test *)
  assert_equal (2 + 2) 4

let check_fun2 _ =
  (* a failing test *)
  assert_equal (2 + 2) 5

(* a helper for testing integers *)
let t_int name value expected =
  name >:: fun _ -> assert_equal expected value ~printer:string_of_int

let t_string name value expected =
  name >:: fun _ -> assert_equal expected value ~printer:(fun s -> s)

let int_list_to_string (l : int list) : string =
  let rec to_string (l : int list) : string =
    match l with
    | [] -> ""
    | first :: rest -> string_of_int first ^ "; " ^ to_string rest
  in
  "[ " ^ to_string l ^ "]"

let str_list_to_string (l : string list) : string =
  let rec to_string (l : string list) : string =
    match l with [] -> "" | first :: rest -> first ^ "; " ^ to_string rest
  in
  "[ " ^ to_string l ^ "]"

let t_list name value expected conv_func =
  name >:: fun _ -> assert_equal expected value ~printer:conv_func

let my_first_test = "my_first_test" >:: check_fun

let my_second_test = "my_second_test" >:: check_fun2

let my_third_test = t_int "my_third_test" (2 + 2) 7

let my_fourth_test = t_int "my_fourth_test" (2 + 2) 4

let fib_test = t_int "fib_test" (fibonacci 6) 8

let string_test = t_string "string_test" "hello" "hello"

let tree1 = Leaf

let tree2 = Node ("a", Leaf, Leaf)

let tree3 = Node ("b", Node ("a", Leaf, Leaf), Leaf)

let tree4 = Node ("b", Node ("a", Leaf, Leaf), Node ("c", Leaf, Leaf))

let tree5 = Node ("c", Node ("b", Node ("a", Leaf, Leaf), Leaf), Leaf)

let inorder_test_1 = t_string "inorder_test_1" (inorder tree1) ""

let inorder_test_2 = t_string "inorder_test_2" (inorder tree2) "a"

let inorder_test_3 = t_string "inorder_test_3" (inorder tree3) "ab"

let inorder_test_4 = t_string "inorder_test_4" (inorder tree4) "abc"

let inorder_test_5 = t_string "inorder_test_4" (inorder tree5) "abc"

let size_test_1 = t_int "size_test_1" (size_nodes tree4) 3

let size_test_2 = t_int "size_test_2" (size_nodes tree1) 0

let size_test_3 = t_int "size_test_3" (size_nodes tree2) 1

let size_test_4 = t_int "size_test_4" (size_nodes tree3) 2

let size_test_5 = t_int "size_test_5" (size_nodes tree5) 3

let height_test_1 = t_int "height_test_1" (height tree1) 0

let height_test_2 = t_int "height_test_2" (height tree2) 1

let height_test_3 = t_int "height_test_3" (height tree3) 2

let height_test_4 = t_int "height_test_4" (height tree4) 2

let height_test_5 = t_int "height_test_5" (height tree5) 3

let list_test_1 =
  t_list "list_test_1"
    (increment_all [1; 2; 3; 4])
    [2; 3; 4; 5] int_list_to_string

let list_test_2 = t_list "list_test_2" (increment_all []) [] int_list_to_string

let long_strings_1 =
  t_list "long_string_1" (long_strings [] 1) [] str_list_to_string

let long_strings_2 =
  t_list "long_string_2"
    (long_strings ["hello"; "gold"; "farmer"; "catches"; "fish"] 5)
    ["farmer"; "catches"] str_list_to_string

let every_string_test_1 =
  t_list "every_string_test_1"
    (every_other ["hello"; "gold"; "farmer"; "catches"; "fish"])
    ["hello"; "farmer"; "fish"]
    str_list_to_string

let every_string_test_2 =
  t_list "every_string_test_2"
    (every_other ["hello"])
    ["hello"] str_list_to_string

let every_string_test_3 =
  t_list "every_string_test_3" (every_other []) [] str_list_to_string

let sum_all_test_1 =
  "sum_all_test_1"
  >:: fun _ -> assert_equal (sum_all [[1; 2; 3; 3]; [3; 2; 53; 54]]) [9; 112]

let sum_all_test_2 = "sum_all_test_2" >:: fun _ -> assert_equal (sum_all []) []

let sum_all_test_3 =
  "sum_all_test_3" >:: fun _ -> assert_equal (sum_all [[]]) [0]

let suite =
  "suite"
  >::: [ my_first_test
       ; fib_test
       ; string_test
       ; inorder_test_1
       ; inorder_test_2
       ; inorder_test_3
       ; inorder_test_4
       ; inorder_test_5
       ; size_test_1
       ; size_test_2
       ; size_test_3
       ; size_test_4
       ; size_test_5
       ; height_test_1
       ; height_test_2
       ; height_test_3
       ; height_test_4
       ; height_test_5
       ; list_test_1
       ; list_test_2
       ; long_strings_1
       ; long_strings_2
       ; every_string_test_1
       ; every_string_test_2
       ; every_string_test_3
       ; sum_all_test_1
       ; sum_all_test_2
       ; sum_all_test_3 ]

let _ = run_test_tt_main suite
