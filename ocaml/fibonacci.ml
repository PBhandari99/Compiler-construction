(* print nth fibonacci integer *)

open Printf

let rec get_fib (num : int) : int =
    if num == 1 then 1
    else if num == 2 then 1
    else
        (get_fib (num - 1)) + (get_fib (num - 2));;

(printf "%d\n" (get_fib 4));

