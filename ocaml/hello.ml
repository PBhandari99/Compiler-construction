(* some simple ocaml programs. *)

open Printf

let message = "Hello world!"

;;
printf "%s\n" message

let square (n : int) : int = n * n

;;
printf "%d\n" (square 3)

let max (n : int) (m : int) : int = if n > m then n else m

;;
printf "%d\n" (max 3 4)

(* the rec keyword tells the compiler that this is recursive so that it  *)
(* doesn't complain about the scoping. *)
let rec sum_up (n : int) : int =
  if n < 0 then failwith "Can't solve for negative int.\n"
  else if n <= 1 then n
  else sum_up (n - 1) + n

;;
printf "sum upto %d: %d" 10 (sum_up 10)
