(* $Id: fibonacci.ml,v 342.1 2004-03-16 17:03:57-08 - - $ *)
(*
* Fibonacci function.
* Done three ways:
* - O(2^n), which is bad
* - O(n), which is good
* - mathematically, which is clever
*)

let rec fib2n = function
    | 0 -> 0
    | 1 -> 1
    | n when n > 1 -> fib2n (n - 1) + fib2n (n - 2)
    | n            -> invalid_arg (string_of_int n)
    ;;

let fib n =
    let rec fib' n a b =
        if n = 0 then a
                 else fib' (n - 1) b (a + b)
    in  if n < 0 then invalid_arg ("fib " ^ string_of_int n)
                 else fib' n 0 1
    ;;

let fibmath n =
    let root5   = sqrt 5.             and
        floatn  = float n             in
    let phi     = (1. +. root5) /. 2. and
        psi     = (1. -. root5) /. 2.
    in  (phi ** floatn -. psi ** floatn) /. root5
    ;;

