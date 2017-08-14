(* $Id: factorial.ml,v 361.3 2014-11-17 14:03:17-08 - - $ *)
(* Factorial example. *)

open Printf;;

(*
let rec fac = function
    | 0 -> 1
    | n when n > 0 -> n * fac (n - 1)
    | n -> invalid_arg ("fac (" ^ (string_of_int n) ^ ")");;
*)

let fac n =
    let rec fac' n' r' = match n' with
        | 0 -> r'
        | n -> fac' (n' - 1) (n' * r')
    in  if n < 0 then invalid_arg ("fac (" ^ (string_of_int n) ^ ")")
                 else fac' n 1;;
    

let printfac n = (printf "fac %d = %d\n" n (fac n);
                  flush stdout);;

printfac 0;;
printfac 1;;
printfac 2;;
printfac 5;;
printfac 10;;
printfac 20;;
printfac (-5);;

