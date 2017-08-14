(* $Id: bigint.ml,v 1.5 2014-11-11 15:06:24-08 - - $ *)

(* 
Arya Kashani
akashani
1474098
*)

open Printf

module Bigint = struct

    type sign     = Pos | Neg
    type bigint   = Bigint of sign * int list
    let  radix    = 10
    let  radixlen =  1

    let car       = List.hd
    let cdr       = List.tl
    let map       = List.map
    let reverse   = List.rev
    let strcat    = String.concat
    let strlen    = String.length
    let strsub    = String.sub
    let zero      = Bigint (Pos, [])

    let charlist_of_string str = 
        let last = strlen str - 1
        in  let rec charlist pos result =
            if pos < 0
            then result
            else charlist (pos - 1) (str.[pos] :: result)
        in  charlist last []

    let bigint_of_string str =
        let len = strlen str
        in  let to_intlist first =
                let substr = strsub str first (len - first) in
                let digit char = int_of_char char - int_of_char '0' in
                map digit (reverse (charlist_of_string substr))
            in  if   len = 0
                then zero
                else if   str.[0] = '_'
                     then Bigint (Neg, to_intlist 1)
                     else Bigint (Pos, to_intlist 0)

    let string_of_bigint (Bigint (sign, value)) =
        match value with
        | []    -> "0"
        | value -> let reversed = reverse value
                   in  strcat ""
                       ((if sign = Pos then "" else "-") ::
                        (map string_of_int reversed))

    let trimzeros list =
        let rec trimzeros' list' = match list' with
            | []       -> []
            | [0]      -> []
            | car::cdr ->
                let cdr' = trimzeros' cdr
                in  match car, cdr' with
                    | 0, [] -> []
                    | car, cdr' -> car::cdr'
        in trimzeros' list

let rec cmp' list1 list2 = match (list1, list2) with
    | [], []          -> 0 
    | list1, list2 when (car list1 < car list2)    -> -1
    | list1, list2 when (car list1 > car list2)    -> 1
    | list1, list2    -> cmp' (cdr list1) (cdr list2)

let cmp list1 list2 =
        if ((List.length list1) < (List.length list2))
          then -1
        else if ((List.length list1) > (List.length list2))
          then 1
        else cmp' (List.rev list1) (List.rev list2)


    let rec add' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 + car2 + carry
          in  sum mod radix :: add' cdr1 cdr2 (sum / radix)

     let rec sub' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
(*        | [], list2, 0       -> list2  *)
        | list1, [], carry   -> trimzeros (sub' list1 [carry] 0)
        | [], list2, carry   -> trimzeros (sub' [carry] list2 0)
        | car1::cdr1, car2::cdr2, carry ->
          if (car1 - carry) < car2
          then let diff = (car1 + 10) - (car2 + carry)
               in diff :: trimzeros (sub' cdr1 cdr2 1)
          else let diff = car1 - car2 - carry
               in diff :: trimzeros (sub' cdr1 cdr2 0)

    let add (Bigint (sign1, value1)) (Bigint (sign2, value2)) =
        if sign1 = sign2
        then Bigint (sign1, add' value1 value2 0)
        else if sign2 = Neg && ((cmp value1 value2) >= 0)
             then Bigint (Pos, trimzeros (sub' value1 value2 0))
        else if sign2 = Neg && ((cmp value2 value1) >= 0)
             then Bigint (Neg, trimzeros (sub' value2 value1 0))
        else if sign1 = Neg && ((cmp value2 value1) >= 0)
             then Bigint (Pos, trimzeros (sub' value2 value1 0))
        else Bigint (Neg, trimzeros (sub' value1 value2 0))

    let sub (Bigint (sign1, value1)) (Bigint (sign2, value2)) =
        if sign1 = Pos && sign2 = Pos
        then if (cmp value1 value2) >= 0
             then Bigint(Pos, trimzeros (sub' value1 value2 0))
             else Bigint(Neg, trimzeros (sub' value2 value1 0))
        else if sign1 = Pos && sign2 = Neg
             then Bigint (Pos, add' value1 value2 0)
        else if sign1 = Neg && sign2 = Pos
             then Bigint (Neg, add' value1 value2 0)
        else if (cmp value1 value2) >= 0
             then Bigint (Neg, trimzeros (sub' value1 value2 0))
        else Bigint (Pos, trimzeros (sub' value2 value1 0))

    let double number = add' number number 0

    let rec mul' (multiplier, powerof2, multiplicand') =
        if (cmp powerof2 multiplier) = 1
        then multiplier, []
        else let remainder, product = 
          mul' (multiplier, double powerof2, double multiplicand')
              in  if (cmp remainder powerof2) = -1
                  then remainder, product
                  else (trimzeros (sub' remainder powerof2 0)), 
                    (add' product multiplicand' 0)

    let mulhelper (multiplier, po2, multiplicand) = 
        let _, product = mul' (multiplier, po2, multiplicand)
        in product

    let mul (Bigint (sign1, value1)) (Bigint (sign2, value2)) =
        if sign1 = sign2
        then (if (cmp value1 value2) >= 0
             then Bigint (Pos, mulhelper (value1, [1], value2))
             else Bigint (Pos, mulhelper (value2, [1], value1)))
        else (if (cmp value1 value2) >= 0
              then Bigint (Neg, mulhelper (value1, [1], value2))
              else Bigint (Neg, mulhelper (value2, [1], value1)))

    let rec divrem' (dividend, powerof2, divisor') =
        if (cmp divisor' dividend) = 1
        then [], dividend
        else let quotient, remainder = 
          divrem' (dividend, double powerof2, double divisor')
             in  if (cmp remainder divisor') = -1
                 then quotient, remainder
                 else ((add' quotient powerof2 0), 
                    trimzeros (sub' remainder divisor' 0))

    let divhelper (dividend, divisor) =
        let quotient, _ = divrem' (dividend, [1], divisor)
        in quotient

    let div (Bigint (sign1, dividend)) (Bigint (sign2, divisor)) =
        if sign1 = sign2
        then Bigint (Pos, divhelper (dividend, divisor))
        else Bigint (Neg, divhelper (dividend, divisor))

    let remhelper (dividend, divisor) =
        let _, remainder = divrem' (dividend, [1], divisor)
        in remainder

    let rem (Bigint (sign1, dividend)) (Bigint (sign2, divisor)) =
        if sign1 = sign2
        then Bigint (Pos, remhelper (dividend, divisor))
        else Bigint (Neg, remhelper (dividend, divisor))

    let even number = 
        let remainder = trimzeros (remhelper (number, [2])) in 
            if (cmp remainder []) = 0
            then true
            else false 

    let rec power' (base, expt, result) =
        if (cmp expt []) = 0
            then result
        else if even expt
            then (power' (mulhelper (base, [1], base), 
                divhelper (expt, [2]), result))
        else (power' (base, trimzeros (sub' expt [1] 0), 
            mulhelper (base, [1], result)))

    let pow (Bigint (sign1, base)) (Bigint (sign2, expt)) =
        if sign2 = Neg
            then zero
        else if sign1 = Pos 
            then Bigint (Pos, power' (base, expt, [1]))
        else if even expt
            then Bigint (Pos, power' (base, expt, [1]))
        else  Bigint (Neg, power' (base, expt, [1]))
        
    

end










