(*------------------------------------------------------------------
 *
 * James Vrionis
 * Luke Tanner
 * CMPS112
 * ASG2
 *
 *------------------------------------------------------------------*)

open Printf

module Bigint = struct

    type sign     = Pos | Neg
    type bigint   = Bigint of sign * int list
    let  radix    = 10
    let  radixlen =  1
(*------------------------------------------------------------------*)

    let car   = List.hd
    let cdr   = List.tl
    let map   = List.map
    let reverse   = List.rev
    let strcat    = String.concat
    let strlen    = String.length
    let strsub    = String.sub
    let zero      = Bigint (Pos, [])

(*--------------------------------------------------------------------
  Char list of Strings:
--------------------------------------------------------------------*)
    let charlist_of_string str =
        let last = strlen str - 1
        in  let rec charlist pos result =
            if pos < 0
            then result
            else charlist (pos - 1) (str.[pos] :: result)
        in  charlist last []
    ;;


(*--------------------------------------------------------------------
   BigInt of strings (in-order):   
--------------------------------------------------------------------*)
    let bigint_of_string str =
        let len = strlen str
        in  let to_intlist first =
                let substr = strsub str first (len - first) in
                let digit char = int_of_char char - int_of_char '0' in
                map digit (reverse (charlist_of_string substr))
            in  if   len = 0 then zero
                else if   str.[0] = '_'
                     then Bigint (Neg, to_intlist 1)
                     else Bigint (Pos, to_intlist 0)
    ;;

(*--------------------------------------------------------------------
   BigInt of strings (reversed):
--------------------------------------------------------------------*)
    let string_of_bigint (Bigint (sign, value)) =
        match value with
        | []    -> "0"
        | value -> let reversed = reverse value
                   in  strcat ""
                       ((if sign = Pos then "" else "-") ::
                        (map string_of_int reversed))
    ;;


(*--------------------------------------------------------------------
  Recursive Subtraction Function:
   (CTF) stands for call to function.
--------------------------------------------------------------------*)
   let rec sub' list1 list2 steal = match (list1, list2, steal) with
     | list1, [], 0 -> list1
     | [], list2, 0 -> failwith "Valid only if list2 > list"
     | car1::cdr1, [], steal  -> 
         if car1 = 0 then 9 :: (sub' cdr1 [] 1)
         else let dif = car1 - steal*1 in dif :: (sub' cdr1 [] 0)
     | [], list2, steal -> failwith "Err in sub':Invalid CTF"
     | car1::cdr1, car2::cdr2, steal ->
         if car2 > (car1 - steal*1) then 
         let dif = ((car1 + 10) - steal*1) - car2 
                 in dif :: (sub' cdr1 cdr2 1)
         else let dif = (car1 - steal*1) - car2
                 in dif :: (sub' cdr1 cdr2 0)

(*--------------------------------------------------------------------
    Recursive Add():
--------------------------------------------------------------------*)
    let rec add' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 + car2 + carry
          in  sum mod radix :: add' cdr1 cdr2 (sum / radix)


(*--------------------------------------------------------------------
    Recursive sub():
--------------------------------------------------------------------*)
    let rec sub' list1 list2 borrow = match(list1, list2, borrow) with
        | [], _, _           -> []
        | list1, [], 0       -> list1
        | list1, [], borrow   -> sub' list1 [borrow] 0
        | car1::cdr1, car2::cdr2, borrow ->
          let dif = car1 - borrow - car2  
          in (if dif < 0 then dif + 10 :: sub' cdr1 cdr2 1
            else dif :: sub' cdr1 cdr2 0) 

(*--------------------------------------------------------------------
  Compare Recursively Defined:
   Bool -> Bool ->  Bool
   && is LR: e1 && e2, e1 is evaluated first (if false) e2 is not 
   evaluated
--------------------------------------------------------------------*)
    let rec cmp' list1 list2 = match (list1, list2) with  
        | [], []                 ->  0
        | list1, []              ->  1
        | [], list2              -> -1
        | car1::cdr1, car2::cdr2 ->
            let result = cmp' cdr1 cdr2
            in if result = 0 && car1 <> car2 then 
               if car1 > car2 then 1 
               else if car1 < car2 then -1
               else 0 
            else result
    ;;

(*--------------------------------------------------------------------
  Into Base Value:
--------------------------------------------------------------------*)
    let rec into base value count =
        if (cmp' value base = 1) then base, 0
        else let check = add' value value 0
            in if (cmp' check base = 1) then value, count
        else into base check (count+1)
    ;;

(*--------------------------------------------------------------------
  Time MSBs():
--------------------------------------------------------------------*)
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
    ;;

(*--------------------------------------------------------------------
 Recursive value doubling:

--------------------------------------------------------------------*)
    let rec doubler value count = match (value, count) with
        | value, 0      -> value
        | value, count  -> (doubler (add' value value 0) (count-1))
    ;;

(*--------------------------------------------------------------------
  Recursive Division():

--------------------------------------------------------------------*)
    let rec divrem' dividend divisor sum =
        if (cmp' dividend [] = 0) then sum, [0]
        else let num, count = into dividend divisor 1
        in if count = 0 then sum, dividend
        else divrem' (trimzeros (sub' dividend num 0)) 
                   divisor (add' sum (doubler [1] (count - 1)) 0)
    ;;


(*--------------------------------------------------------------------
    Recursive Multiply():
    
--------------------------------------------------------------------*)
   let rec mul' value base sum = match (value, base, sum) with
        | [], _, sum         -> sum
        | [1], base, sum  -> add' base sum 0
        | value, base, sum -> 
            let num, count = into value [2] 1
            in mul' (trimzeros (sub' value num 0)) base 
            (add' sum (doubler base count) 0)

(*--------------------------------------------------------------------
  Recursive Exponentiation():

--------------------------------------------------------------------*)
let rec expt value count = match (value, count) with
        | value, 0      -> value
        | value, count  -> (expt (mul' value value []) (count-1))
    ;;

(*--------------------------------------------------------------------
  Recursive Power():

--------------------------------------------------------------------*)
    let rec pow' expo base prod = match (expo, base, prod) with
        | [], _, prod      -> Pos, prod
        | [1], base, prod  -> Neg, mul' base prod []
        | expo, base, prod -> 
            let num, count = into expo [2] 1
            in pow' (trimzeros (sub' expo num 0)) 
            base (mul' prod (expt base count) [])
    ;;

(*--------------------------------------------------------------------
  Compare:
    if the sign is the same then Compare
--------------------------------------------------------------------*)
    let cmp (Bigint (neg1, arg1)) (Bigint (neg2, arg2)) =
        if neg1 = neg2 
            then cmp' arg1 arg2
        else if neg1 = Pos
            then 1
            else -1
    ;;
    
(*--------------------------------------------------------------------
  Add():
      The process or skill of calculating the total of two or 
      more numbers or amounts.
--------------------------------------------------------------------*)
    let add (Bigint(neg1, arg1)) (Bigint(neg2, arg2)) =
        if neg1 = neg2 then Bigint(neg1, add' arg1 arg2 0)
        else if(cmp' arg1 arg2) = 1 
            then Bigint(neg1, (trimzeros(sub' arg1 arg2 0) ))
        else if(cmp' arg1 arg2) = -1
            then Bigint(neg2, (trimzeros(sub' arg2 arg1 0) ))
        else zero
    ;;

(*--------------------------------------------------------------------
  Subtraction():
      Anal -> analyze 
--------------------------------------------------------------------*)
 let sub (Bigint (neg1, arg1)) (Bigint (neg2, arg2)) =
        if neg1 = neg2 then 
            let anal = cmp' arg1 arg2 in
                 if anal > 0 
                    then Bigint (neg1, trimzeros (sub' arg1 arg2 0))
                 else if anal < 0 then 
                     let sign = if neg1 = Pos then Neg else Pos
                     in  Bigint (sign, trimzeros (sub' arg2 arg1 0))
                 else zero
        else Bigint (neg1, add' arg1 arg2 0) 


(*--------------------------------------------------------------------
  Multiply():
      Anal -> analyze 
      combine quantities under given rules to obtain their product.   
--------------------------------------------------------------------*)
    let mul (Bigint (neg1, arg1)) (Bigint (neg2, arg2)) =
        let anal = (cmp' arg1 arg2 = 1) in (if neg1 = neg2
            then (if anal
                then Bigint (Pos, mul' arg2 arg1 [])
                else Bigint (Pos, mul' arg1 arg2 []))
            else (if anal
                then Bigint (Neg, mul' arg2 arg1 [])
                else Bigint (Neg, mul' arg1 arg2 [])))
    ;;

(*--------------------------------------------------------------------
  Power():
      The number of times as indicated by an exponent that a number 
      occurs as a factor in a product
--------------------------------------------------------------------*)
    let pow (Bigint (neg1, arg1)) (Bigint (neg2, arg2)) =
        let sign, value = pow' arg2 arg1 [1]
        in if neg1 = Pos
            then Bigint (Pos, value)
            else Bigint (sign, value)
    ;;

(*--------------------------------------------------------------------
  Division/Remainder():
      Return the remainder of 2 terms by division

--------------------------------------------------------------------*)
    let divrem (Bigint (neg1, arg1)) (Bigint (neg2, arg2)) =
    let quot, xcess = divrem' arg1 arg2 []
        in match (neg1, neg2) with
            | Pos, Pos -> Bigint (Pos, quot), Bigint (Pos, xcess)
            | Neg, Pos -> Bigint (Neg, add' quot [1] 0),
                          Bigint (Pos, trimzeros (sub' arg2 xcess 0))
            | Pos, Neg -> Bigint (Neg, quot), Bigint (Pos, xcess)
            | Neg, Neg -> Bigint (Pos, add' quot [1] 0),
                          Bigint (Pos, trimzeros (sub' arg2 xcess 0))
    ;;

(*--------------------------------------------------------------------
  Remainder():
      Return the remainder by division 
--------------------------------------------------------------------*)
    let rem (Bigint (neg1, arg1)) (Bigint (neg2, arg2)) =
        let _, remainder = divrem (Bigint (neg1, arg1)) 
                                  (Bigint (neg2, arg2))
        in remainder
    ;;

(*--------------------------------------------------------------------
  Division():
      Return the Quotient of a Bigint by Division
--------------------------------------------------------------------*)
    let div (Bigint (neg1, arg1)) (Bigint (neg2, arg2)) =
        let quotient, _ = divrem (Bigint (neg1, arg1)) 
                                 (Bigint (neg2, arg2))
        in quotient
    ;;
(*------------------------------------------------------------------*)
end

