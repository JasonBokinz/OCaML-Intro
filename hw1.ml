(* Jason Bokinz Homework 1 CSE-216*)

(* Question 1 *)

let rec compress lst = match lst with
  | [] -> []
  | [x] -> [x]
  | h::(consec::t) -> 
      if (h = consec) then 
        compress (consec::t)
      else 
        h ::(compress (consec::t))
;;


(* Question 2 *)

let rec remove_if lst predicate = match lst with
    | [] -> []
    | h::t ->
      if (predicate h) then
        remove_if t predicate
      else
        h :: (remove_if t predicate)
;;


(* Question 3 *)

let rec slice lst i j =
  match (lst, i, j) with
  | (_, 0, _) when i > j -> []
  | ([], _, _) -> []
  | (h::t, 0, _) when j > 0 -> h :: slice t 0 (j - 1)
  | (h::t, _, _) -> 
    slice t (i - 1) (j - 1)
;;


(* Question 4 *)

let rec equivs f lst =
  let rec partition f curr lst acc1 acc2 = match lst with
    | [] -> (acc1, acc2)
    | h::t ->
      if f curr h then
        partition f curr t (h :: acc1) acc2
      else
        partition f curr t acc1 (h :: acc2)
      in match lst with
        | [] -> []
        | h::t ->
          let (equiv_class, remaining) = partition f h t [h] [] 
        in equiv_class :: equivs f remaining
;;
  

(* Question 5 *)

let goldbachpair n =
  let if_prime p =
    if p <= 1 then 
      false
    else
      let rec if_divisible d =
        d * d > p || (p mod d <> 0 && if_divisible (d + 1))
      in if_divisible 2
    in let rec find_primes i =
    if if_prime i && if_prime (n - i) then
      (i, n - i)
    else
      find_primes (i + 1)
    in find_primes 2
;;
  

(* Question 6 *)

let rec identical_on f g lst = match lst with
  | [] -> true
  | h::t -> 
      (f h = g h) && identical_on f g t
;;


(* Question 7 *)

let rec pairwisefilter cmp lst = match lst with
  | [] -> []
  | [x] -> [x]
  | h::(consec::t) ->
    let element = cmp h consec in
    element :: (pairwisefilter cmp t)
  ;;


(* Question 8 *)

let polynomial lst =
  let rec plug_in_x lst x = match lst with
    | [] -> 0
    | (coefficient, exponent)::t ->
      coefficient * int_of_float (float_of_int x ** float_of_int exponent) + plug_in_x t x
    in fun x -> plug_in_x lst x
;;


(* Question 9 *)

let rec suffixes lst = match lst with
  | [] -> []
  | _::t ->
    lst :: suffixes t
;;


(* Question 10 *)

let rec powerset lst =
  let rec myList_map x lst_lsts =
    match lst_lsts with
    | [] -> []
    | h::t -> 
      (x::h) :: (myList_map x t)
  in
  match lst with
  | [] -> [[]]
  | x::t ->
    let rest = powerset t 
  in rest @ (myList_map x rest)
;;
