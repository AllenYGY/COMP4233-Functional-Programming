type unary = S;;
type peano = Nil | Cons of unary * peano;;

let zero = Nil;;
let one  = Cons(S, Nil);;
let two = Cons(S, one);;

let successor x  = 
  Cons(S, x);;

let three = successor two;;

let predecessor = function 
  | Nil -> Nil
  | Cons (_, a) -> a;;

let rec add  x = function
  | Nil -> x
  | Cons(_, b') -> successor (add x b');;

exception NotNat;;
let rec peano_of_nat (n:int)=
  if n < 0 then raise NotNat
  else if n = 0 then Nil
  else Cons(S, peano_of_nat (n-1));;

type tag = Neg|Zero|Pos;;
type pint = tag * int;;

let split (n:int) =
  if n < 0 then (Neg, n)
  else if n = 0 then (Zero, 0)
  else (Pos, n);;

type int_or_float =
  | Int of int      
  | Float of float  

exception TypeError;;

let ( +.+ ) x y = 
  match x, y with
  | Int a, Int b -> Int (a + b)
  | Float a, Float b -> Float (a +. b)
  | _ -> raise TypeError;;