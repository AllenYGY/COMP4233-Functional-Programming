(*Example 1.*)
module Cal = struct 
  let one = 1
  let two = 2
  let plus = ( + )
  let mult = ( * )
end;;

(*Example 3.*)
module type Monoid =
  sig
    type element
    val zero : element
    val ( + ) : element -> element -> element
    val print : element -> unit
end;; 

type peano = O | Cons of unit * peano;;
module PeanoMult : Monoid with 
  type element = peano
= struct 
  type element = peano
  let zero = Cons ((), O) 
  let suc a = Cons ((), a) 
  let rec add a = function 
    | O -> a 
    | Cons ((), b) -> suc (add a b)
  let rec (+) a = function 
    | O -> O 
    | Cons ((), Cons ((), b)) -> add a (a + (Cons ((), b)))
    | zero -> a 
  let rec print = function 
    | O -> print_string "O"
    | Cons ((), b) -> print_string "S "; print b
end;;

(*Example 4.*)
type ternary = Larger | Equal | Smaller;;
module type Comparable = sig 
  type t 
  val compare : t -> t -> ternary 
  val max : t -> t -> t 
  val min : t -> t -> t
end;;

module Int_Comparable : Comparable with 
	type t = int 
= struct 
	type t = int 
	let compare a b =
		if a > b then Larger 
		else if a = b then Equal 
		else Smaller
	let max a b =	if a > b then a else b 
	let min a b = if a < b then a else b
end;;

module Make_Itvl(Bound : Comparable) = struct
  type i = Empty | Itvl of Bound.t * Bound.t
  let create a b =
    match Bound.compare a b with 
    | Equal -> Empty 
    | Larger -> Itvl (b, a)
    | Smaller -> Itvl (a, b)
  let intersect t1 t2 =
    match t1,t2 with
    | Itvl (l1, u1), Itvl (l2, u2) ->
      if not (Bound.compare u1 l2 = Larger) || 
        not (Bound.compare u2 l1 = Larger) then 
          Empty 
      else create (max l1 l2) (min u1 u2)
    | _, _ -> Empty
end;;

(*This is how functor is used on the slide*)
module Int_Itvl = Make_Itvl (struct 
  type t = int 
  let compare = Int_Comparable.compare 
  let min = Int_Comparable.min 
  let max = Int_Comparable.max
end);;

(*Functor can also be used on predefined modules*)
module Int_Itvl = Make_Itvl (Int_Comparable);;

(*Example 5.*)
type week = Sun | Mon | Tue | Wed | Thu | Fri | Sat;;
module Week_Comparable : Comparable with 
  type t = week 
= struct 
  type t = week 
  let int_of_w = function 
  | Sun -> 0
  | Mon -> 1
  | Tue -> 2
  | Wed -> 3
  | Thu -> 4
  | Fri -> 5
  | Sat -> 6
  let compare a b = 
    let x = int_of_w a in 
    let y = int_of_w b in 
    if x = y then Equal 
    else if x < y then Smaller 
    else Larger
  let max a b = if (compare a b) = Larger then a else b 
  let min a b = if (compare a b) = Smaller then a else b 
end;; 

module Week_Itvl = Make_Itvl (Week_Comparable);;

(*try Week_Itvl*)
open Week_Itvl;;
let i1 = create Sun Fri;;
let i2 = create Mon Sat;;
let i3 = intersect i2 i1;;