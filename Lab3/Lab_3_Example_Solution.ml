(*Page 2*)
let upper a = 
  if a >= 'a' && a <= 'z' then 
    char_of_int (int_of_char a + (int_of_char 'A' - int_of_char 'a'))
  else
    char_of_int 0;;

let lower a = 
  if a >= 'A' && a <= 'Z' then 
    char_of_int (int_of_char a + (int_of_char 'a' - int_of_char 'A'))
  else
    char_of_int 0;;

(*Page 3*)
let rec fib n =
  if n = 0 then 1
  else if n = 1 then 1
  else fib (n-1) + fib (n-2);;

(*Page 5*)
let suc n =
  if n < 0 then 0
  else n + 1;;

let rec add a b =
  if a < 0 || b < 0 then 0
  else if b = 0 then a
  else suc (add a (b - 1));;

(*Page 6*)
let avg_1 a b = (a +. b)/.2.;;
let avg_2 (a,b) = (a +. b)/.2.;;

(*Page 7*)
let avg_3 = fun a b -> (a+.b)/.2.;;

(*Page 8*)
let rec (|^) a b = 
  if a < 0 || b < 0 then -1
  else if a = 0 then 0
  else if b = 0 then 1
  else a * (a |^ (b-1));;

let rec (|^|^) a b =
  if a < 0 || b < 0 then -1
  else if a = 0 then 0
  else if b = 0 then 1
  else a |^ (a |^|^ (b-1));;

(*Page 9*)
(*
  (<) is of type 'a -> 'a -> 'a, which can compair two values of the same type, even two lists, two variants(by constructor order).
*)

(*Page 10*)
assert true;;
(* Assert Fail
assert false;;
*)

(*Page 11*)
let rec (|^^) a b =
  if a = 0 then 1
  else a * ((a-1) |^^ b);;

(* Assert Fail
assert (8 = (2 |^^ 3));;
*)

