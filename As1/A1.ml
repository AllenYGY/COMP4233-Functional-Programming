(*this reverse function is a tail recursive function, 
since the recursive call is the last operation in the function.*)

let rec reverse_tail lst acc  =
  match lst with
  | [] -> acc
  | x :: lst -> reverse_tail lst (x::acc) ;;

let reverse lst = reverse_tail lst [];;



type 'a under = E | S of 'a | P of 'a under * 'a under;;

let rec unify = function
  | E -> [] 
  | S x -> [x]
  | P (x, y) -> (unify x) @ (unify y);;