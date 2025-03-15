let rec reverse lst =
  match lst with
  | [] -> []
  | [x] -> [x]
  | x :: lst -> (reverse lst) @ [x];;

type 'a under = E | S of 'a | P of 'a under * 'a under;;

let rec unify = function
  | E -> [] 
  | S x -> [x]
  | P (x, y) -> (unify x) @ (unify y);;