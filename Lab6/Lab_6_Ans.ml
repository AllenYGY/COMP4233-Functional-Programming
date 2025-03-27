(* Question 1*)
exception Different_dimension;;

let rec bi_map f v1 v2 =
  match v1, v2 with
  | [], [] -> []
  | h1::t1, h2::t2 -> f h1 h2 :: bi_map f t1 t2
  | _ -> raise Different_dimension;;

let vec_add a b = bi_map (+) a b;; 

let rec fold f ini = function
  | [] -> ini 
  | h :: t -> f h (fold f ini t);;

let rec zero = function
  | [] -> []
  | _::t -> 0::zero t;;

let rec ll_sum_1 llst acc  =
  match llst with 
  | [] -> acc
  | h::t -> ll_sum_1  t (vec_add h acc);;

let ll_sum llst =
  match llst with
  | [] -> []
  | h::t -> ll_sum_1 t h;;

(* Question 2*)
let rec bi_filter f lst1 lst2 = 
  match lst1, lst2 with
  | [], [] -> []
  | h1::t1, h2::t2 -> (if f h1 h2 then [(h1,h2)] else []) @ bi_filter f t1 t2
  | _ -> [];;

(* Question 3*)
let id x = x;;
let compose f g x = f (g x);;
  
let pipe funcs =
List.fold_right compose (List.rev funcs) id;;
  

  