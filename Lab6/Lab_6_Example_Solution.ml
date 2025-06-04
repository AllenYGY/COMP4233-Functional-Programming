(*Page 3 - map, filter, fold, and their applications*)
let rec map f = function
  | [] -> []
  | h :: t -> f h :: map f t;;

let rec filter r = function 
  | [] -> []
  | h :: t -> ( if r h then [h] else [] ) @ filter r t;;

let rec fold f ini = function
  | [] -> ini 
  | h :: t -> f h (fold f ini t);;

let scr_lst = [92; 81; 75; 64; 55; 49; 37; 23; 11];;

type letter_grade = A | B | C | D | F;; 
exception S_error of string;;
let score_to_letter x =
  if x < 0 then raise (S_error "Score is too low!")
  else if x < 20 then F 
  else if x < 30 then D 
  else if x < 50 then C 
  else if x < 70 then B 
  else if x <= 100 then A 
  else raise (S_error "Score is too high!");;

let scrlst_to_letlst lst = map score_to_letter lst;;

let fail s = 
  match score_to_letter s with 
  | F -> true 
  | _ -> false;; 

let fail_lst lst = filter fail lst;;

let min a b = if a < b then a else b;;
let max a b = if a < b then b else a;;

let min_scr lst = fold min 100 lst;;
let max_scr lst = fold max 0 lst;;


(*Page 4 - Currify and Uncurrify*)

let currify f a b = f (a, b);;

let uncurrify f (a, b) = f a b;;


(*Page 5 - id, apply, compose, and repose*)
let id x = x;;

let apply a b = a b;;

let compose f g x = f (g x);;

let repose f g x = g (f x);;


(*Page 6 - Vector calculation*)
type int_vector = int list;;

exception Different_dimension;;

let rec vec_add l1 l2 = 
  match l1, l2 with 
  | [], [] -> [] 
  | h1 :: t1, h2 :: t2 -> (h1 + h2) :: (vec_add t1 t2)
  | _ -> raise Different_dimension;;

  let rec convolution l1 l2 = 
    match l1, l2 with 
    | [], [] -> [] 
    | h1 :: t1, h2 :: t2 -> (h1 * h2) :: (convolution t1 t2)
    | _ -> raise Different_dimension;;

(*Page 7 - bi_map*)
let rec bi_map f l1 l2 =
  match l1, l2 with
  | [], [] -> [] 
  | h1 :: t1, h2 :: t2 -> f h1 h2 :: (bi_map f t1 t2)
  | _ -> raise Different_dimension;;

let vec_add' = bi_map ( + );; 
let convolution' = bi_map ( * );;

