module type Monad = sig
  type 'a t 
  val return : 'a -> 'a t
  val ( >>= ): 'a t -> ('a -> 'b t) -> 'b t
end;;

module ListMonad : Monad with
  type 'a t = 'a list = struct
  type 'a t = 'a list
  let return x = [x]
  let rec (>>=) lst f = 
    match lst with
    | [] -> []
    | h::t -> (f h) @ (t >>= f)
end;;

open ListMonad;;

let list_return_binary (op : int -> int -> int) (x : int) (y : int) : int list =
  ListMonad.return (op x y);;


let list_upgrade_binary (op : int -> int -> int) (x : int list) (y : int list) : int list =
  x >>= (fun a -> 
    y >>= (fun b -> list_return_binary op a b));;

let ( *| ) : int list -> int list -> int list = list_upgrade_binary ( * );;


let rec filter f lst =
  match lst with
  | [] -> []
  | x::xs -> if f x then x :: filter f xs
             else filter f xs
;;

let rec singularize lst =
  match lst with
  | [] -> []
  | x::xs ->
      x :: singularize (filter (fun y -> y <> x) xs)
;;

module SetMonad : Monad with
  type 'a t = 'a list = struct
  type 'a t = 'a list
  let return x = [ x]
  let rec (>>=) lst f = 
    match lst with
    | [] -> []
    | h::t -> singularize ((f h) @ (t >>= f))
end;;
