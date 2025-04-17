module type Stack = sig
  type 'a element
  type 'a stack
  exception Empty
  val empty : 'a stack
  val push: 'a stack -> 'a element -> 'a stack
  val pop: 'a stack -> 'a stack
  val peek: 'a stack -> 'a element
end;;

type 'a pairlst = NULL | P of 'a * 'a pairlst;;

module PairStack:Stack with 
  type 'a element = 'a and
  type 'a stack = 'a pairlst = struct
  type 'a element = 'a
  type 'a stack = 'a pairlst
  exception Empty
  let empty = NULL
  let push s e = P(e, s)
  let pop = function 
    | NULL -> raise Empty
    | P(_, s) -> s
  let peek = function
    | NULL -> raise Empty
    | P(e, _) -> e
end;;

module type ToString = sig 
  type t 
  val to_string : t -> string 
end;;

module IntToString : ToString with type t = int = struct
  type t = int
  let to_string x = string_of_int x
end;;

module Print (M: ToString) = struct 
  let print x = print_string (M.to_string x) 
end;;

module PrintInt = Print(IntToString);;
