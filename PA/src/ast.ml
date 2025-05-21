(** The type of the abstract syntax tree (AST). *)
type bop = 
  | Add
  | Mult
  | Leq                           (*integer comparison*)
  | And                           (*boolean conjunction*)
  | Cons                          (*list constructor*)

type expr =
  | Int of int 
  | Binop of bop * expr * expr
  | Let of string * expr * expr   (*the string is the id's name*)
  | Id of string
  | Fun of string * expr          (*the string is the argument's name*)
  | App of expr * expr            (*function application from left to right*)
  | If of expr * expr * expr      (*1st expr is the guard. 2nd expr is on "if" branch. 3rd expr is on "else" branch.*)
  | List
  | Bool of bool