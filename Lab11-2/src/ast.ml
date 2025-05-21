(** The type of the abstract syntax tree (AST). *)
type bop =
| Add
| Mult
type expr =
| Int of int
| Id of string
| Binop of bop * expr * expr
| Let of string * expr * expr