(** The type of the abstract syntax tree (AST). *)
type bop =
| Add
| Mult
type expr =
| Int of int
| Binop of bop * expr * expr