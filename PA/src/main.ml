open Ast

(** [parse s] parses [s] into an AST. *)
let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let is_val = function 
  | Int _ | Id _ -> true 
  | _ -> false

let step_binop = function 
  | (Add, Int a, Int b) -> Int (a + b)
  | (Mult, Int a, Int b) -> Int (a * b)
  | _ -> failwith "Unexpected in step binop"

let rec substitute n v e =
  match e with 
  | Binop (bop, e1, e2) -> Binop (bop, substitute n v e1, substitute n v e2)
  | Int i -> Int i 
  | Id id -> 
    if n = id then v 
    else Id id 
  | Let (id, e1, e2) -> 
    if not (id = n) then 
      Let (id, substitute n v e1, substitute n v e2)
    else Let (id, e1, e2) 
(*| _ -> failwith "unexpected in substitution"*)

let rec step = function
  | Int _ | Id _ -> failwith "Does not step"
  | Binop (bop, e1, e2) -> 
    if not (is_val e1) then Binop (bop, step e1, e2)
    else if not (is_val e2) then Binop (bop, e1, step e2)
    else step_binop (bop, e1, e2)
  | Let (name, e1, e2) -> 
    if not (is_val e1) then Let (name, step e1, e2)
    else substitute name e1 e2
(*| _ -> failwith "unexpected in step"*)

let rec string_of_val = function
  | Int i -> string_of_int i
  | Id name -> name
  | List -> "[]"
  | Bool b -> string_of_bool b
  | Fun (name, e) -> "fun " ^ name ^ " -> " ^ string_of_val e
  | App (e1, e2) ->(string_of_val e1) ^ " " ^ (string_of_val e2)
  | If (e1, e2, e3) -> "If " ^ (string_of_val e1) ^ " then " ^ (string_of_val e2) ^ " else " ^ (string_of_val e3)
  | Binop (bop, e1, e2) -> (string_of_val e1) ^ (string_of_bop bop) ^ (string_of_val e2)
  | _ -> failwith "Not a value"

let string_of_bop = function
  | Add -> " + "
  | Mult -> " * "
  | Leq -> " <= "
  | And -> " ^ "
  | Cons -> " :: "

let rec eval e = 
  if is_val e then e 
  else e |> step |> eval
  
let interp s = 
  s |> parse |> eval |> string_of_val
