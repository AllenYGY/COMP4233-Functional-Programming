open Ast

(** [parse s] parses [s] into an AST. *)
let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in ast

let rec is_val = function
  | Int _ -> true
  | Bool _ -> true
  | List -> true
  | Fun _ -> true
  | Binop(Cons, h, t) -> is_val h && is_val t  
  | _ -> false

let step_binop = function 
  | (Add, Int a, Int b) -> Int (a + b)
  | (Mult, Int a, Int b) -> Int (a * b)
  | (Leq, Int a, Int b) -> Bool (a <= b)
  | (And, Bool a, Bool b) -> Bool (a && b)
  | (Cons, v, List) ->  Binop(Cons, v, List)
  | _ -> failwith "Invalid binary operation"

let rec substitute n v e =
  match e with 
  | Binop (bop, e1, e2) -> Binop (bop, substitute n v e1, substitute n v e2)
  | Int i -> Int i 
  | Bool b -> Bool b
  | List -> List
  | Id id -> 
    if n = id then v 
    else Id id 
  | Let (id, e1, e2) -> 
    if id = n then 
      Let (id, substitute n v e1, e2)
    else 
      Let (id, substitute n v e1, substitute n v e2)
  | Fun (id, e) -> 
      if id = n then Fun (id, e)  
      else Fun (id, substitute n v e)  
  | App (e1, e2) -> App (substitute n v e1, substitute n v e2)
  | If (e1, e2, e3) -> 
    If (substitute n v e1, substitute n v e2, substitute n v e3)

let rec step = function
  | Int _ | Bool _ | List | Fun _ -> failwith "Does not step"
  | Id _ -> failwith "Unbound variable"
  | Binop (bop, e1, e2) -> 
    if not (is_val e1) then Binop (bop, step e1, e2)
    else if not (is_val e2) then Binop (bop, e1, step e2)
    else step_binop (bop, e1, e2)
  | Let (name, e1, e2) -> 
    if not (is_val e1) then Let (name, step e1, e2)
    else substitute name e1 e2
  | App (e1, e2) ->
    if not (is_val e1) then App (step e1, e2)
    else if not (is_val e2) then App (e1, step e2)
    else
      (match e1 with
       | Fun (x, body) -> substitute x e2 body
       | _ -> failwith "Application of non-function")
  | If (e1, e2, e3) ->
    if not (is_val e1) then If (step e1, e2, e3)
    else
      (match e1 with
       | Bool true -> e2
       | Bool false -> e3
       | _ -> failwith "Guard of if must be boolean")

let string_of_bop = function
  | Add -> " + "
  | Mult -> " * "
  | Leq -> " <= "
  | And -> " ^ "
  | Cons -> " :: "

let rec string_of_val = function
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | List -> "[]"
  | Fun (name, _) -> "fun " ^ name ^ " -> ..."
  | Binop(Cons, h, t) -> 
      let rec list_elements = function
        | List -> ""
        | Binop(Cons, h, List) -> string_of_val h
        | Binop(Cons, h, t) -> string_of_val h ^ "; " ^ list_elements t
        | _ -> failwith "Not a valid list"
      in
      "[" ^ list_elements (Binop(Cons, h, t)) ^ "]"
  | _ -> failwith "Not a value"

let rec string_of_expr = function
  | Int i -> string_of_int i
  | Id name -> name
  | List -> "[]"
  | Bool b -> string_of_bool b
  | Fun (name, e) -> "fun " ^ name ^ " -> " ^ string_of_expr e
  | App (e1, e2) -> "(" ^ string_of_expr e1 ^ " " ^ string_of_expr e2 ^ ")"
  | If (e1, e2, e3) -> "if " ^ string_of_expr e1 ^ " then " ^ string_of_expr e2 ^ " else " ^ string_of_expr e3
  | Binop (bop, e1, e2) -> "(" ^ string_of_expr e1 ^ string_of_bop bop ^ string_of_expr e2 ^ ")"
  | Let (name, e1, e2) -> "let " ^ name ^ " = " ^ string_of_expr e1 ^ " in " ^ string_of_expr e2

let rec eval e = 
  if is_val e then e 
  else e |> step |> eval
  
let interp s = 
  s |> parse |> eval |> string_of_val

let () =
  let s = read_line () in
  print_endline (interp s)
