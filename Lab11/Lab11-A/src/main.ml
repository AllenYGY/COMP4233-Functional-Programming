open Ast

(** [parse s] parses [s] into an AST. *)
let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in ast

let is_val = function
  | Int _ -> true
  | _ -> false

let step_binop = function
  | (Add, Int a, Int b) -> Int (a + b)
  | (Mult, Int a, Int b) -> Int (a * b)
  | _ -> failwith "Unexpected in step_binop"

let rec step: expr -> expr = function
  | Int _ -> failwith "Does not step"
  | Binop (bop, e1,e2) ->
    if not (is_val e1) then Binop (bop,step e1,e2)
    else if not (is_val e2) then Binop (bop,e1,step e2)
    else step_binop(bop,e1,e2)

let rec eval (e: expr) : expr =
  if is_val e then e 
  else e |> step |> eval

let string_of_val : expr -> string = function
  |Int i -> string_of_int i 
  |Binop _ -> failwith "Not a value"

let interp (s :string) : string =
  s |> parse |> eval |> string_of_val

let () =
  let s = read_line () in
  print_endline (interp s)
  
  