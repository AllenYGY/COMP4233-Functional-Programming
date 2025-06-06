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

let rec subst id v e =
  match e with
  | Id x -> if x = id then v else e
  | Int _ -> e
  | Binop (op, e1, e2) -> Binop (op, subst id v e1, subst id v e2)
  | Let (x, e1, e2) -> 
      if x = id then Let (x, subst id v e1, e2)  (* 变量被遮蔽 *)
      else Let (x, subst id v e1, subst id v e2)

let rec step: expr -> expr = function
  | Int _ -> failwith "Does not step"
  | Id _ -> failwith "Does not step"
  | Binop (bop, e1, e2) ->
    if not (is_val e1) then Binop (bop, step e1, e2)
    else if not (is_val e2) then Binop (bop, e1, step e2)
    else step_binop(bop, e1, e2)
  | Let (x, e1, e2) ->
    if not (is_val e1) then Let (x, step e1, e2)
    else subst x e1 e2

let rec eval (e: expr) : expr =
  if is_val e then e 
  else e |> step |> eval

let string_of_val : expr -> string = function
  | Int i -> string_of_int i
  | Id _ -> failwith "Not a value"  
  | Binop _ -> failwith "Not a value"
  | Let _ -> failwith "Not a value"

let interp (s :string) : string =
  s |> parse |> eval |> string_of_val

let () =
  let s = read_line () in
  print_endline (interp s)
