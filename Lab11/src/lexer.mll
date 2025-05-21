{
open Parser
}

let int = '-'?['0'-'9']+
let white = [' ' '\t']
let id =['a'-'z']+


rule read = parse
  | "(" {LPREN}
  | ")" {RPREN}
  | "+" {PLUS}
  | "*" {TIMES}
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }
