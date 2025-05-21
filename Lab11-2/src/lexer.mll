{
open Parser
}

let int = '-'?['0'-'9']+
let white = [' ' '\t']
let id =['a'-'z']+

rule read = parse
  | "(" {LPAREN}
  | ")" {RPAREN}
  | "+" {PLUS}
  | "*" {TIMES}
  | "=" {EQ}
  | "in" {IN}
  | "let" {LET}
  | white+ {read lexbuf}
  | id { ID (Lexing.lexeme lexbuf) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }
