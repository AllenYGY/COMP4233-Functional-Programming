{
open Parser
}

let int = '-'?['0'-'9']+
let white = [' ' '\t']
let id = ['a'-'z']+
let bool = "true" | "false"

rule read = 
  parse
  | white {read lexbuf} 
  | '+' {PLUS}
  | '*' {TIMES}
  | '(' {LPREN}
  | ')' {RPREN}
  | "let" {LET}
  | "in" {IN} 
  | '=' {EQ}
  | "fun" {FUN}
  | "->" {TO}
  | "if" {IF}
  | "then" {THEN}
  | "else" {ELSE}
  | "<=" {LEQ}
  | "^" {AND}
  | "[]" {LIST}
  | "::" {CONS}
  | bool {BOOL (bool_of_string (Lexing.lexeme lexbuf))}
  | int {INT (int_of_string (Lexing.lexeme lexbuf))}
  | id {ID (Lexing.lexeme lexbuf)}
  | eof { EOF }