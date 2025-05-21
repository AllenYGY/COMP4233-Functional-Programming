%{
open Ast
%}

%token EOF
%token <int> INT 
%token PLUS
%token TIMES
%token LPREN
%token RPREN

%token LET 
%token EQ 
%token <string> ID 
%token IN 

%left PLUS 	
%left TIMES 

%start <expr> prog

%%

expr:
	| e1 = expr; PLUS; e2 = expr {Binop (Add, e1, e2)}
	| e1 = expr; TIMES; e2 = expr {Binop (Mult, e1, e2)}
	| LPREN; e = expr; RPREN {e} 
	| i = INT {Int i}
	| LET; name = ID; EQ; e1 = expr; IN; e2 = expr {Let (name, e1, e2)}
	| name = ID {Id name}
	;

prog:
	| e = expr; EOF { e }
	;

	
