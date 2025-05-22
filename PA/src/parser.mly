%{
open Ast
%}

%token EOF
%token <int> INT 
%token <bool> BOOL
%token PLUS
%token TIMES
%token LPREN  
%token RPREN  

%token LET 
%token IN 
%token EQ 
%token FUN 
%token TO 
%token IF
%token THEN
%token ELSE

%token LEQ 
%token AND
%token LIST 
%token CONS
%token APP

%token <string> ID 

%left PLUS 	
%left TIMES 
%left AND
%left LEQ
%right CONS
%nonassoc APP 

%start <expr> prog

%%

expr:
	| e1 = expr; PLUS; e2 = expr {Binop (Add, e1, e2)}
	| e1 = expr; TIMES; e2 = expr {Binop (Mult, e1, e2)}
	| e1 = expr; AND; e2 = expr {Binop (And, e1, e2)}
	| e1 = expr; CONS; e2 = expr {Binop (Cons, e1, e2)}
	| e1 = expr; LEQ; e2 = expr {Binop (Leq, e1, e2)}
	| LPREN; e = expr; RPREN {e} 
	| e1 = expr; e2 = expr %prec APP {App (e1, e2)} 
	| name = ID {Id name}
	| i = INT {Int i}
	| b = BOOL {Bool b}
	| LIST {List}  
	| LET; name = ID; EQ; e1 = expr; IN; e2 = expr {Let (name, e1, e2)}
	| FUN; name = ID; TO; e = expr {Fun (name, e)}
	| IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr {If (e1, e2, e3)}
	;

prog:
	| e = expr; EOF { e }
	;
