
%{open Ast%}

%token EOF
%token <int> INT
%token PLUS
%token TIMES
%token LPREN
%token RPREN
%left PLUS
%left TIMES

%start <Ast.expr> prog
%type <Ast.expr> expr
%%

expr:
	| e1 = expr; PLUS; e2 = expr{
		Binop(Add,e1,e2)}
	| e1 = expr; TIMES; e2 = expr{
		Binop(Mult,e1,e2)}
	| LPREN; e = expr; RPREN {e}
	| i = INT {Int i};

prog:
	|e=expr; EOF { e };

	
