%{open Ast %}

%token EOF
%token PLUS
%token TIMES
%token LPAREN
%token RPAREN
%token LET 
%token IN
%token EQ
%token <int> INT
%token <string> ID

%left PLUS
%left TIMES

%start <Ast.expr> prog
%type <Ast.expr> expr
%%

expr:
    | e1 = expr; PLUS; e2 = expr {
        Binop(Add, e1, e2)}
    | e1 = expr; TIMES; e2 = expr {
        Binop(Mult, e1, e2)}
    | LPAREN; e = expr; RPAREN {e}
    | LET; e0 = ID; EQ; e1 = expr; IN; e2 = expr { 
        Let(e0, e1, e2)}
    | i = INT {Int i}
    | i = ID {Id i};

prog:
    |e=expr; EOF { e };
