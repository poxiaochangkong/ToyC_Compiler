%{
      (*open Ast *)
%}

%token <int> NUMBER
%token <string> ID
%token INT VOID IF ELSE WHILE RETURN
%token PLUS MINUS TIMES DIVIDE
%token ASSIGN EQ
%token LPAREN RPAREN LBRACE RBRACE SEMICOLON
%token EOF

%start <Ast.program> program

%% 
program :
    | id = ID EOF {id}