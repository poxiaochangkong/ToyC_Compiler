
%{
  open Ast
%}



%token IF ELSE WHILE BREAK CONTINUE RETURN INT VOID


%token <int> NUMBER
%token <string> ID


%token PLUS MINUS TIMES DIVIDE MOD
%token ASSIGN EQ NEQ LT LE GT GE
%token AND OR NOT
%token LPAREN RPAREN LBRACE RBRACE SEMICOLON COMMA


%token EOF

%right ASSIGN                     /* a = b = c is parsed as a = (b = c) */
%left OR                          /* a || b || c is parsed as (a || b) || c */
%left AND
%nonassoc EQ NEQ                  /* a == b == c is a syntax error */
%nonassoc LT LE GT GE             /* a < b < c is a syntax error */
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right NOT NEG                    /* A dummy token for unary minus/not precedence */

%start <Ast.program> program

%%


program:
  | funcs=nonempty_list(func_def) EOF { funcs }

/* FuncDef -> ("int" | "void") ID "(" (Param ("," Param)*)? ")" Block */
func_def:
  | INT fname=ID LPAREN params=separated_list(COMMA, param) RPAREN body=block {
      { fname=fname; params=params; body=body }
    }
  /* 这里我们现在只是返回int类型的值 */

/* Param -> "int" ID */
param:
  | INT name=ID { name }

/* Block -> "{" Stmt* "}" */
block:
  | LBRACE stmts=list(stmt) RBRACE { stmts }

/* Stmt -> ... */
stmt:
  | e=expr SEMICOLON { Expr(e) }
  | INT name=ID ASSIGN e=expr SEMICOLON { VarDecl(name, e) }
  | IF LPAREN cond=expr RPAREN then_stmt=stmt { If(cond, then_stmt, None) }
  | IF LPAREN cond=expr RPAREN then_stmt=stmt ELSE else_stmt=stmt { If(cond, then_stmt, Some(else_stmt)) }
  | WHILE LPAREN cond=expr RPAREN loop_body=stmt { While(cond, loop_body) }
  | BREAK SEMICOLON { Break }
  | CONTINUE SEMICOLON { Continue }
  | RETURN e=expr SEMICOLON { Return(Some(e)) }
  | inner_block=block { Block(inner_block) } /* A block can also be a statement */


expr:
  | ID ASSIGN e=expr { Assign($1, e) }
  | LOrExpr { $1 }

/* LOrExpr -> LAndExpr | LOrExpr "||" LAndExpr */
LOrExpr:
  | LAndExpr { $1 }
  | LOrExpr OR LAndExpr { BinOp(Or, $1, $3) }

/* LAndExpr -> RelExpr | LAndExpr "&&" RelExpr */
LAndExpr:
  | RelExpr { $1 }
  | LAndExpr AND RelExpr { BinOp(And, $1, $3) }

/* RelExpr -> AddExpr | RelExpr ("<" | ">" | "<=" | ">=" | "==" | "!=") AddExpr */
RelExpr:
  | AddExpr { $1 }
  | RelExpr LT AddExpr  { BinOp(Lt,  $1, $3) }
  | RelExpr LE AddExpr  { BinOp(Le,  $1, $3) }
  | RelExpr GT AddExpr  { BinOp(Gt,  $1, $3) }
  | RelExpr GE AddExpr  { BinOp(Ge,  $1, $3) }
  | RelExpr EQ AddExpr  { BinOp(Eq,  $1, $3) }
  | RelExpr NEQ AddExpr { BinOp(Neq, $1, $3) }

/* AddExpr -> MulExpr | AddExpr ("+" | "-") MulExpr */
AddExpr:
  | MulExpr { $1 }
  | AddExpr PLUS MulExpr  { BinOp(Add, $1, $3) }
  | AddExpr MINUS MulExpr { BinOp(Sub, $1, $3) }

/* MulExpr -> UnaryExpr | MulExpr ("*" | "/" | "%") UnaryExpr */
MulExpr:
  | UnaryExpr { $1 }
  | MulExpr TIMES UnaryExpr  { BinOp(Mul, $1, $3) }
  | MulExpr DIVIDE UnaryExpr { BinOp(Div, $1, $3) }
  | MulExpr MOD UnaryExpr    { BinOp(Mod, $1, $3) }

/* UnaryExpr -> PrimaryExpr | ("+" | "-" | "!") UnaryExpr */
UnaryExpr:
  | PrimaryExpr { $1 }
  | PLUS UnaryExpr  { $2 } /* Unary plus is a no-op */
  | MINUS UnaryExpr %prec NEG { UnOp(Neg, $2) } /* Use %prec to give it correct precedence */
  | NOT UnaryExpr   %prec NEG { UnOp(Not, $2) }

/* PrimaryExpr -> ID | NUMBER | "(" Expr ")" | ID "(" (Expr ("," Expr)*)? ")" */
PrimaryExpr:
  | n=NUMBER { IntLiteral(n) }
  | name=ID { Id(name) }
  | LPAREN e=expr RPAREN { e }
  | name=ID LPAREN args=separated_list(COMMA, expr) RPAREN { Call(name, args) }