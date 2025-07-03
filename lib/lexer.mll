(* lib/lexer.mll *)

{
  open Parser (* To get access to the token definitions *)
  exception Error of string
}

rule token = parse
  | [' ' '\t' '\n'] { token lexbuf } (* Skip whitespace *)
  | "/*"            { comment lexbuf } (* Handle block comments *)

  (* Keywords *)
  | "if"      { IF }
  | "else"    { ELSE }
  | "while"   { WHILE }
  | "break"   { BREAK }
  | "continue" { CONTINUE }
  | "return"  { RETURN }
  | "int"     { INT }
  | "void"    { VOID }

  (* Literals and Identifiers *)
  | ['0'-'9']+ as lxm { NUMBER (int_of_string lxm) }
  | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID lxm }

  (* Operators and Punctuation *)
  | "("       { LPAREN }
  | ")"       { RPAREN }
  | "{"       { LBRACE }
  | "}"       { RBRACE }
  | ";"       { SEMICOLON }
  | ","       { COMMA }
  | "="       { ASSIGN }
  | "=="      { EQ }
  | "!="      { NEQ }
  | "<"       { LT }
  | "<="      { LE }
  | ">"       { GT }
  | ">="      { GE }
  | "&&"      { AND }
  | "||"      { OR }
  | "!"       { NOT }
  | "+"       { PLUS }
  | "-"       { MINUS }
  | "*"       { TIMES }
  | "/"       { DIVIDE }
  | "%"       { MOD }

  | eof       { EOF }

and comment = parse
  | "*/" { token lexbuf }
  | _    { comment lexbuf }