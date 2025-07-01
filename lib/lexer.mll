{
    open Parser
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']


rule token = parse
  | [' ' '\t' '\n'] { token lexbuf } (* Skip whitespace *)
  | "int"     { INT }
  | "void"    { VOID }
  | "if"      { IF }
  | "else"    { ELSE }
  | "while"   { WHILE }
  | "return"  { RETURN }
  | letter (letter | digit)* as id { ID id }
  | digit+ as n { NUMBER (int_of_string n) }
  | "+"       { PLUS }
  | "-"       { MINUS }
  | "*"       { TIMES }
  | "/"       { DIVIDE }
  | "="       { ASSIGN }
  | "=="      { EQ }
  | "("       { LPAREN }
  | ")"       { RPAREN }
  | "{"       { LBRACE }
  | "}"       { RBRACE }
  | ";"       { SEMICOLON }
  | eof       { EOF }