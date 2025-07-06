(* lib/lexer.mll *)

{
  open Parser
  exception Error of string
}

(* 定义一些可复用的正则表达式别名，让代码更清晰 *)
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let identifier_start = letter | '_'
let identifier_body = letter | digit | '_'

rule token = parse
  (* 1. 忽略空白字符 *)
  | [' ' '\t' '\n'] { token lexbuf }

  (* 2. 处理注释 *)
  | "/*" { comment lexbuf }
  | "//" { single_line_comment lexbuf }

  (* 3. 关键字 (必须放在标识符规则之前，以确保优先匹配) *)
  | "if"      { IF }
  | "else"    { ELSE }
  | "while"   { WHILE }
  | "break"   { BREAK }
  | "continue" { CONTINUE }
  | "return"  { RETURN }
  | "int"     { INT }
  | "void"    { VOID }

  (* 4. 整数 (NUMBER) - 根据官方正则表达式重写 *)
  | '0' | ['1'-'9'] digit* as lxm { NUMBER (int_of_string lxm) }  (*这里计时只有一个'0' as 关键字也会自动转换成一个string类型, 确保 int_of_string 不会报错*)
  (* 注意：我们不在这里处理负号*)

  (* 5. 标识符 (ID) - 使用我们定义的别名 *)
  | identifier_start identifier_body* as lxm { ID lxm }

  (* 6. 运算符和标点符号 *)
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

  (* 文件结束符 *)
  | eof       { EOF }

  (* 捕获所有未知字符 *)
  | _ as char { raise (Error (Printf.sprintf "Unknown character: '%c'" char)) }

(* 注释处理规则 (保持不变) *)
and comment = parse
  | "*/" { token lexbuf }
  | _    { comment lexbuf }

and single_line_comment = parse
  | '\n' { token lexbuf }
  | eof  { EOF }
  | _    { single_line_comment lexbuf }