(* lib/ast.ml *)

(* 定义所有二元运算符 *)
type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Eq
  | Neq
  | Lt
  | Le
  | Gt
  | Ge
  | And
  | Or

(* 定义所有一元运算符 *)
type unop =
  | Neg
  | Not

(* 表达式 (Expr) 的定义 *)
type expr =
  | IntLiteral of int (* NUMBER *)
  | Id of string (* ID *)
  | BinOp of binop * expr * expr (* ... op ... *)
  | UnOp of unop * expr (* + - ! *)
  | Assign of string * expr (* ID = Expr *)
  | Call of string * expr list (* ID(Expr, ...) *)

(* 语句 (Stmt) 的定义 *)
type stmt =
  | Block of stmt list
  | Expr of expr
  | Return of expr option
  | If of expr * stmt * stmt option (* if (expr) stmt else stmt? *)
  | While of expr * stmt
  | VarDecl of string * expr (* int ID = Expr; *)
  | Break
  | Continue

(* 形参 (Param) 的定义: int ID *)
(* 我们只支持 int，所以只需要名字 *)
type param = string

(* 函数定义 (FuncDef) *)
type func_def =
  { fname : string (* ID *)
  ; params : param list (* (Param, ...)? *)
  ; body : stmt list (* Block *)
  }

(* 编译单元 (CompUnit)，也就是整个程序 *)
type program = func_def list (* FuncDef+ *)

let indent level = String.make (level * 2) ' '

(* 将二元运算符转换为字符串 *)
let string_of_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Eq -> "=="
  | Neq -> "!="
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="
  | And -> "&&"
  | Or -> "||"
;;

(* 将一元运算符转换为字符串 *)
let string_of_unop = function
  | Neg -> "-"
  | Not -> "!"
;;

(* 递归打印表达式 *)
let rec string_of_expr e =
  match e with
  | IntLiteral n -> Printf.sprintf "Int(%d)" n
  | Id s -> Printf.sprintf "Id(%s)" s
  | BinOp (op, e1, e2) ->
    Printf.sprintf
      "BinOp(%s, %s, %s)"
      (string_of_binop op)
      (string_of_expr e1)
      (string_of_expr e2)
  | UnOp (op, e) -> Printf.sprintf "UnOp(%s, %s)" (string_of_unop op) (string_of_expr e)
  | Assign (id, e) -> Printf.sprintf "Assign(%s, %s)" id (string_of_expr e)
  | Call (fname, args) ->
    let args_str = List.map string_of_expr args |> String.concat ", " in
    Printf.sprintf "Call(%s, [%s])" fname args_str

and string_of_stmt s =
  match s with
  | Block stmts ->
    let stmts_str = List.map string_of_stmt stmts |> String.concat "; " in
    Printf.sprintf "Block([%s])" stmts_str
  | Expr e -> Printf.sprintf "Expr(%s)" (string_of_expr e)
  | Return expr_opt ->
    let inner_str =
      match expr_opt with
      | Some e -> Printf.sprintf "Some(%s)" (string_of_expr e)
      | None -> "None"
    in
    Printf.sprintf "Return(%s)" inner_str
  | If (cond, then_stmt, else_stmt_opt) ->
    let else_str =
      match else_stmt_opt with
      | Some else_stmt -> Printf.sprintf ", Else(%s)" (string_of_stmt else_stmt)
      | None -> ""
    in
    Printf.sprintf
      "If(%s, %s%s)"
      (string_of_expr cond)
      (string_of_stmt then_stmt)
      else_str
  | While (cond, body) ->
    Printf.sprintf "While(%s, %s)" (string_of_expr cond) (string_of_stmt body)
  | VarDecl (id, e) -> Printf.sprintf "VarDecl(%s, %s)" id (string_of_expr e)
  | Break -> "Break"
  | Continue -> "Continue"
;;

let string_of_func_def f =
  let params_str = String.concat ", " f.params in
  (* 将 body 语句列表直接转换为一个 Block 字符串，更符合结构 *)
  let body_str = string_of_stmt (Block f.body) in
  Printf.sprintf "FuncDef(name=%s, params=[%s], body=%s)" f.fname params_str body_str
;;

let string_of_program prog =
  let funcs_str = List.map string_of_func_def prog |> String.concat ", " in
  Printf.sprintf "[%s]" funcs_str
;;
