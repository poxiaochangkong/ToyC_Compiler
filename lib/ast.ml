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
let rec string_of_expr level e =
  let str_ind = indent level in
  match e with
  | IntLiteral n -> Printf.sprintf "%sInt(%d)" str_ind n
  | Id s -> Printf.sprintf "%sId(%s)" str_ind s
  | BinOp (op, e1, e2) ->
    Printf.sprintf
      "%sBinOp(%s,\n%s,\n%s\n%s)"
      str_ind
      (string_of_binop op)
      (string_of_expr (level + 1) e1)
      (string_of_expr (level + 1) e2)
      str_ind
  | UnOp (op, e) ->
    Printf.sprintf
      "%sUnOp(%s,\n%s\n%s)"
      str_ind
      (string_of_unop op)
      (string_of_expr (level + 1) e)
      str_ind
  | Assign (id, e) ->
    Printf.sprintf
      "%sAssign(%s,\n%s\n%s)"
      str_ind
      id
      (string_of_expr (level + 1) e)
      str_ind
  | Call (fname, args) ->
    let args_str =
      if List.length args = 0
      then ""
      else
        "\n"
        ^ (List.map (string_of_expr (level + 1)) args |> String.concat ",\n")
        ^ "\n"
        ^ str_ind
    in
    Printf.sprintf "%sCall(%s,%s)" str_ind fname args_str

(* 递归打印语句 (已根据你的要求修改) *)
and string_of_stmt level s =
  let str_ind = indent level in
  match s with
  | Block stmts ->
    let stmts_str = List.map (string_of_stmt (level + 1)) stmts |> String.concat "\n" in
    Printf.sprintf "%sBlock(\n%s\n%s)" str_ind stmts_str str_ind
  | Expr e ->
    Printf.sprintf "%sExpr(\n%s\n%s)" str_ind (string_of_expr (level + 1) e) str_ind
  | Return expr_opt ->
    let return_val_str =
      match expr_opt with
      | Some e -> Printf.sprintf "\n%s" (string_of_expr (level + 1) e)
      | None -> " (void)" (* 表示没有返回值 *)
    in
    Printf.sprintf "%sReturn(%s\n%s)" str_ind return_val_str str_ind
  | If (cond, then_stmt, else_stmt_opt) ->
    let else_str =
      match else_stmt_opt with
      | Some else_stmt ->
        Printf.sprintf
          ",\n%sElse(\n%s\n%s)"
          str_ind
          (string_of_stmt (level + 1) else_stmt)
          str_ind
      | None -> ""
    in
    Printf.sprintf
      "%sIf(\n%s,\n%s%s\n%s)"
      str_ind
      (string_of_expr (level + 1) cond)
      (string_of_stmt (level + 1) then_stmt)
      else_str
      str_ind
  | While (cond, body) ->
    Printf.sprintf
      "%sWhile(\n%s,\n%s\n%s)"
      str_ind
      (string_of_expr (level + 1) cond)
      (string_of_stmt (level + 1) body)
      str_ind
  | VarDecl (id, e) ->
    Printf.sprintf
      "%sVarDecl(%s,\n%s\n%s)"
      str_ind
      id
      (string_of_expr (level + 1) e)
      str_ind
  | Break -> str_ind ^ "Break"
  | Continue -> str_ind ^ "Continue"
;;

(* 打印函数定义 *)
let string_of_func_def level f =
  let str_ind = indent level in
  let params_str = String.concat ", " f.params in
  let body_block = Block f.body in
  let body_str = string_of_stmt (level + 1) body_block in
  Printf.sprintf
    "%sFuncDef(name=%s, params=[%s],\n  body=\n%s\n%s)"
    str_ind
    f.fname
    params_str
    body_str
    str_ind
;;

(* 打印整个程序 *)
let string_of_program prog = List.map (string_of_func_def 0) prog |> String.concat "\n\n"
