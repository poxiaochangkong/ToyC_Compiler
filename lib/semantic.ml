(* open Ast
open Symbol

exception SemanticError of string

(* 构建函数符号表 *)
let build_func_env (funcs: func list) : func_env =
  List.fold_left (fun env f ->
    let arg_types = List.map fst f.args in
    StringMap.add f.name (arg_types, f.rettyp) env
  ) StringMap.empty funcs

(* 扩展变量环境 *)
let add_var (venv: var_env) (name: string) (t: typ) : var_env =
  StringMap.add name t venv

(* 表达式类型推导 *)
let rec type_of_expr (env: env) (e: expr) : typ =
  match e with
  | Int _ -> TyInt
  | Bool _ -> TyBool
  | Var x -> StringMap.find x env.vars
  | Binop (op, e1, e2) ->
      let t1 = type_of_expr env e1 in
      let t2 = type_of_expr env e2 in
      begin match op with
      | Add | Sub | Mul | Div -> if t1 = TyInt && t2 = TyInt then TyInt else raise (SemanticError "Int op on non-int")
      | Lt | Le | Gt | Ge | Eq | Ne -> if t1 = t2 then TyBool else raise (SemanticError "Comparison type mismatch")
      | And | Or -> if t1 = TyBool && t2 = TyBool then TyBool else raise (SemanticError "Logical op on non-bool")
      end
  | Unop (op, e) ->
      let t = type_of_expr env e in
      begin match op with
      | Neg -> if t = TyInt then TyInt else raise (SemanticError "Neg on non-int")
      | Not -> if t = TyBool then TyBool else raise (SemanticError "Not on non-bool")
      end
  | Call (fname, args) ->
      let (param_types, ret_type) = StringMap.find fname env.funcs in
      if List.length param_types <> List.length args then
        raise (SemanticError "Function argument count mismatch")
      else
        List.iter2 (fun arg typ ->
          let argt = type_of_expr env arg in
          if argt <> typ then raise (SemanticError "Argument type mismatch")
        ) args param_types;
      ret_type
  | Assign (name, e) ->
      let vt = StringMap.find name env.vars in
      let et = type_of_expr env e in
      if vt <> et then raise (SemanticError "Assign type mismatch");
      vt

(* 声明语句中的变量 *)
let rec analyze_stmt (env: env) (stmt: stmt) : env =
  match stmt with
  | Decl (t, name) -> { env with vars = add_var env.vars name t }
  | AssignStmt (name, e) ->
      ignore (type_of_expr env e); env
  | Expr e ->
      ignore (type_of_expr env e); env
  | If (cond, s1, s2) ->
      ignore (type_of_expr env cond);
      ignore (analyze_stmt env s1);
      ignore (analyze_stmt env s2);
      env
  | While (cond, body) ->
      ignore (type_of_expr env cond);
      ignore (analyze_stmt env body);
      env
  | Return eo ->
      begin match eo with
      | None -> ()
      | Some e -> ignore (type_of_expr env e)
      end;
      env
  | Block stmts ->
      List.fold_left analyze_stmt env stmts

(* 整个函数 *)
let analyze_func (func_env: func_env) (f: func) : unit =
  let init_env = {
    funcs = func_env;
    vars = List.fold_left (fun acc (t, n) -> add_var acc n t) StringMap.empty f.args;
  } in
  ignore (analyze_stmt init_env f.body)

(* 程序分析入口 *)
let analyze_program (p: program) : unit =
  let func_env = build_func_env p in
  List.iter (analyze_func func_env) p *)






open Ast

(* 符号表类型 *)
module VarEnv = Map.Make(String)
type func_sig = { params: typ list; return: typ }
module FuncEnv = Map.Make(String)

type env = {
  vars: typ VarEnv.t;        (* 当前作用域下的变量环境 *)
  funcs: func_sig FuncEnv.t; (* 全局函数签名环境 *)
}

(* 构建函数符号表 - 收集函数名、参数类型（全为 int）、返回类型 *)
let build_func_env (funcs: func list) : env =
  let func_env =
    List.fold_left (fun acc f ->
      let param_types = List.map (fun _ -> TInt) f.params in
      FuncEnv.add f.fname { params = param_types; return = f.rettyp } acc
    ) FuncEnv.empty funcs
  in
  { vars = VarEnv.empty; funcs = func_env }

(* 分析函数：构建局部变量环境（参数和声明变量），不检查类型 *)
let analyze_func (env: env) (f: func) : env =
  (* 参数全部为 int 类型，加入变量环境 *)
  let var_env =
    List.fold_left (fun acc name ->
      VarEnv.add name TInt acc
    ) VarEnv.empty f.params
  in

  (* 递归分析语句列表，构建变量环境 *)
  let rec analyze_stmt env = function
    | Expr _ -> env
    | VarDecl (name, _) -> { env with vars = VarEnv.add name TInt env.vars }
    | If (_, s1, opt_s2) ->
        let env1 = analyze_stmt env s1 in
        (match opt_s2 with
         | Some s2 -> analyze_stmt env1 s2
         | None -> env1)
    | While (_, body) -> analyze_stmt env body
    | Break | Continue -> env
    | Return _ -> env
    | Block stmts -> List.fold_left analyze_stmt env stmts
  in

  let local_env = List.fold_left analyze_stmt { env with vars = var_env } f.body in
  (* 返回全局环境（不改变 func_env） *)
  { env with vars = local_env.vars }

(* 程序入口：构建符号表（函数 + 局部变量） *)
let analyze_program (p: program) : env =
  let base_env = build_func_env p in
  List.fold_left analyze_func base_env p
