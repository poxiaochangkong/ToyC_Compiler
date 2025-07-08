open Ast
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
  List.iter (analyze_func func_env) p
