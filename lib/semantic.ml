(* 

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
 *)

 open Ast

(* 符号表类型 *)
module VarEnv = Map.Make(String)
type func_sig = { params: typ list; return: typ }
module FuncEnv = Map.Make(String)

(* 新类型：存储全局函数表 + 各函数的局部变量表 *)
type analysis_result = {
  global_funcs : func_sig FuncEnv.t;  (* 全局函数签名 *)
  local_vars  : (string * typ VarEnv.t) list;  (* 函数名 -> 其局部变量 *)
}

(* 构建全局函数符号表 *)
let build_func_env (funcs: func_def list) : func_sig FuncEnv.t =
  List.fold_left (fun acc (f:func_def) ->
    let param_types = List.map (fun _ -> TInt) f.params in
    FuncEnv.add f.fname { params = param_types; return = f.rettyp } acc
  ) FuncEnv.empty funcs

(* 分析单个函数的局部变量 *)
let analyze_local_vars (f: func_def) : typ VarEnv.t =
  (* 添加参数 *)
  let var_env = 
    List.fold_left (fun acc name ->
      VarEnv.add name TInt acc
    ) VarEnv.empty f.params
  in
  
  (* 递归分析语句 *)
  let rec analyze_stmt env = function
    | Expr _ -> env
    | VarDecl (name, _) -> 
        if VarEnv.mem name env then
          failwith ("Duplicate variable: " ^ name)
        else
          VarEnv.add name TInt env
    | If (_, s1, opt_s2) ->
        let env1 = analyze_stmt env s1 in
        (match opt_s2 with Some s2 -> analyze_stmt env1 s2 | None -> env1)
    | While (_, body) -> analyze_stmt env body
    | Block stmts -> List.fold_left analyze_stmt env stmts
    | _ -> env  (* Break, Continue, Return *)
  in
  
  List.fold_left analyze_stmt var_env f.body

(* 程序入口：返回全局函数表 + 所有函数的局部变量表 *)
let analyze_program (p: program) : analysis_result =
  let global_funcs = build_func_env p in
  let local_vars = 
    List.map (fun f -> 
      (f.fname, analyze_local_vars f)
    ) p
  in
  { global_funcs; local_vars }