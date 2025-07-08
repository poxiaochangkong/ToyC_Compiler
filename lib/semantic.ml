(* open Ast *)
(* 测试 *)
(* let check_program prog =
  print_endline "开始进行语义分析...";
  prog
;; *)


(* semantic.ml *)

open Ast
open Symbol
open Env

exception SemanticError of string
let error msg = raise (SemanticError msg)

(* 类型判断辅助 *)
let expect_type actual expected msg =
  if actual <> expected then error msg

(* 表达式检查，返回表达式类型 *)
let rec check_expr env = function
  | IntLiteral _ -> TInt

  | Var id ->
      (match find_var env id with
       | Some { v_type; initialized = true } -> v_type
       | Some _ -> error ("变量未初始化: " ^ id)
       | None -> error ("变量未声明: " ^ id))

  | Assign (id, expr) ->
      let t = check_expr env expr in
      (match find_var env id with
       | Some { v_type; _ } when v_type = t ->
           let env = update_var env id { v_type; initialized = true } in
           (env, t)
       | Some _ -> error "赋值类型不匹配"
       | None -> error ("变量未声明: " ^ id))

  | Binop (e1, _, e2) ->
      let t1 = check_expr env e1 in
      let t2 = check_expr env e2 in
      expect_type t1 TInt "二元操作数类型必须为 int";
      expect_type t2 TInt "二元操作数类型必须为 int";
      TInt

  | Call (fname, args) ->
      (match find_func env fname with
       | None -> error ("未定义函数: " ^ fname)
       | Some { ret_type; params } ->
           if List.length args <> List.length params then
             error ("函数参数个数不匹配: " ^ fname);
           List.iter2 (fun arg (expected_t, _) ->
             let actual_t = check_expr env arg in
             expect_type actual_t expected_t "函数参数类型不匹配")
             args params;
           ret_type
      )

      let rec check_stmt env = function
  | Decl (TInt, id, expr) ->
      let t = check_expr env expr in
      expect_type t TInt "变量初始化必须是 int 类型";
      add_var env id { v_type = TInt; initialized = true }

  | AssignStmt (id, expr) ->
      let t = check_expr env expr in
      (match find_var env id with
       | Some { v_type } when v_type = t ->
           update_var env id { v_type; initialized = true }
       | Some _ -> error "赋值类型错误"
       | None -> error ("变量未声明: " ^ id))

  | If (cond, s1, s2_opt) ->
      expect_type (check_expr env cond) TInt "if 条件必须为 int";
      ignore (check_stmt env s1);
      (match s2_opt with Some s2 -> ignore (check_stmt env s2) | None -> ());
      env

  | While (cond, body) ->
      expect_type (check_expr env cond) TInt "while 条件必须为 int";
      let env' = { env with in_loop = true } in
      ignore (check_stmt env' body);
      env

  | Break | Continue ->
      if env.in_loop then env
      else error "break/continue 必须在循环内使用"

  | Return None ->
      expect_type TVoid env.current_ret_type "缺少返回值"

  | Return (Some expr) ->
      let t = check_expr env expr in
      expect_type t env.current_ret_type "返回值类型不匹配";
      env

  | Block stmts ->
      let env' = push_scope env in
      let _ = List.fold_left check_stmt env' stmts in
      pop_scope env

      let check_func env (ret_type, name, params, body) =
  let param_env =
    List.fold_left (fun acc (t, id) ->
      add_var acc id { v_type = t; initialized = true })
      (push_scope env)
      params
  in
  let env' = { param_env with current_ret_type = ret_type; in_loop = false } in
  ignore (check_stmt env' body)

let check_program funcs =
  let env =
    List.fold_left (fun env (ret_type, name, params, _) ->
      add_func env name { ret_type; params })
      empty_env funcs
  in
  let has_main =
    List.exists (fun (ret_type, name, params, _) ->
      name = "main" && ret_type = TInt && params = []) funcs
  in
  if not has_main then error "必须包含 int main() 函数";
  List.iter (check_func env) funcs
