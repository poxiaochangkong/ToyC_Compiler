(* lib/semantic.ml *)
open Ast

(* 符号表类型 *)
module VarEnv = Map.Make(String)
type func_sig = { params: typ list;
return: typ }
module FuncEnv = Map.Make(String)

(* **FIX**: Define a module for string sets using the Set.Make functor *)
module StringSet = Set.Make(String)

(* 新类型：存储全局函数表 + 各函数的局部变量表 *)
type analysis_result = {
  global_funcs : func_sig FuncEnv.t;
(* 全局函数签名 *)
  local_vars  : (string * typ VarEnv.t) list;
(* 函数名 -> 其局部变量 *)
}

(* 构建全局函数符号表 *)
let build_func_env (funcs: func_def list) : func_sig FuncEnv.t =
  List.fold_left (fun acc (f:func_def) ->
    let param_types = List.map (fun _ -> TInt) f.params in
    FuncEnv.add f.fname { params = param_types; return = f.rettyp } acc
  ) FuncEnv.empty funcs

(*******************************************************************
 * MODIFIED: analyze_local_vars
 *******************************************************************)
let analyze_local_vars (f: func_def) : typ VarEnv.t =
  (* 参数名集合 *)
  let param_set = f.params |> List.to_seq |> 
StringSet.of_seq in
  (* all_locals: 累加函数中所有出现过的局部变量声明 *)
  let all_locals = ref VarEnv.empty in

  (* 递归分析语句, scopes 是一个 StringSet 的列表，代表作用域栈 *)
let rec analyze_stmts (scopes: StringSet.t list) (stmts: stmt list) : unit =
    (*
     * We use List.fold_left to iterate through the statements at the current level.
     * The accumulator for the fold is the scope stack. For each statement, we may
     * update the innermost scope (e.g., by adding a new variable).
     *)
    let process_statement current_scopes stmt =
      match stmt with
      | Expr _ -> current_scopes (* Expressions don't change scope *)
      | VarDecl (name, _) ->
          let current_scope_set = List.hd current_scopes in
          (* Check for duplicates in the current (innermost) scope. *)
          if StringSet.mem name current_scope_set then
            failwith ("Duplicate variable definition in the same scope: " ^ name);
          (* If in the outermost scope, also check against parameter names. *)
          if List.length current_scopes = 1 && StringSet.mem name param_set then
             failwith ("Duplicate variable definition, conflicts with parameter: " ^ name);

          (* Add the variable to our collection of all locals for the function. *)
          all_locals := VarEnv.add name TInt !all_locals;

          (* Return the updated scope stack, with the new variable added to the current scope. *)
          let new_current_scope_set = StringSet.add name current_scope_set in
          new_current_scope_set :: (List.tl current_scopes)

      | If (_, s1, opt_s2) ->
          (* Analyze the 'then' and 'else' branches. The scopes within them are
             self-contained and don't affect the scope of statements following the if. *)
          analyze_stmts current_scopes [s1];
          (match opt_s2 with
           | Some s2 -> analyze_stmts current_scopes [s2]
           | None -> ());
          current_scopes (* Return the original scopes for subsequent statements. *)

      | While (_, body) ->
          (* Analyze the loop body. This is a self-contained scope. *)
          analyze_stmts current_scopes [body];
          current_scopes (* Return original scopes. *)

      | Block inner_stmts ->
          (* For a new block, we add a new, empty scope to the stack and recurse. *)
          analyze_stmts (StringSet.empty :: current_scopes) inner_stmts;
          current_scopes (* After the block, we return the original scope stack. *)

      | _ -> current_scopes (* Break, Continue, Return don't affect scopes. *)
    in
    (* The fold's result is ignored because we only care about the side effects:
       populating `all_locals` and checking for errors. *)
    let _ = List.fold_left process_statement scopes stmts in
    ()
  in

  (* Start the analysis on the function body with a single, empty top-level scope. *)
  analyze_stmts [StringSet.empty] f.body;

  (* Return the map of all unique local variables found in the function. *)
  !all_locals

(* 程序入口：返回全局函数表 + 所有函数的局部变量表 *)
let analyze_program (p: program) : analysis_result =
  let global_funcs = build_func_env p in
  let local_vars =
    List.map (fun f ->
      (f.fname, analyze_local_vars f)
    ) p
  in
  { global_funcs;
local_vars }