open Symbol
module StringMap = Map.Make(String)

type var_table = var_info StringMap.t
type func_table = func_info StringMap.t

type env = {
  var_stack : var_table list;   (* 局部作用域栈，越靠前是越外层 *)
  funcs : func_table;           (* 全局函数表 *)
  current_ret_type : typ;       (* 当前正在检查的函数的返回类型 *)
  in_loop : bool;               (* 是否在循环中 *)
}

let empty_env = {
  var_stack = [StringMap.empty];
  funcs = StringMap.empty;
  current_ret_type = TVoid;
  in_loop = false;
}

(* 作用域管理 *)
let push_scope env =
  { env with var_stack = StringMap.empty :: env.var_stack }

let pop_scope env =
  match env.var_stack with
  | [] | [_] -> failwith "尝试弹出全局作用域"
  | _ :: rest -> { env with var_stack = rest }

let add_var env name info =
  match env.var_stack with
  | scope :: rest ->
      if StringMap.mem name scope then
        failwith ("重复定义变量: " ^ name)
      else
        { env with var_stack = (StringMap.add name info scope) :: rest }
  | [] -> failwith "无作用域可添加变量"

let rec find_var env name =
  let rec aux = function
    | [] -> None
    | scope :: rest ->
        match StringMap.find_opt name scope with
        | Some info -> Some info
        | None -> aux rest
  in
  aux env.var_stack

let update_var env name info =
  let rec aux acc = function
    | [] -> failwith ("变量未声明: " ^ name)
    | scope :: rest ->
        if StringMap.mem name scope then
          List.rev_append acc ((StringMap.add name info scope) :: rest)
        else
          aux (scope :: acc) rest
  in
  let new_stack = aux [] env.var_stack in
  { env with var_stack = new_stack }

(* 函数操作 *)
let add_func env name info =
  if StringMap.mem name env.funcs then
    failwith ("重复定义函数: " ^ name)
  else
    { env with funcs = StringMap.add name info env.funcs }

let find_func env name =
  StringMap.find_opt name env.funcs
