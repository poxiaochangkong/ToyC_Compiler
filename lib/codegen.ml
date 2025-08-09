(* lib/codegen.ml *)
open Ast
open Semantic

(*******************************************************************
 * 1. 中间表示 (Intermediate Representation) 和环境定义
 *******************************************************************)

(**
 * 操作数类型。
 * 这是 codegen.mli 中 `type operand` 的具体实现。
 *)
type operand =
  | Imm of int          (* 立即数, e.g., 5 *)
  | Reg of string       (* 物理或虚拟寄存器, e.g., "t0", "a0" *)
  | Stack of int        (* 栈上位置 (相对于fp的偏移量), e.g., -4, -8 *)

(* * 为了在没有 .mli 文件的情况下打破模块间的类型依赖，
 * 我们在内部复制一份操作符的定义。
 *)
type ir_binop = | IR_Add | IR_Sub | IR_Mul | IR_Div | IR_Mod
                | IR_Eq | IR_Neq | IR_Lt | IR_Le | IR_Gt | IR_Ge
                | IR_And | IR_Or
type ir_unop = IR_Neg | IR_Not

(**
 * 中间表示 (IR) 类型。
 * 这是 codegen.mli 中 `type ir` 的具体实现。
 *)
type ir =
  | Label of string
  | Li of operand * int
  | Move of operand * operand
  | BinOp of ir_binop * operand * operand * operand
  | UnOp of ir_unop * operand * operand
  | Load of operand * operand
  | Store of operand * operand
  | Branch of ir_binop * operand * operand * string
  | BranchZ of operand * string
  | BranchNZ of operand * string
  | Jump of string
  | Call of string * int
  | Ret
  | Prologue of string * int
  | Epilogue of string * int

(* 代码生成环境 (内部使用) *)
type cg_env = {
  funcs: func_sig FuncEnv.t;
  vars: int VarEnv.t list; (* A stack of scopes，变成list存储不同作用域 *)
  stack_top: int ref;      
  mutable temp_counter: int;
  mutable label_counter: int;
  current_function_name: string; (* NEW: Store the current function's name，用于label命名 *)
}

(* 在作用域栈中查找变量 *)
let find_var_offset (env: cg_env) (name: string) : int =
  let rec find_in_scopes scopes =
    match scopes with
    | [] -> failwith ("Undeclared variable: " ^ name)
    | current_scope :: outer_scopes ->
        match VarEnv.find_opt name current_scope with
        | Some offset -> offset
        | None -> find_in_scopes outer_scopes
  in
  find_in_scopes env.vars

(* 创建一个新的临时寄存器名 (t0-t5) *)
let fresh_temp_reg env =
  if env.temp_counter >= 6 then failwith "Expression too complex, ran out of temporary registers";
let reg_name = "t" ^ string_of_int env.temp_counter in
  env.temp_counter <- env.temp_counter + 1;
Reg reg_name

(* 创建一个新的标签，加上函数名前缀 *)
let fresh_label env pfx =
  let label_name = Printf.sprintf "%s_%s%d" env.current_function_name pfx env.label_counter in
  env.label_counter <- env.label_counter + 1;
  label_name

(* 在栈上为临时计算结果分配空间 *)
let alloc_temp_stack_slot env =
  env.stack_top := !(env.stack_top) - 4;
Stack !(env.stack_top)

(* 将 Ast 操作符转换为内部 IR 操作符的辅助函数 *)
let binop_from_ast_op op =
  match op with
  | Add -> IR_Add | Sub -> IR_Sub | Mul -> IR_Mul | Div -> IR_Div | Mod -> IR_Mod
  | Eq -> IR_Eq | Neq -> IR_Neq | Lt -> IR_Lt | Le -> IR_Le | Gt -> IR_Gt | Ge -> IR_Ge
  | And -> IR_And | Or -> IR_Or

let unop_from_ast_op op =
  match op with
  | Neg -> IR_Neg | Not -> IR_Not

(*******************************************************************
 * 2. 从 AST 到 IR 的转换 (内部函数)
 *******************************************************************)

(* 在 lib/codegen.ml 中，替换掉整个这个函数 *)

let rec gen_expr_ir_internal env (e: expr) : ir list * operand =
  match e with
  | IntLiteral n ->
      let temp_reg = fresh_temp_reg env in
      [Li (temp_reg, n)], temp_reg
  | Id x ->
      let var_loc = find_var_offset env x in
      let temp_reg = fresh_temp_reg env in
      [Load (temp_reg, Stack var_loc)], temp_reg
  | Assign (x, rhs_expr) ->
      let rhs_ir, rhs_op = gen_expr_ir_internal env rhs_expr in
      let var_loc = find_var_offset env x in
      rhs_ir @ [Store (rhs_op, Stack var_loc)], rhs_op
  | UnOp (op, expr) ->
      let expr_ir, expr_op = gen_expr_ir_internal env expr in
      let dest_reg = fresh_temp_reg env in
      expr_ir @ [UnOp (unop_from_ast_op op, dest_reg, expr_op)], dest_reg
  | BinOp (op, e1, e2) ->
    (match op with
    (* 短路求值逻辑保持不变 *)
    | And ->
        let dest_reg = fresh_temp_reg env in
        let false_label = fresh_label env "L_false_" in
        let end_label = fresh_label env "L_end_" in
        let ir1, op1 = gen_expr_ir_internal env e1 in
        env.temp_counter <- 0;
        let ir2, op2 = gen_expr_ir_internal env e2 in
        ir1 @ [BranchZ(op1, false_label)] @ ir2 @ [BranchZ(op2, false_label)] @
        [Li(dest_reg, 1); Jump(end_label); Label(false_label); Li(dest_reg, 0); Label(end_label)], dest_reg
    | Or ->
        let dest_reg = fresh_temp_reg env in
        let true_label = fresh_label env "L_true_" in
        let end_label = fresh_label env "L_end_" in
        let ir1, op1 = gen_expr_ir_internal env e1 in
        env.temp_counter <- 0;
        let ir2, op2 = gen_expr_ir_internal env e2 in
        ir1 @ [BranchNZ(op1, true_label)] @ ir2 @ [BranchNZ(op2, true_label)] @
        [Li(dest_reg, 0); Jump(end_label); Label(true_label); Li(dest_reg, 1); Label(end_label)], dest_reg

    (* 【最终修复】针对算术和比较运算的稳健、高效逻辑 *)
    | _ ->
        (* 1. 首先计算左操作数 e1 *)
        let ir1, op1 = gen_expr_ir_internal env e1 in

        (* 2. 将其结果保存到栈上的一个临时槽位 *)
        let temp_slot_e1 = alloc_temp_stack_slot env in
        let save_ir1 = [Store(op1, temp_slot_e1)] in

        (* 3. 重置寄存器计数器，然后计算右操作数 e2 *)
        env.temp_counter <- 0;
        let ir2, op2 = gen_expr_ir_internal env e2 in

        (* 4. 从栈中加载 e1 的结果到一个新的临时寄存器 *)
        let loaded_op1 = fresh_temp_reg env in
        let load_ir1 = [Load(loaded_op1, temp_slot_e1)] in
        
        (* 5. e2 的结果 (op2) 将被用作目标寄存器 *)
        let dest_op = op2 in
        
        (* 6. 根据操作符生成最终指令 *)
        let final_op = binop_from_ast_op op in
        (match final_op with
        | IR_Eq | IR_Neq | IR_Lt | IR_Le | IR_Gt | IR_Ge ->
            let true_label = fresh_label env "L_true_" in
            let end_label = fresh_label env "L_end_" in
            (* 指令组合: 计算e1 -> 保存e1 -> 计算e2 -> 加载e1 -> 比较与跳转 *)
            ir1 @ save_ir1 @ ir2 @ load_ir1 @
            [
              Li(dest_op, 0); 
              Branch(final_op, loaded_op1, dest_op, true_label); 
              Jump(end_label);
              Label(true_label); 
              Li(dest_op, 1); 
              Label(end_label)
            ], dest_op
        | _ ->
            (* 算术运算: 计算e1 -> 保存e1 -> 计算e2 -> 加载e1 -> 执行运算 *)
            ir1 @ save_ir1 @ ir2 @ load_ir1 @
            [BinOp(final_op, dest_op, loaded_op1, dest_op)], dest_op
        )
    )
   | Call (fname, args) ->
    (* 1. 像之前一样，计算所有参数的值，并存入临时的栈槽(-fp)中 *)
    let (eval_args_ir, temp_arg_slots_rev) =
      List.fold_left
        (fun (acc_ir, acc_slots) arg_expr ->
          env.temp_counter <- 0;
          let (arg_ir, arg_op) = gen_expr_ir_internal env arg_expr in
          let temp_slot = alloc_temp_stack_slot env in
          let store_ir = [Store (arg_op, temp_slot)] in
          (acc_ir @ arg_ir @ store_ir, temp_slot :: acc_slots)
        )
        ([], [])
        args
    in
    let temp_arg_slots = List.rev temp_arg_slots_rev in
    env.temp_counter <- 0;

    (* 2. 分离寄存器参数和栈参数 *)
    let reg_arg_slots, stack_arg_slots =
      let rec split n lst = if n <= 0 then ([], lst) else match lst with | [] -> ([], []) | h :: t -> let (taken, rest) = split (n - 1) t in (h :: taken, rest)
      in split 8 temp_arg_slots
    in
    let num_stack_args = List.length stack_arg_slots in
    let stack_space_for_args = num_stack_args * 4 in

    (* 3. 【新】为栈参数预分配空间 (生成 addi sp, sp, -N) *)
    let ir_alloc_stack =
      if stack_space_for_args > 0 then
        [BinOp(IR_Sub, Reg "sp", Reg "sp", Imm stack_space_for_args)]
      else
        []
    in

    (* 4. 加载寄存器参数 *)
    let ir_pass_reg_args =
      List.mapi (fun i temp_slot ->
        let reg = Reg ("a" ^ string_of_int i) in
        [Load(reg, temp_slot)]
      ) reg_arg_slots |> List.concat
    in

    (* 5. 【修正】加载并存储栈参数到【正确】的位置 (0(sp), 4(sp)...) *)
    let ir_pass_stack_args =
      List.mapi (fun i temp_slot ->
        env.temp_counter <- 0;
        let temp_reg = fresh_temp_reg env in
        (* 偏移量现在从0开始，相对于新的sp *)
        [Load(temp_reg, temp_slot); Store(temp_reg, Stack (i * 4))]
      ) stack_arg_slots |> List.concat
    in
    
    (* 6. 执行函数调用 (不再需要调整sp) *)
    let ir_call = [Call (fname, 0)] in (* 第二个参数设为0 *)

    (* 7. 【新】释放为栈参数分配的空间 (生成 addi sp, sp, N) *)
    let ir_dealloc_stack =
      if stack_space_for_args > 0 then
        [BinOp(IR_Add, Reg "sp", Reg "sp", Imm stack_space_for_args)]
      else
        []
    in

    (* 8. 处理返回值 *)
    env.temp_counter <- 0;
    let temp_ret_reg = fresh_temp_reg env in
    let ir_move_ret = [Move (temp_ret_reg, Reg "a0")] in

    (* 9. 【关键】按正确顺序组合所有IR *)
    ( eval_args_ir @         (* a. 计算所有参数值 *)
      ir_pass_reg_args @     (* b. 准备寄存器参数 *)
      ir_alloc_stack @       (* c. 【先】分配栈空间 *)
      ir_pass_stack_args @   (* d. 【后】将参数存入新空间 *)
      ir_call @              (* e. 调用函数 *)
      ir_dealloc_stack @     (* f. 释放栈空间 *)
      ir_move_ret,           (* g. 保存返回值 *)
      temp_ret_reg
    )
(* _ -> failwith "Unsupported expression type in codegen" *)


let rec gen_stmt_ir_internal (env: cg_env) ?break_lbl ?cont_lbl (s: stmt) : ir list * cg_env =
  env.temp_counter <- 0; (* MODIFIED: Do not create a copy of the environment.  *)

  match s with
  | Expr e ->
      let (ir, _) = gen_expr_ir_internal env e in
      (ir, env) (* The environment does not change for an expression statement. *)

  | Return None -> ([Ret], env)
  | Return (Some e) ->
      let (ir, op) = gen_expr_ir_internal env e in
      (ir @ [Move (Reg "a0", op); Ret], env)

  | VarDecl (id, init_e) ->
      (* 1. Generate IR for the initializer using the CURRENT environment. *)
      let (init_ir, op) = gen_expr_ir_internal env init_e in

      (* 2. Allocate stack space for the new variable. *)
      env.stack_top := !(env.stack_top) - 4;
      let var_loc = !(env.stack_top) in

      (* 3. Create the store instruction. *)
      let store_ir = [Store (op, Stack var_loc)] in

      (* 4. Create the NEW environment for subsequent statements by updating the vars list. *)
      let new_current_scope = VarEnv.add id var_loc (List.hd env.vars) in
      let new_env = { env with vars = new_current_scope :: (List.tl env.vars) } in

      (init_ir @ store_ir, new_env) (* Return the IR and the MODIFIED environment. *)

  | Block stmts ->
      (* 1. Enter a new scope by pushing an empty var map. *)
      let block_env = { env with vars = VarEnv.empty :: env.vars } in
      (* 2. Process the statements within the new scope. *)
      let (block_ir, _) = gen_stmts_ir_internal block_env ?break_lbl ?cont_lbl stmts in
      (* 3. Exit the scope by returning the original environment. *)
      (block_ir, env)

  | If (cond, then_s, else_s_opt) ->
      (* All sub-expressions and statements will now use the same `env` instance,
         ensuring the label_counter is correctly incremented and never reset. *)
      let (cond_ir, cond_op) = gen_expr_ir_internal env cond in
      let else_label = fresh_label env "L_else_" in
      let end_label = fresh_label env "L_end_" in
      let (then_ir, _) = gen_stmt_ir_internal env ?break_lbl ?cont_lbl then_s in
      (match else_s_opt with
      | None ->
          (cond_ir @ [BranchZ (cond_op, end_label)] @ then_ir @ [Label end_label], env)
      | Some else_s ->
          let (else_ir, _) = gen_stmt_ir_internal env ?break_lbl ?cont_lbl else_s in
          (cond_ir @ [BranchZ (cond_op, else_label)] @ then_ir @ [Jump end_label; Label else_label] @ else_ir @ [Label end_label], env)
      )
  | While (cond, body) ->
      let start_label = fresh_label env "L_while_start_" in
      let end_label = fresh_label env "L_while_end_" in
      let (cond_ir, cond_op) = gen_expr_ir_internal env cond in
      let (body_ir, _) = gen_stmt_ir_internal env ~break_lbl:end_label ~cont_lbl:start_label body in
      ([Label start_label] @ cond_ir @ [BranchZ (cond_op, end_label)] @ body_ir @ [Jump start_label; Label end_label], env)

  | Break -> (match break_lbl with Some lbl -> ([Jump lbl], env) | None -> failwith "break statement not within a loop")
  | Continue -> (match cont_lbl with Some lbl -> ([Jump lbl], env) | None -> failwith "continue statement not within a loop")

(* This helper function processes a LIST of statements,
 * passing the updated environment from one statement to the next. *)
and gen_stmts_ir_internal (env: cg_env) ?break_lbl ?cont_lbl (stmts: stmt list) : ir list * cg_env =
  List.fold_left
    (fun (acc_ir, current_env) stmt ->
      let (new_ir, next_env) = gen_stmt_ir_internal current_env ?break_lbl ?cont_lbl stmt in
      (acc_ir @ new_ir, next_env)
    )
    ( [], env )
    stmts

(* MODIFIED: This function now correctly calculates stack size after generating the body IR. *)
(* 在 lib/codegen.ml 中，完全替换掉这个函数 *)

(**************************** 替换为以下代码 ****************************)
let gen_func_ir_internal (ana: analysis_result) (f: func_def) : ir list =
  (* 1. 为所有参数在本地栈帧中分配“家” (local storage) *)
  let param_offset = ref (-8) in (* fp-4是ra, fp-8是旧fp *)
  let params_with_local_offsets =
    List.map (fun name ->
      param_offset := !param_offset - 4;
      (name, !param_offset)
    ) f.params
  in
  (* 【修复一】: 使用 List.fold_left 替代 VarEnv.of_list *)
  let initial_var_map =
    List.fold_left
      (fun acc (name, offset) -> VarEnv.add name offset acc)
      VarEnv.empty
      params_with_local_offsets
  in

  (* 2. 生成将传入参数值复制到这些“新家”的IR指令 *)
  let copy_params_ir =
    (* 【修复二】: 将未使用的 'name' 变量改为 '_name' *)
    List.mapi (fun i (_name, local_offset) ->
      if i < 8 then
        (* a. 对于寄存器参数 (a0-a7): 直接存入新家 *)
        let reg_name = "a" ^ string_of_int i in
        [Store (Reg reg_name, Stack local_offset)]
      else
        (* b. 对于栈参数: 先从调用者栈帧加载，再存入新家 *)
        let source_offset_from_fp = 0 + (i - 8) * 4 in
        let temp_reg = Reg "t0" in (* 使用一个固定的临时寄存器 *)
        [
          Load (temp_reg, Stack source_offset_from_fp);
          Store (temp_reg, Stack local_offset)
        ]
    ) params_with_local_offsets |> List.concat
  in

  (* 3. 创建初始代码生成环境 *)
  let env = {
    funcs = ana.global_funcs;
    vars = [initial_var_map];
    stack_top = ref !param_offset;
    temp_counter = 0;
    label_counter = 0;
    current_function_name = f.fname;
  } in

  (* 4. 生成函数体的IR *)
  let (body_ir, final_env) = gen_stmts_ir_internal env f.body in

  (* 5. 计算最终需要的总栈大小，并确保16字节对齐 *)
  let required_stack = abs !(final_env.stack_top) + 8 in
  let stack_size = if required_stack mod 16 == 0 then required_stack else required_stack + (16 - required_stack mod 16) in

  (* 6. 组合最终的函数IR *)
  [Prologue (f.fname, stack_size)] @
  copy_params_ir @
  body_ir @
  [Epilogue (f.fname, stack_size)]
(**************************** 替换结束 ****************************)
(*******************************************************************
 * 3. 从 IR 到 RISC-V 汇编的转换 (内部函数)
 *******************************************************************)
(* This section remains unchanged *)
let ir_to_asm_list_internal (ir_instr: ir) : string list =
  let op_to_str op =
    match op with
    | Imm i -> string_of_int i
    | Reg s -> s
    | Stack i -> Printf.sprintf "%d(fp)" i (* 栈操作数总是相对于fp *)
  in
  match ir_instr with
  | Label s -> [s ^ ":"]
  | Li (dest, imm) -> [Printf.sprintf "  li %s, %d" (op_to_str dest) imm]
  | Move (dest, src) -> [Printf.sprintf "  mv %s, %s" (op_to_str dest) (op_to_str src)]
  | Load (dest, src) -> [Printf.sprintf "  lw %s, %s" (op_to_str dest) (op_to_str src)]
  | Store (src, Stack i) when i < 0 ->
      (* 存储到fp负偏移量的本地变量/临时槽位 *)
      [Printf.sprintf "  sw %s, %d(fp)" (op_to_str src) i]
  | Store (src, Stack i) ->
      (* 存储到sp正偏移量的参数槽位 *)
      [Printf.sprintf "  sw %s, %d(sp)" (op_to_str src) i]
  | Store (src, dest) ->
      (* 这是一个备用情况，理论上不应发生 *)
      [Printf.sprintf "  sw %s, %s" (op_to_str src) (op_to_str dest)]
  | Jump s -> [Printf.sprintf "  j %s" s]
  | Ret -> failwith "Ret should not be directly converted, it's handled by Epilogue"
  
  (*** 这是本次的核心 ***)
  | Call (s, stack_arg_space) ->
      if stack_arg_space > 0 then
        (* 如果有栈参数，正确地调整sp来分配和释放空间 *)
        [ Printf.sprintf "  addi sp, sp, -%d" stack_arg_space;
          Printf.sprintf "  call %s" s;
          Printf.sprintf "  addi sp, sp, %d" stack_arg_space; ]
      else
        (* 如果没有栈参数，直接调用即可 *)
        [Printf.sprintf "  call %s" s]
  (*************************)
        
  | UnOp (op, dest, src) ->
      let op_str = match op with IR_Neg -> "neg" | IR_Not -> "seqz" in
      [Printf.sprintf "  %s %s, %s" op_str (op_to_str dest) (op_to_str src)]
  | BinOp (op, dest, src1, Imm imm) ->
      let op_str = match op with
        | IR_Add -> "addi" | IR_Sub -> "addi"
        | _ -> failwith "Unsupported immediate operation for this BinOp"
      in
      let immediate = if op = IR_Sub then -imm else imm in
      [Printf.sprintf "  %s %s, %s, %d" op_str (op_to_str dest) (op_to_str src1) immediate]
  | BinOp (op, dest, src1, src2) ->
      let op_str = match op with
        | IR_Add -> "add" | IR_Sub -> "sub" | IR_Mul -> "mul"
        | IR_Div -> "div" | IR_Mod -> "rem"
        | _ -> failwith "Unsupported register-register operation for this BinOp"
      in
      [Printf.sprintf "  %s %s, %s, %s" op_str (op_to_str dest) (op_to_str src1) (op_to_str src2)]
  | BranchZ (src, label) -> [Printf.sprintf "  beqz %s, %s" (op_to_str src) label]
  | BranchNZ (src, label) -> [Printf.sprintf "  bnez %s, %s" (op_to_str src) label]
  | Branch (op, src1, src2, label) ->
      let branch_op_str = match op with | IR_Eq -> "beq" | IR_Neq -> "bne" | IR_Lt -> "blt" | IR_Le -> "ble" | IR_Gt -> "bgt" | IR_Ge -> "bge" | _ -> failwith "Invalid op" in
      (match src1, src2 with
       | Reg _, Reg _ -> [Printf.sprintf "  %s %s, %s, %s" branch_op_str (op_to_str src1) (op_to_str src2) label]
       | Reg r, Imm i -> [Printf.sprintf "  li t6, %d" i; Printf.sprintf "  %s %s, t6, %s" branch_op_str r label]
       | Imm i, Reg r -> [Printf.sprintf "  li t6, %d" i; Printf.sprintf "  %s t6, %s, %s" branch_op_str r label]
       | _ -> failwith "Invalid operands for branch")
  | Prologue (fname, stack_size) ->
      [ "  .text"; "  .globl " ^ fname; fname ^ ":";
        Printf.sprintf "  addi sp, sp, -%d" stack_size;
        Printf.sprintf "  sw ra, %d(sp)" (stack_size - 4);
        Printf.sprintf "  sw fp, %d(sp)" (stack_size - 8);
        Printf.sprintf "  addi fp, sp, %d" stack_size; ]
  | Epilogue (fname, stack_size) ->
      [ ".L_ret_" ^ fname ^ ":";
        Printf.sprintf "  lw fp, %d(sp)" (stack_size - 8);
        Printf.sprintf "  lw ra, %d(sp)" (stack_size - 4);
        Printf.sprintf "  addi sp, sp, %d" stack_size;
        "  ret"; ]

(*******************************************************************
 * 4. 公共接口 (Public Interface)
 *******************************************************************)
(* This section remains unchanged *)
let string_of_ir (ir_instr: ir) : string =
  let op_to_str op = match op with
    | Imm i -> string_of_int i
    | Reg s -> s
    | Stack i -> Printf.sprintf "stack[%d]" i
  in
  let binop_to_str op = match op with
    | IR_Add -> "+" | IR_Sub -> "-" | IR_Mul -> "*" | IR_Div -> "/" | IR_Mod -> "%"
    | IR_Eq -> "==" | IR_Neq -> "!=" | IR_Lt -> "<" | IR_Le -> "<=" | IR_Gt -> ">" | IR_Ge -> ">="
    | IR_And -> "&&" | IR_Or -> "||"
  in
  match ir_instr with
  | Label s -> s ^ ":"
  | Li (dest, imm) -> Printf.sprintf "  %s = %d" (op_to_str dest) imm
  | Move (dest, src) -> Printf.sprintf "  %s = %s" (op_to_str dest) (op_to_str src)
  | Load (dest, src) -> Printf.sprintf "  %s = *%s" (op_to_str dest) (op_to_str src)
  | Store (src, dest) -> Printf.sprintf "  *%s = %s" (op_to_str dest) (op_to_str src)
  | Jump s -> Printf.sprintf "  j %s" s
  | Ret -> "  ret"
  | Call (s, n) -> Printf.sprintf "  call %s, %d" s n
  | UnOp (op, dest, src) ->
      let op_str = match op with IR_Neg -> "-" | IR_Not -> "!" in
      Printf.sprintf "  %s = %s%s" (op_to_str dest) op_str (op_to_str src)
  | BinOp (op, dest, src1, src2) ->
      Printf.sprintf "  %s = %s %s %s" (op_to_str dest) (op_to_str src1) (binop_to_str op) (op_to_str src2)
  | BranchZ (src, label) -> Printf.sprintf "  ifz %s j %s" (op_to_str src) label
  | BranchNZ (src, label) -> Printf.sprintf "  ifnz %s j %s" (op_to_str src) label
  | Branch (op, src1, src2, label) ->
      Printf.sprintf "  if %s %s %s j %s" (op_to_str src1) (binop_to_str op) (op_to_str src2) label
  | Prologue (fname, size) -> Printf.sprintf "prologue %s, %d" fname size
  | Epilogue (fname, size) -> Printf.sprintf "epilogue %s, %d" fname size

let gen_program (p: program) : ir list =
  let ana = Semantic.analyze_program p in
  List.concat_map (gen_func_ir_internal ana) p

let gen_assembly (ir_code: ir list) : string list =
  let current_fname = ref "" in
  let convert_ir_to_asm ir =
    (match ir with
    | Prologue (fname, _) -> current_fname := fname
    | Epilogue (fname, _) -> current_fname := fname
    | _ -> ());
if ir = Ret then [Printf.sprintf "  j .L_ret_%s" !current_fname]
    else ir_to_asm_list_internal ir
  in
  List.concat_map convert_ir_to_asm ir_code

let generate_code (p: program) : string =
  let ir = gen_program p in
  let asm_lines = gen_assembly ir in
  String.concat "\n" asm_lines

let compile_source (src: string) : string =
  let lexbuf = Lexing.from_string src in
  let ast = Parser.program Lexer.token lexbuf in
  generate_code ast