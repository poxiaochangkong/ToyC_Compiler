(* (* lib/codegen.ml *)
open Ast

let generate_code (prog : program) : string =
  let buffer = Buffer.create 1024 in
  (* 目前我们只处理程序中的第一个函数 *)
  let main_func = List.hd prog in
  (* 生成汇编代码头部 *)
  Buffer.add_string buffer ".globl main\n";
  Buffer.add_string buffer "main:\n";
  (* 函数序言 (Function Prologue) *)
  Buffer.add_string buffer "  push %rbp\n";
  Buffer.add_string buffer "  mov %rsp, %rbp\n";
  (* 遍历函数体内的语句 *)
  List.iter
    (fun statement ->
       match statement with
       | Return expr ->
         (match expr with
          | IntLiteral n ->
            (* 将返回值放入 %eax 寄存器 *)
            Buffer.add_string buffer (Printf.sprintf "  mov $%d, %%eax\n" n)
          | _ -> failwith "Code generation for this expression is not implemented yet")
       | _ -> failwith "Code generation for this statement is not implemented yet")
    main_func.body;
  (* 函数尾声 (Function Epilogue) *)
  Buffer.add_string buffer "  pop %rbp\n";
  Buffer.add_string buffer "  ret\n";
  Buffer.contents buffer
;;
*)
(* codegen.ml *)

open Ast (* 确保 ast.ml 已经存在并被正确编译和链接 *)

let rec gen_expr expr temp_counter =
  match expr with
  | IntLiteral n -> 
      let t = "t" ^ string_of_int !temp_counter in
      incr temp_counter;
      ([t ^ " = " ^ string_of_int n], t)
  | BinOp (op, e1, e2) ->
      let (code1, t1) = gen_expr e1 temp_counter in
      let (code2, t2) = gen_expr e2 temp_counter in
      let t = "t" ^ string_of_int !temp_counter in
      incr temp_counter;
      let op_str = match op with
        | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"
        | Mod -> "%" | Eq -> "==" | Neq -> "!=" 
        | Lt -> "<" | Le -> "<=" | Gt -> ">" | Ge -> ">="
        | And -> "&&" | Or -> "||" in
      (code1 @ code2 @ [t ^ " = " ^ t1 ^ " " ^ op_str ^ " " ^ t2], t)
  | UnOp (op, e) ->
      let (code, t1) = gen_expr e temp_counter in
      let t = "t" ^ string_of_int !temp_counter in
      incr temp_counter;
      let op_str = match op with
        | Neg -> "-" | Not -> "!" in
      (code @ [t ^ " = " ^ op_str ^ t1], t)
  | Assign (x, e) ->
      let (code, t) = gen_expr e temp_counter in
      (code @ [x ^ " = " ^ t], x)
  | _ -> failwith "Not implemented yet"

let rec gen_stmt stmt temp_counter =
  match stmt with
  | Expr e ->
      let (code, _) = gen_expr e temp_counter in
      code
  | Return (Some e) ->
      let (code, t) = gen_expr e temp_counter in
      code @ ["ret " ^ t]
  | Return None ->
      ["ret"]
  | If (cond, then_stmt, else_stmt_opt) ->
      let (cond_code, t_cond) = gen_expr cond temp_counter in
      let then_code = gen_stmt then_stmt temp_counter in
      let else_code =
        match else_stmt_opt with
        | Some s -> gen_stmt s temp_counter
        | None -> []
      in
      let label_else = "L_else_" ^ string_of_int !temp_counter in
      let label_end = "L_end_" ^ string_of_int (!temp_counter + 1) in
      incr temp_counter; incr temp_counter;
      cond_code
      @ [Printf.sprintf "beqz %s, %s" t_cond label_else]
      @ then_code
      @ [Printf.sprintf "j %s" label_end]
      @ [label_else ^ ":"]
      @ else_code
      @ [label_end ^ ":"]
  | _ -> failwith "Not implemented"

let gen_func f =
  let header = Printf.sprintf "%s:\n" f.fname in
  let temp_counter = ref 0 in
  let body =
    List.map (fun stmt -> gen_stmt stmt temp_counter) f.body
    |> List.flatten
    |> String.concat "\n"
  in
  header ^ body
  


let gen_program (p : program) =
  List.map gen_func p |> String.concat "\n"
