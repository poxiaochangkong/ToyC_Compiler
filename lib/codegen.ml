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
(* • 分离临时与标签计数器
    • 布尔、比较、短路逻辑归一化为 0/1
    • 调用序列 param / call / retval
--------------------------------------------------------------*) 

open Ast

(* ---------- 计数器 ---------- *)
let temp_counter  = ref 0
let label_counter = ref 0
let fresh_tmp ()  = let t = "t" ^ string_of_int !temp_counter in incr temp_counter; t
let fresh_lbl pfx = let l = pfx ^ string_of_int !label_counter in incr label_counter; l

(* ============================================================= *)
(*  Expr → code * result tmp                                      *)
(* ============================================================= *)

let rec gen_expr (e : expr) : string list * string =
  match e with
  | IntLiteral n ->
      let t = fresh_tmp () in
      ([Printf.sprintf "%s = %d" t n], t)

  | Id x ->
      let t = fresh_tmp () in
      ([Printf.sprintf "%s = %s" t x], t)

  | Assign (x, rhs) ->
      let c_rhs, t_rhs = gen_expr rhs in
      let t_ret = fresh_tmp () in
      ( c_rhs
      @ [Printf.sprintf "%s = %s" x t_rhs;
         Printf.sprintf "%s = %s" t_ret x],
        t_ret )

  | BinOp (op, e1, e2) when List.mem op [Add;Sub;Mul;Div;Mod] ->
      let c1, t1 = gen_expr e1 in
      let c2, t2 = gen_expr e2 in
      let t = fresh_tmp () in
      let op_str = match op with
        | Add->"+"|Sub->"-"|Mul->"*"|Div->"/"|Mod->"%"| _ -> assert false in
      ( c1 @ c2 @ [Printf.sprintf "%s = %s %s %s" t t1 op_str t2], t)

  | BinOp (op, e1, e2) when List.mem op [Eq;Neq;Lt;Le;Gt;Ge] ->
      let c1, t1 = gen_expr e1 in
      let c2, t2 = gen_expr e2 in
      let t = fresh_tmp () in
      let l_true = fresh_lbl "L_true_" in
      let l_end  = fresh_lbl "L_end_" in
      let br = match op with
        | Eq->"beq" | Neq->"bne" | Lt->"blt" | Le->"ble" | Gt->"bgt" | Ge->"bge" | _->assert false in
      ( c1 @ c2
        @ [Printf.sprintf "%s = 0" t;
           Printf.sprintf "%s %s, %s, %s" br t1 t2 l_true;
           Printf.sprintf "j %s" l_end;
           l_true ^ ":";
           Printf.sprintf "%s = 1" t;
           l_end ^ ":"],
        t)

  | BinOp (op, e1, e2) when op = And || op = Or ->
      let c1, t1 = gen_expr e1 in
      let c2, t2 = gen_expr e2 in
      let t_res = fresh_tmp () in
      let l_short = fresh_lbl "L_short_" in
      let l_end   = fresh_lbl "L_end_"   in
      let set_short, set_normal, cond1, cond2 =
        if op = And then
          ("0", "1",
           Printf.sprintf "beqz %s, %s" t1 l_short,
           Printf.sprintf "beqz %s, %s" t2 l_short)
        else (* Or *)
          ("1", "0",
           Printf.sprintf "bnez %s, %s" t1 l_short,
           Printf.sprintf "bnez %s, %s" t2 l_short) in
      ( c1 @ [cond1] @ c2 @ [cond2;
          Printf.sprintf "%s = %s" t_res set_normal;
          Printf.sprintf "j %s" l_end;
          l_short ^ ":";
          Printf.sprintf "%s = %s" t_res set_short;
          l_end ^ ":"],
        t_res)

  | UnOp (Not, e1) ->
      let c, t1 = gen_expr e1 in
      let t = fresh_tmp () in
      let l_true = fresh_lbl "L_true_" in
      let l_end  = fresh_lbl "L_end_" in
      ( c @ [Printf.sprintf "%s = 0" t;
             Printf.sprintf "beqz %s, %s" t1 l_true;
             Printf.sprintf "j %s" l_end;
             l_true ^ ":";
             Printf.sprintf "%s = 1" t;
             l_end ^ ":"],
        t)

  | UnOp (Neg, e1) ->
      let c, t1 = gen_expr e1 in
      let t = fresh_tmp () in
      (c @ [Printf.sprintf "%s = - %s" t t1], t)

  | Call (fname, args) ->
      let arg_codes, arg_tmps = List.split (List.map gen_expr args) in
      let t_ret = fresh_tmp () in
      ( List.flatten arg_codes
        @ List.map (fun t -> "param " ^ t) arg_tmps
        @ [Printf.sprintf "call %s, %d" fname (List.length arg_tmps);
           Printf.sprintf "%s = retval" t_ret],
        t_ret )

  | _ -> failwith "unsupported expr"

(* ============================================================= *)
(*  Stmt → code list                                              *)
(* ============================================================= *)

let rec gen_stmt ?break_lbl ?cont_lbl (s : stmt) : string list =
  match s with
  | Block lst -> List.flatten (List.map (gen_stmt ?break_lbl ?cont_lbl) lst)

  | VarDecl (id, init_e) ->
      let c, t = gen_expr init_e in
      c @ [Printf.sprintf "%s = %s" id t]

  | Expr e -> fst (gen_expr e)

  | Return None -> ["ret 0"]

  | Return (Some e) -> let c, t = gen_expr e in c @ ["ret " ^ t]

  | If (cond, s_then, s_else_opt) ->
      let c_cond, t_c = gen_expr cond in
      let l_else = fresh_lbl "L_else_" in
      let l_end  = fresh_lbl "L_end_" in
      let then_c = gen_stmt ?break_lbl ?cont_lbl s_then in
      let else_c = match s_else_opt with Some s -> gen_stmt ?break_lbl ?cont_lbl s | None -> [] in
      c_cond
      @ [Printf.sprintf "beqz %s, %s" t_c l_else]
      @ then_c
      @ [Printf.sprintf "j %s" l_end; l_else ^ ":"]
      @ else_c
      @ [l_end ^ ":"]

  | While (cond, body) ->
      let l_begin = fresh_lbl "L_begin_" in
      let l_end   = fresh_lbl "L_end_"   in
      let c_cond, t_c = gen_expr cond in
      let body_c = gen_stmt ~break_lbl:l_end ~cont_lbl:l_begin body in
      [l_begin ^ ":"]
      @ c_cond
      @ [Printf.sprintf "beqz %s, %s" t_c l_end]
      @ body_c
      @ [Printf.sprintf "j %s" l_begin; l_end ^ ":"]

  | Break -> (match break_lbl with Some l -> ["j " ^ l] | None -> failwith "break not in loop")

  | Continue -> (match cont_lbl with Some l -> ["j " ^ l] | None -> failwith "continue not in loop")

  | _ -> failwith "stmt not supported"

(* ============================================================= *)
(*  Function / Program                                            *)
(* ============================================================= *)

let gen_func (f : func_def) : string list =
  let body = List.flatten (List.map (gen_stmt) f.body) in
  (f.fname ^ ":") :: body

let gen_program (p : program) : string list =
  List.flatten (List.map gen_func p)

