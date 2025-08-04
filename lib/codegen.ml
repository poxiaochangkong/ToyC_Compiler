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
    假设物理寄存器6个
--------------------------------------------------------------*)

open Ast
open Semantic

(* ---------- 1. 计数器 ---------- *)
(*用于生成临时变量和标签
1.临时变量用于存储中间计算结果。
2.标签用于控制流（如条件跳转、循环）*)
let temp_counter = ref 0
let label_counter = ref 0

let fresh_tmp () =  (*生成唯一的临时变量（如 t0, t1）*)
  let t = "t" ^ string_of_int !temp_counter in
  incr temp_counter;
  t
;;

let fresh_lbl pfx = (*生成唯一的标签（如 L0, L1）*)
  let l = pfx ^ string_of_int !label_counter in
  incr label_counter;
  l
;;

exception SemanticError of string (* 查符号失败时抛出 *)

(* ---------- 2. 符号表环境 ---------- *)
(* 把语义阶段的符号信息打包成 codegen 用的 env *)
(*make_env 函数将符号表转换为代码生成环境（cg_env）
1.vars：存储局部变量的类型信息。
2.funcs：存储全局函数的签名信息。
*)
type cg_env =
  { vars : typ Semantic.VarEnv.t
  ; funcs : func_sig Semantic.FuncEnv.t 
  }

let make_env ~(globals : func_sig FuncEnv.t) ~(locals : typ VarEnv.t) : cg_env =
  { vars = locals; funcs = globals }
;;

(* ============================================================= *)
(*  3.表达式生成    Expr → code * result tmp                      *)
(* ============================================================= *)
(*将expr转换成中间表示IR*)

(*1.string list:生成的IR指令*)
(*2.string :表达式结果存储的临时变量 如：返回值：["t0 = 42"], "t0"  *) 
(*debug:gen_expr函数会为每个变量引用都生成新的临时变量。
当处理 return x 时，会生成额外的临时变量来存储 x 的值
debug2:gen_expr导致IR阶段beq t0, t1, L_true_0中间有逗号，转汇编代码时有逗号重复的问题*)
let rec gen_expr env (e : expr) : string list * string = 
  match e with
  | IntLiteral n ->
    let t = fresh_tmp () in                   (* 输入：42*)
    [ Printf.sprintf "%s = %d" t n ], t       (* 输出：t0=42 *)
  | Id x ->                                     
    (match VarEnv.find_opt x env.vars with
     | None -> raise (SemanticError ("unbound variable " ^ x))
     | Some _ ->
       let t = fresh_tmp () in                        (* 输入：x*)
       [ Printf.sprintf "%s = %s" t x ], t)           (* 输出：t1=x*)
  | Assign (x, rhs) ->
    if VarEnv.find_opt x env.vars = None
    then raise (SemanticError ("assignment to undeclared " ^ x));
    let c_rhs, t_rhs = gen_expr env rhs in
    let t_ret = fresh_tmp () in                                                           (* 输入：x = 42 *)
    c_rhs @ [ Printf.sprintf "%s = %s" x t_rhs; Printf.sprintf "%s = %s" t_ret x ], t_ret (* 输出：t2 = x, t2 = 42 *)
  | BinOp (op, e1, e2) when List.mem op [ Add; Sub; Mul; Div; Mod ] ->
    let c1, t1 = gen_expr env e1 in
    let c2, t2 = gen_expr env e2 in
    let t = fresh_tmp () in
    let op_str =
      match op with
      | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Mod -> "%" | _ -> assert false
    in                                                                       (* 输入：e1 + e2 *)                    
    c1 @ c2 @ [ Printf.sprintf "%s = %s %s %s" t t1 op_str t2 ], t           (* 输出：t2 = t0 + t1 *)
  | BinOp (op, e1, e2) when List.mem op [ Eq; Neq; Lt; Le; Gt; Ge ] ->
    let c1, t1 = gen_expr env e1 in
    let c2, t2 = gen_expr env e2 in
    let t = fresh_tmp () in
    let l_true = fresh_lbl "L_true_" in                 (*eg:L_true_0*)
    let l_end = fresh_lbl "L_end_" in                   (*eg:L_end_0*)
    let br =
      match op with
      | Eq  -> "beq" | Neq -> "bne" | Lt  -> "blt"
      | Le  -> "ble" | Gt  -> "bgt" | Ge  -> "bge" | _ -> assert false
    in
    ( c1 @ c2
      @ [ Printf.sprintf "%s = 0" t                           (* 初始化结果t2为0 *)
        ; Printf.sprintf "%s %s %s %s" br t1 t2 l_true      (* 如果条件成立，跳转到 l_true *)
        ; Printf.sprintf "j %s" l_end                         (* 否则跳转到 l_end *)
        ; l_true ^ ":"                                        (*输入：x==1*)
        ; Printf.sprintf "%s = 1" t                           (*输出: t0 = x  t1 = 1  t2 = 0*)
        ; l_end ^ ":"                                         (*beq t0, t1, L_true_0  j L_end_0  *)
        ]                                                     (*L_true_0:t2 = 1       L_end_0:*)
    , t )
  | BinOp (op, e1, e2) when op = And || op = Or ->
    let c1, t1 = gen_expr env e1 in
    let c2, t2 = gen_expr env e2 in
    let t_res = fresh_tmp () in
    let l_short = fresh_lbl "L_short_" in
    let l_end = fresh_lbl "L_end_" in
    let set_short, set_normal, cond1, cond2 =  (*布尔逻辑归一化,简化布尔逻辑的处理*)
      if op = And then                         (*实现了对 && 和 ||的短路逻辑优化*)
        ( "0"
        , "1"                                                 
        , Printf.sprintf "beqz %s %s" t1 l_short            
        , Printf.sprintf "beqz %s %s" t2 l_short )          
      else                                                   
        ( "1"                                                
        , "0"                                                 
        , Printf.sprintf "bnez %s %s" t1 l_short            
        , Printf.sprintf "bnez %s %s" t2 l_short )          
   in
  ( c1
    @ [ cond1 ]
    @ c2
    @ [ cond2
      ; Printf.sprintf "%s = %s" t_res set_normal
      ; Printf.sprintf "j %s" l_end
      ; l_short ^ ":"
      ; Printf.sprintf "%s = %s" t_res set_short
      ; l_end ^ ":"
      ]
  , t_res )
  | UnOp (Not, e1) -> 
    let c, t1 = gen_expr env e1 in                
    let t = fresh_tmp () in
    let l_true = fresh_lbl "L_true_" in    
    let l_end = fresh_lbl "L_end_" in
    ( c
      @ [ Printf.sprintf "%s = 0" t
        ; Printf.sprintf "beqz %s %s" t1 l_true
        ; Printf.sprintf "j %s" l_end
        ; l_true ^ ":"
        ; Printf.sprintf "%s = 1" t
        ; l_end ^ ":"
        ]
    , t )
  | UnOp (Neg, e1) ->
    let c, t1 = gen_expr env e1 in
    let t = fresh_tmp () in                           (*输入：-x*)
    c @ [ Printf.sprintf "%s = - %s" t t1 ], t        (*输出：t0 = - x*)
  | Call (fname, args) ->
    (match FuncEnv.find_opt fname env.funcs with
     | None -> raise (SemanticError ("unknown function " ^ fname))
     | Some sig_ ->
       if List.length args <> List.length sig_.params
       then raise (SemanticError ("arity mismatch in call " ^ fname));
       let arg_codes, arg_tmps = List.split (List.map (gen_expr env) args) in
       let t_ret = fresh_tmp () in
       ( List.flatten arg_codes
         @ List.map (fun t -> "param " ^ t) arg_tmps
         @ [ Printf.sprintf "call %s %d" fname (List.length arg_tmps);
            Printf.sprintf "%s = retval" t_ret
           ]
       , t_ret))
  | _ -> raise (SemanticError "unsupported expr")
;;

(* ============================================================= *)
(*  4.语句生成 Stmt → code list                                   *)
(* ============================================================= *)
(*将stmt转化为中间表示IR*)

let rec gen_stmt env ?break_lbl ?cont_lbl (s : stmt) : string list =
  match s with
  | Block lst -> List.flatten (List.map (gen_stmt env ?break_lbl ?cont_lbl) lst)
  (*| VarDecl (id, init_e) ->
    if VarEnv.mem id env.vars then raise (SemanticError ("redefinition of " ^ id));
    let c, t = gen_expr env init_e in
    c @ [ Printf.sprintf "%s = %s" id t ]*)  (*sementic.ml里已经检查过了*)
  | VarDecl (id, init_e) ->
    let c, t = gen_expr env init_e in
    c @ [ Printf.sprintf "%s = %s" id t ]  
  | Expr e -> fst (gen_expr env e)
  | Return None -> [ "ret 0" ]
  | Return (Some e) ->
    let c, t = gen_expr env e in
    c @ [ Printf.sprintf "ret %s" t ]
  | If (cond, s_then, s_else_opt) ->
    let c_cond, t_c = gen_expr env cond in
    let l_else = fresh_lbl "L_else_" in
    let l_end = fresh_lbl "L_end_" in
    let then_c = gen_stmt env ?break_lbl ?cont_lbl s_then in
    let else_c = match s_else_opt with Some s -> gen_stmt env ?break_lbl ?cont_lbl s | None -> [] in
    c_cond
    @ [ Printf.sprintf "beqz %s %s" t_c l_else ]
    @ then_c
    @ [ Printf.sprintf "j %s" l_end; l_else ^ ":" ]
    @ else_c
    @ [ l_end ^ ":" ]
  | While (cond, body) ->
    let l_begin = fresh_lbl "L_begin_" in
    let l_end = fresh_lbl "L_end_" in
    let c_cond, t_c = gen_expr env cond in
    let body_c = gen_stmt env ~break_lbl:l_end ~cont_lbl:l_begin body in
    [ l_begin ^ ":" ]
    @ c_cond
    @ [ Printf.sprintf "beqz %s %s" t_c l_end ]
    @ body_c
    @ [ Printf.sprintf "j %s" l_begin; l_end ^ ":" ]
  | Break -> (match break_lbl with Some l -> [ Printf.sprintf "j %s" l ] | None -> raise (SemanticError "break not in loop"))
  | Continue -> (match cont_lbl with Some l -> [ Printf.sprintf "j %s" l ] | None -> raise (SemanticError "continue not in loop"))
  (*| _ -> []*)
;;

(* ============================================================= *)
(* 5.函数和程序生成  Function / Program                           *)
(* ============================================================= *)
(*将函数和整个程序转换为中间表示IR*)

(*
1.获取函数的局部变量环境。
2.构建代码生成环境。
3.遍历函数体中的每个语句，生成对应的 IR。*)
let gen_func (ana : analysis_result) (f : func_def) : string list =
  let locals =
    try List.assoc f.fname ana.local_vars
    with Not_found -> raise (Failure ("internal: no locals for " ^ f.fname))
  in
  let env = make_env ~globals:ana.global_funcs ~locals in
  (f.fname ^ ":") :: List.flatten (List.map (gen_stmt env) f.body)
;;

(*
1.调用语义分析模块生成符号表。
2.遍历每个函数，调用 gen_func 生成 IR。*)
let gen_program (p : program) : string list =
  let ana = Semantic.analyze_program p in
  List.flatten (List.map (gen_func ana) p)
;;

(* ------------------------------------------------------------------ *)
(* 6.寄存器分配  Graph Coloring Register Allocation with Spill         *)
(* ------------------------------------------------------------------ *)
(*使用图着色算法分配寄存器*)
(* 1.分析每条指令的活跃变量
   2.构建干扰图
   3.简化干扰图
   4.分配寄存器
*)
module SS = Set.Make(String)
module SM = Map.Make(String)

(*分析活跃变量*)
let analyze_liveness ir =
  List.fold_left (fun s instr ->
    List.fold_left (fun s tok -> if tok <> "=" then SS.add tok s else s) s (String.split_on_char ' ' instr)
  ) SS.empty ir

(*构建干扰图*)
let build_interference temps =  
  let g = Hashtbl.create (List.length temps) in
  List.iter (fun v -> Hashtbl.add g v SS.empty) temps;
  List.iter (fun v1 ->
    List.iter (fun v2 -> if v1 <> v2 then
      let s1 = Hashtbl.find g v1 in Hashtbl.replace g v1 (SS.add v2 s1)
    ) temps
  ) temps;
  g

(*简化干扰图*)
let simplify g phys stack =
  let tbl = Hashtbl.copy g in
  try while Hashtbl.length tbl > 0 do
    let v = Hashtbl.fold (fun v adj acc -> if SS.cardinal adj < List.length phys then v else acc) tbl "" in
    Hashtbl.remove tbl v;
    Stack.push v stack;
    Hashtbl.iter (fun key adj -> Hashtbl.replace tbl key (SS.remove v adj)) tbl
  done with _ -> ()

(*分配寄存器*)
let select stack g phys =
  let mapping = Hashtbl.create 16 in
  while not (Stack.is_empty stack) do
    let v = Stack.pop stack in
    let adj = Hashtbl.find g v in
    let used = SS.fold (fun u acc -> match Hashtbl.find_opt mapping u with Some r->r::acc|None->acc) adj [] in
    match List.find_opt (fun r -> not (List.mem r used)) phys with
    | Some reg -> Hashtbl.add mapping v reg
    | None -> failwith ("Spill needed for variable " ^ v)
  done;
  mapping

(* 自定义实现字符串全数字检查 *)
let is_all_digits s =
  let rec check i =
    if i >= String.length s then true
    else (s.[i] >= '0' && s.[i] <= '9') && check (i + 1)
  in
  check 0
(*已 debug：IR转汇编代码不能return的错误*)
(*let allocate_registers ir =
  let phys = ["t0";"t1";"t2";"t3";"t4";"t5"] in
  let temps = SS.elements (analyze_liveness ir) in
  let ig = build_interference temps in
  let stack = Stack.create () in
  simplify ig phys stack;
  let mapping = select stack ig phys in
  List.map (fun instr ->
    instr
    |> String.split_on_char ' '
    |> List.map (fun tok -> try Hashtbl.find mapping tok with Not_found -> tok)
    |> String.concat " ")
    ir
*)

(*已 优化debug:当需要分配的临时变量数量超过可用物理寄存器数量时，
List.nth phys !reg_counter 会抛出 Failure("nth") 异常---
debug2：在字符串处理时去除逗号*)
let allocate_registers ir =  
  let phys = ["t0";"t1";"t2";"t3";"t4";"t5"] in  
  let mapping = Hashtbl.create 16 in  
  let reg_counter = ref 0 in  
    
  List.map (fun instr ->  
    let tokens = String.split_on_char ' ' instr in  
    let replaced_tokens = List.map (fun tok ->  
      (* 去除逗号后处理 *)  
      let clean_tok = String.map (function ',' -> ' ' | c -> c) tok |> String.trim in  
      let suffix = if String.contains tok ',' then "," else "" in  
      if String.length clean_tok > 0 && clean_tok.[0] = 't' &&   
         is_all_digits (String.sub clean_tok 1 (String.length clean_tok - 1)) then  
        try (Hashtbl.find mapping clean_tok) ^ suffix  
        with Not_found ->  
          let reg_index = !reg_counter mod (List.length phys) in  
          let reg = List.nth phys reg_index in  
          incr reg_counter;  
          Hashtbl.add mapping clean_tok reg;  
          reg ^ suffix  
      else tok  
    ) tokens in  
    String.concat " " replaced_tokens  
  ) ir
(* ------------------------------------------------------------------ *)
(* 7.汇编代码生成  Assembly Generation                                 *)
(* ------------------------------------------------------------------ *)
(*将中间表示（IR）转换为 RISC-V32 汇编代码*)
(*步骤：
1. 将 IR 指令转换为汇编指令。
2. 生成函数的 prologue（建立栈帧） 和 epilogue(回收栈帧)。
*)


(*已 debug:待补充在模式匹配里对条件分支指令的处理*)
let assemble_instr instr =
  let ws = String.split_on_char ' ' instr in
  match ws with
  | [dst;"=";src] when is_all_digits src ->  (* 替换String.for_all *)
      (* 常量赋值，例如 t0 = 1 *)
      Printf.sprintf "  li %s, %s" dst src
  | [dst;"=";src] ->
      (* 简单赋值，例如 t1 = t0 *)
      Printf.sprintf "  mv %s, %s" dst src
  | [dst;"=";s1;"+";s2] -> Printf.sprintf "  add %s, %s, %s" dst s1 s2
  | [dst;"=";s1;"-";s2] -> Printf.sprintf "  sub %s, %s, %s" dst s1 s2
  | [dst;"=";s1;"*";s2] -> Printf.sprintf "  mul %s, %s, %s" dst s1 s2
  | [dst;"=";s1;"/";s2] -> Printf.sprintf "  div %s, %s, %s" dst s1 s2
  | ["param";r] -> Printf.sprintf "  mv a0, %s" r
  | ["call";f;_] -> Printf.sprintf "  call %s" f
  | ["ret";r] -> Printf.sprintf "  mv a0, %s\n  ret" r
  | [lbl] when String.get lbl (String.length lbl - 1) = ':' -> lbl
  | ["j";lbl] -> Printf.sprintf "  j %s" lbl
  | ["blt"; r1; r2; lbl] -> Printf.sprintf "  blt %s, %s, %s" r1 r2 lbl  
  | ["ble"; r1; r2; lbl] -> Printf.sprintf "  ble %s, %s, %s" r1 r2 lbl  
  | ["bgt"; r1; r2; lbl] -> Printf.sprintf "  bgt %s, %s, %s" r1 r2 lbl  
  | ["bge"; r1; r2; lbl] -> Printf.sprintf "  bge %s, %s, %s" r1 r2 lbl  
  | ["beq"; r1; r2; lbl] -> Printf.sprintf "  beq %s, %s, %s" r1 r2 lbl  
  | ["bne"; r1; r2; lbl] -> Printf.sprintf "  bne %s, %s, %s" r1 r2 lbl  
  | ["beqz"; r; lbl] -> Printf.sprintf "  beqz %s, %s" r lbl  
  | ["bnez"; r; lbl] -> Printf.sprintf "  bnez %s, %s" r lbl  
  (*| _ -> failwith ("unhandled: " ^ instr)*)
  | _ ->
      Printf.eprintf "Unhandled instruction: %s\n" instr;
      failwith ("unhandled: " ^ instr)

let gen_prologue () = [".text"; ".globl main"; "main:"]
let gen_epilogue () = ["  ret"]

(*let gen_assembly ir =
  let colored = allocate_registers ir in
  gen_prologue () @ List.map assemble_instr colored @ gen_epilogue ()
*)
(*已 debug:IR转汇编代码出现指令重复，修改算法：*)
let gen_assembly ir =  
  let colored = allocate_registers ir in  
  let filtered_ir = List.filter (fun instr ->   
    not (String.contains instr ':' && String.get instr (String.length instr - 1) = ':')  
  ) colored in  
  gen_prologue () @ List.map assemble_instr filtered_ir

(* ------------------------------------------------------------------ *)
(* 8.公共接口 Public Interface                                         *)
(* ------------------------------------------------------------------ *)
(*从源代码生成汇编代码*)
(*
1.调用词法分析器和语法分析器生成 AST。
2.调用代码生成模块生成 IR 和汇编代码。
*)
let compile_source src =
  let lexbuf = Lexing.from_string src in
  let ast = Parser.program Lexer.token lexbuf in
  let ir = gen_program ast in
  gen_assembly ir
