(* (* test/test_toyc_compiler.ml *)

open OUnit2
open Toyc_compiler_lib

(* --- 辅助函数 --- *)

(* 解析一个文件，如果成功则返回 AST，如果失败则抛出异常 *)
let parse_file filename =
  let in_channel = open_in filename in
  let lexbuf = Lexing.from_channel in_channel in
  try
    let ast = Parser.comp_unit Lexer.main lexbuf in
    close_in in_channel;
    ast
  with
  | Lexer.Error msg ->
    close_in_noerr in_channel;
    failwith (Printf.sprintf "Lexer Error in %s: %s" filename msg)
  | Parser.Error ->
    close_in_noerr in_channel;
    let pos = Lexing.lexeme_start_p lexbuf in
    failwith
      (Printf.sprintf
         "Parser Error in %s at line %d, column %d"
         filename
         pos.pos_lnum
         (pos.pos_cnum - pos.pos_bol + 1))
;;

(* --- 测试生成器 --- *)

(* 为 "passing" 目录下的每个文件生成一个测试用例 *)
(* 这个测试的期望是：解析过程不应抛出任何异常 *)
let make_passing_test filename =
  filename
  >:: fun _ ->
  (* 如果 parse_file 抛出异常，这个测试就会自动失败 *)
  ignore (parse_file filename)
;;

(* 为 "failing" 目录下的每个文件生成一个测试用例 *)
(* 这个测试的期望是：解析过程必须抛出一个 Failure 异常 *)
let make_failing_test filename =
  filename
  >:: fun _ ->
  assert_raises (Failure "Parser or Lexer Error") (fun () -> ignore (parse_file filename))
;;

(* 我们使用 assert_raises 来捕获预期的失败。
     注意：为了简化，我们让 parse_file 统一抛出 Failure 异常。
     因此这里捕获 Failure。如果 parse_file 没有抛出任何异常，测试也会失败。*)

(* --- 主逻辑 --- *)

let () =
  (* 定义测试用例存放的目录 *)
  let passing_dir = "test/cases/passing" in
  let failing_dir = "test/cases/failing" in
  (* 读取目录内容，并为每个 .tc 文件生成测试 *)
  let passing_tests =
    Sys.readdir passing_dir
    |> Array.to_list
    |> List.filter (fun name -> Filename.check_suffix name ".tc")
    |> List.map (fun name -> make_passing_test (Filename.concat passing_dir name))
  in
  let failing_tests =
    Sys.readdir failing_dir
    |> Array.to_list
    |> List.filter (fun name -> Filename.check_suffix name ".tc")
    |> List.map (fun name -> make_failing_test (Filename.concat failing_dir name))
  in
  (* 将所有生成的测试组织成一个测试套件 *)
  let suite =
    "ToyC File-based Tests"
    >::: [ "Passing Cases" >::: passing_tests; "Failing Cases" >::: failing_tests ]
  in
  (* 运行测试套件 *)
  run_test_tt_main suite
;;
*)
