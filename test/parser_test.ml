(* open OUnit2
open Toyc_compiler_lib
open Ast

(* let ast_to_string (code : string) : string =
  let lexbuf = Lexing.from_string code in
  let ast = Parser.program Lexer.token lexbuf in
  Ast.string_of_program ast
;; *)

(*打开文件读取内容到一个字符串*)
let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s
;;

(*code -> ast*)
let ast_string_from_source code =
  let lexbuf = Lexing.from_string code in
  let ast = Parser.program Lexer.token lexbuf in
  string_of_program ast
;;

(*
   * 规范化字符串：
 * 1. 使用正则表达式将所有空白字符序列 (\s+) 替换为单个空格。
 * 2. 去除字符串开头和结尾的空格。
*)
let normalize_string s =
  let open Str in
  let s = global_replace (regexp "[ \t\n\r]+") " " s in
  Base.String.strip s
;;

(* let data_dir = Filename.concat (Sys.getcwd ()) "data" *)

(*通过文件名自动创建测试 测试ast的生成是否正确*)
let create_test_case (test_file_path : string) =
  let test_name = Filename.remove_extension (Filename.basename test_file_path) in
  let expected_file_path = Filename.chop_extension test_file_path ^ ".expected" in
  (* OUnit2 的测试函数 *)
  let test_fun _ =
    (* OUnit2 需要这个 context 参数 *)
    (* 读取源文件和期望的输出文件 *)
    let source_code = read_file test_file_path in
    let expected_output_raw = read_file expected_file_path in
    (* 运行我们的编译器逻辑，得到实际的输出 *)
    let actual_output_raw = ast_string_from_source source_code in
    (*在断言之前进行规范化*)
    let expected_output = normalize_string expected_output_raw in
    let actual_output = normalize_string actual_output_raw in
    (* 断言实际输出与期望输出是否一致 *)
    assert_equal
      expected_output
      actual_output
      ~msg:(Printf.sprintf "Test failed for %s" test_name)
      ~printer:(fun s -> "\n" ^ s)
    (* 在失败时打印出更易读的格式 *)
  in
  test_name >:: test_fun
;;

(* let suite =
  "File-based AST Test Suite"
  >::: ((* a. 读取 tests/ 目录下的所有文件 *)
        Sys.readdir "."
        (* b. 转换成完整路径 *)
        |> Array.to_list
        |> List.map (fun fname -> Filename.concat data_dir fname)
        (* c. 只保留以 .tc 结尾的文件 *)
        |> List.filter (fun f -> Filename.check_suffix f ".tc")
        (* d. 为每个 .tc 文件创建一个测试用例 *)
        |> List.map create_test_case)
;; *)
let suite =
  "File-based AST Test Suite"
  >:::
  let tests_dir = "../../../tests" in
  try
    Sys.readdir tests_dir
    |> Array.to_list
    (* 使用 Stdlib.Filename.concat 来拼接路径 *)
    |> List.map (fun fname -> Stdlib.Filename.concat tests_dir fname)
    (* 使用 Stdlib.Filename.check_suffix 来筛选文件 *)
    |> List.filter (fun f -> Stdlib.Filename.check_suffix f ".tc")
    |> List.map create_test_case
  with
  | Sys_error msg ->
    Printf.eprintf
      "\n[FATAL TEST ERROR] Could not read test directory '%s': %s\n\n"
      tests_dir
      msg;
    []
  | exn ->
    Printf.eprintf
      "\n[FATAL TEST ERROR] An unexpected error occurred: %s\n\n"
      (Printexc.to_string exn);
    []
;;

let () = run_test_tt_main suite *)
(*以上是先前不完善的项目测试结构*)
(* test/parser_test.ml *)
open OUnit2
open Toyc_compiler_lib

(* 1. 使用标准库的函数来读取文件 *)
let read_file filename =
  let ch = open_in filename in
  try
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s
  with
  | e ->
    close_in_noerr ch;
    raise e
;;

(* 2. 将源代码解析为 AST，然后转换为字符串，并捕获错误 *)
let ast_string_from_source code =
  let lexbuf = Lexing.from_string code in
  try
    let ast = Parser.program Lexer.token lexbuf in
    Ast.string_of_program ast
  with
  | Lexer.Error msg -> "Lexer Error: " ^ msg
  | Parser.Error ->
    let pos = lexbuf.lex_curr_p in
    (* 2. 从位置记录中提取出行号和列号 *)
    let line = pos.pos_lnum in
    let col = pos.pos_cnum - pos.pos_bol + 1 in
    (* 3. 构造一个详细的错误信息字符串 *)
    Printf.sprintf "Parser Error at Line %d, Column %d" line col
  | Failure msg -> "Parser Failure" ^ msg (*这里是我们自定义的错误*)
;;

(* 3. 规范化字符串 (需要 str 和 base 库) *)
let normalize_string s =
  let open Str in
  let s_no_internal_space = global_replace (regexp "[ \t\n\r]+") " " s in
  Base.String.strip s_no_internal_space
;;

(* ========================================================================== *)
(*                     自动化测试用例生成                                     *)
(* ========================================================================== *)

let create_test_case (test_file_path : string) =
  let test_name = Filename.remove_extension (Filename.basename test_file_path) in
  let expected_file_path = Filename.chop_extension test_file_path ^ ".expected" in
  let test_fun _ =
    if not (Sys.file_exists expected_file_path)
    then skip_if true (Printf.sprintf "Expected file not found: %s" expected_file_path);
    let source_code = read_file test_file_path in
    (* === 添加调试打印 === *)
    (*   Printf.printf "\n--- Testing file: %s ---\n" test_file_path;
    Printf.printf "Content read from file (length %d):\n" (String.length source_code);
    Printf.printf "[%s]\n" source_code;
    Printf.printf "--------------------------\n"; *)
    let expected_output_raw = read_file expected_file_path in
    let actual_output_raw = ast_string_from_source source_code in
    let expected_output = normalize_string expected_output_raw in
    let actual_output = normalize_string actual_output_raw in
    assert_equal
      expected_output
      actual_output
      ~msg:(Printf.sprintf "Test failed for: %s" test_name)
      ~printer:(fun s -> "\n" ^ s)
  in
  test_name >:: test_fun
;;

(* ========================================================================== *)
(*          suite 构建逻辑 (使用 ../../../ 访问 tests/parser/)             *)
(* ========================================================================== *)
let suite =
  "Parser Test Suite (from files)"
  >:::
  (*
     * 目标目录是相对于项目根目录的 tests/parser/
     * 我们的测试程序在 _build/default/test/ 中运行
     * 所以相对路径是 ../../../tests/parser
  *)
  let tests_dir = "../../../tests/parser" in
  try
    if Sys.file_exists tests_dir && Sys.is_directory tests_dir
    then
      Sys.readdir tests_dir
      |> Array.to_list
      |> List.map (fun fname -> Filename.concat tests_dir fname)
      |> List.filter (fun f -> Filename.check_suffix f ".tc")
      |> List.map create_test_case
    else (
      Printf.eprintf
        "\n\
         [TEST WARNING] Parser test directory not found at '%s'. Skipping parser tests.\n"
        tests_dir;
      [])
  with
  | Sys_error msg ->
    Printf.eprintf
      "\n[FATAL TEST ERROR] Could not read parser test directory '%s': %s\n"
      tests_dir
      msg;
    []
;;

(* ========================================================================== *)
(*                             运行测试                                       *)
(* ========================================================================== *)
let () = run_test_tt_main suite
