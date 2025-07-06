(* test/lexer_test.ml *)
open OUnit2
open Toyc_compiler_lib

(* ========================================================================== *)
(*                             辅助函数                                       *)
(* ========================================================================== *)

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

(* 2. 将 Token 转换为可读的字符串，这是 Lexer 测试的核心！ *)
let string_of_token = function
  | Parser.IF -> "IF"
  | Parser.ELSE -> "ELSE"
  | Parser.WHILE -> "WHILE"
  | Parser.BREAK -> "BREAK"
  | Parser.CONTINUE -> "CONTINUE"
  | Parser.RETURN -> "RETURN"
  | Parser.INT -> "INT"
  | Parser.VOID -> "VOID"
  | Parser.NUMBER n -> Printf.sprintf "NUMBER(%d)" n
  | Parser.ID s -> Printf.sprintf "ID(%s)" s
  | Parser.PLUS -> "PLUS"
  | Parser.MINUS -> "MINUS"
  | Parser.TIMES -> "TIMES"
  | Parser.DIVIDE -> "DIVIDE"
  | Parser.MOD -> "MOD"
  | Parser.ASSIGN -> "ASSIGN"
  | Parser.EQ -> "EQ"
  | Parser.NEQ -> "NEQ"
  | Parser.LT -> "LT"
  | Parser.LE -> "LE"
  | Parser.GT -> "GT"
  | Parser.GE -> "GE"
  | Parser.AND -> "AND"
  | Parser.OR -> "OR"
  | Parser.NOT -> "NOT"
  | Parser.LPAREN -> "LPAREN"
  | Parser.RPAREN -> "RPAREN"
  | Parser.LBRACE -> "LBRACE"
  | Parser.RBRACE -> "RBRACE"
  | Parser.SEMICOLON -> "SEMICOLON"
  | Parser.COMMA -> "COMMA"
  | Parser.EOF -> "EOF"
  | Parser.Menhir_error -> "MENHIR_ERROR"
;;

(* 3. 将源代码字符串完整地转换为一个 token 字符串 *)
let token_string_from_source code =
  try
    let lexbuf = Lexing.from_string code in
    let tokens = ref [] in
    let rec loop () =
      let token = Lexer.token lexbuf in
      tokens := token :: !tokens;
      (* 持续循环直到遇到 EOF *)
      if token <> Parser.EOF then loop ()
    in
    loop ();
    (* 将 token 列表反转（因为我们是头插法），然后转换为字符串 *)
    List.rev !tokens
    |> List.map string_of_token
    |> String.concat "\n" (* 每个 token 占一行，更清晰 *)
  with
  | Lexer.Error msg -> "Lexer Error: " ^ msg
;;

(* ========================================================================== *)
(*                     自动化测试用例生成 (与 parser_test 类似)                 *)
(* ========================================================================== *)

let create_test_case (test_file_path : string) =
  let test_name = Filename.remove_extension (Filename.basename test_file_path) in
  let expected_file_path = Filename.chop_extension test_file_path ^ ".expected" in
  let test_fun _ =
    if not (Sys.file_exists expected_file_path)
    then skip_if true (Printf.sprintf "Expected file not found: %s" expected_file_path);
    let source_code = read_file test_file_path in
    let expected_output = read_file expected_file_path in
    let actual_output = token_string_from_source source_code in
    (* 注意：这里我们不再需要 normalize_string，因为我们的输出格式是固定的 (每行一个 token) *)
    assert_equal
      expected_output
      actual_output
      ~msg:(Printf.sprintf "Test failed for: %s" test_name)
      ~printer:(fun s -> "\n" ^ s)
  in
  test_name >:: test_fun
;;

(* ========================================================================== *)
(*          suite 构建逻辑 (访问 tests/lexer/)                                *)
(* ========================================================================== *)
let suite =
  "Lexer Test Suite (from files)"
  >:::
  let tests_dir = "../../../tests/lexer" in
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
        "\n[TEST WARNING] Lexer test directory not found at '%s'. Skipping lexer tests.\n"
        tests_dir;
      [])
  with
  | Sys_error msg ->
    Printf.eprintf
      "\n[FATAL TEST ERROR] Could not read lexer test directory '%s': %s\n"
      tests_dir
      msg;
    []
;;

(* ========================================================================== *)
(*                             运行测试                                       *)
(* ========================================================================== *)
let () = run_test_tt_main suite
