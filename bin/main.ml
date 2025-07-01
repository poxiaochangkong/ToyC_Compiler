let () = print_endline "ToyC Compiler Starting ..."
(*   try
    let lexbuf = Lexing.from_channel stdin in
    let ast = Parser.program Lexer.token lexbuf in
    print_endline "Parsing successful ";
    print_string (Ast.string_of_program ast);
    let checked_ast = Semantic.check_program ast in
    let generrated_code = Codegen.generate_code checked_ast in
    print_endline "\n Generated Code  -";
    print_endline generated_code;
    print_ednline "ToyC Compiler Finished Successfully."
  with
  | Lexer.Error msg -> Printf.eprintf "Lexer error: %s\n" msg
  | Parser.Error ->
    let pos = Lexing.lexeme_start_p (Lexing.from_channel stdin) in
    Printf.eprintf
      "Parser error at line %d, column %d\n"
      pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol + 1) *)
