open Ast

let () = Printf.printf "AST: %s\n" (show_expr (Parser.main Lexer.read (Lexing.from_string "3+5*5")));;