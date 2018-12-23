open Token
open Lexer

let () = print_token_list (lex_help "int x = 5 string y = \"abcdef\"" 0 0);;