open Token
open Lexer

let () = print_token_list (lex_help "x=5.6@" 0 0);;