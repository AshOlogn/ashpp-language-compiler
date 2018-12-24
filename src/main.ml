open Token
open Lexer

let () = print_token_list (lex_help "/* /* hello, world */ */ x = 5" 0 0);;