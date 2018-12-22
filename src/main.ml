open Stdio
open Token

let () = printf "show_token_type: %s\nshow_token: %s\n" 
                    (show_token_type (LEFT_PAREN)) (show_token {ttype = LEFT_BRACE; startLine = 5; endLine = 10;})