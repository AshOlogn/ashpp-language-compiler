
let src = "y = 1 | 2 & 3 ^ 4"

open Ast
let show_ast str = (show_stat_silent (Parser.main Lexer.read (Lexing.from_string str)))
let () = Printf.printf "AST for %s:\n%s\n" src (show_ast src)

(* 
open Token
let show_lexed str = Lexer.read (Lexing.from_string str)
let print_lexed str =
  Printf.printf "src: %s\n" str;
  let buffer = Lexing.from_string str in
  let d = ref false in
  let s = ref "" in
  while not !d do
    s := show_token (Lexer.read buffer);
    Printf.printf "token: %s\n" !s;
    d := "Token.END" = !s;
  done 

let () = print_lexed src;
*)
