open Core
open Ast
open Err
open Checker

(* copied some boilerplate from Real World OCaml textbook for file IO *)
(* http://dev.realworldocaml.org/parsing-with-ocamllex-and-menhir.html#defining-a-lexer *)

let fname = "src/test.app"
let parse_with_error lexbuf =
  try (Printf.printf "%s\n" (show_stat_silent (check_stat (Parser.main Lexer.read lexbuf)))) with
  | LexerError msg | TypeError msg -> Printf.printf "%s" msg 

let () = 
  (* get buffer for source file *)
  let inb = In_channel.create fname in
  let lexbuf = Lexing.from_channel inb  in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fname };

  (* parse and lex *)
  parse_with_error lexbuf;
  In_channel.close inb;


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