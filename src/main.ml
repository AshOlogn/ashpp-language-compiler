open Core
open Ast
open Err
open Checker
open Symtable
open Opt_const_fold

(* copied some boilerplate from Real World OCaml textbook for file IO *)
(* http://dev.realworldocaml.org/parsing-with-ocamllex-and-menhir.html#defining-a-lexer *)

let fname = "src/test.app"
let parse_with_error lexbuf =
  try
  let (stat, _) = check_stat ((Parser.main Lexer.read lexbuf), symtable_init) in
  let (stat', _) = const_fold_stat (stat, symtable_init) in
  (Printf.printf "%s\n" (show_stat_silent stat')) with
  | LexerError msg -> Printf.printf "%s" msg 
  | TypeError msg -> Printf.printf "%s" msg 
  | ScopeError msg -> Printf.printf "%s" msg 

let () = 
  (* get buffer for source file *)
  let inb = In_channel.create fname in
  let lexbuf = Lexing.from_channel inb  in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fname };

  (* lex, parse, type-check *)
  parse_with_error lexbuf;
  In_channel.close inb;