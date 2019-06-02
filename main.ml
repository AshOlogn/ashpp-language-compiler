open Core
open Src
open Ast
open Err
open Checker
open Symtable
open Opt_const_fold
open Three_address
open Gen_three_address

(* copied some boilerplate from Real World OCaml textbook for file IO *)
(* http://dev.realworldocaml.org/parsing-with-ocamllex-and-menhir.html#defining-a-lexer *)

let fname = "src/test.app"
let parse_with_error lexbuf =
  try
  let (stat, _) = check_stat ((Parser.main Lexer.read lexbuf), symtable_init) in
  (* let (stat', _) = (const_fold_stat (stat, symtable_init)) in *)
  let stat'' = prune_empty stat in
  let (three_ins, _, _, _) = gen_three_address_stat stat'' 0 0 symtable_init in
  (Printf.printf "%s\n" (show_stat stat''));
  (Printf.printf "%s" (show_three_instructions three_ins)) with
  | LexerError msg 
  | TypeError msg 
  | ScopeError msg 
  | ReturnError msg -> Printf.printf "%s" msg

let () = 
  (* get buffer for source file *)
  let inb = In_channel.create fname in
  let lexbuf = Lexing.from_channel inb  in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fname };

  (* lex, parse, type-check *)
  parse_with_error lexbuf;
  In_channel.close inb;