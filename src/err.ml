open Format
open Lexing
open Ast

(* Exceptions across different stages of compilation *)
exception LexerError of string
exception TypeError of string
exception ScopeError of string
exception ReturnError of string

(* general-purpose error format *)
let gen_location_format l1 c1 l2 c2 = 
  if l1 == l2 then 
    if c1 == c2 then (sprintf "line %d, col %d" l1 c1) else (sprintf "line %d, col %d-%d" l1 c1 c2)
  else
    sprintf "(line %d, col %d) to (line %d, col %d)" l1 c1 l2 c2

let gen_err_format spos epos err_type msg =
  let fname = spos.pos_fname in
  let l1 = spos.pos_lnum in
  let l2 = epos.pos_lnum in
  let c1 = spos.pos_cnum - spos.pos_bol in
  let c2 = epos.pos_cnum - epos.pos_bol in
  sprintf "@[File \"%s\", %s: %s error:@ %s@]@." 
    fname (gen_location_format l1 c1 l2 c2) err_type msg

(* Lexer error *)
let lexer_error pos msg = raise (LexerError (gen_err_format pos pos "Lexer" msg))

(* static type errors *)
let checker_unop_error spos epos op t = 
  let msg = sprintf "'%s' operator does not take a %s type argument" 
    (show_pretty_op_un op) (show_pretty_tp t)
  in
    raise (TypeError (gen_err_format spos epos "Type" msg))

let checker_binop_error spos epos op t1 t2 = 
  let msg = sprintf "'%s' operator does not take %s and %s type arguments" 
    (show_pretty_op_bin op) (show_pretty_tp t1) (show_pretty_tp t2)
  in
    raise (TypeError (gen_err_format spos epos "Type" msg))

let var_not_declared_error spos epos name =
  raise (ScopeError (gen_err_format spos epos "Scope" (sprintf "variable %s not declared" name)))

let var_mult_declared_error spos epos name = 
  raise (ScopeError (gen_err_format spos epos "Scope" (sprintf "variable %s already declared in this scope" name)))

let inconsistent_return_error spos epos typ_exp typ_ret = 
  let msg = sprintf "statement has return of type %s but function return type is %s" 
    (show_pretty_tp typ_ret) (show_pretty_tp typ_exp)
  in 
  raise (ReturnError (gen_err_format spos epos "Return" msg))

let incomplete_return_error spos epos typ_ret = 
  let msg = sprintf "function has return type %s but not all code paths return" (show_pretty_tp typ_ret) in
  raise (ReturnError (gen_err_format spos epos "Return" msg))
