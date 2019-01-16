open Lexing
open Printf

(* Some type naming terminology and inspiration for node type from Menhir demo: 
https://gitlab.inria.fr/fpottier/menhir/tree/master/demos/calc-ast-dune *)

(* Type data types, support recursive types like int[][] *)
type t_prim = TInt | TChar | TFloat | TString | TBool | TVoid
[@@deriving show]

(* Dummy type used in expressions when parsing with Menhir to defer type-checking to later code *)
(* Invalid used in type-checker to signify type error *)
type tp =
  | TPrim of t_prim
  | TArray of tp
  | TNtuple of tp list
  | TFun of tp list * tp
  | TClass of string
  | TDummy
  | TInvalid
[@@deriving show]

(* Operation types *)
type op_bin = 
  | OIden 
  | OExp 
  | OMult | ODiv | OMod
  | OAdd | OSub 
  | OBitl | OBitr
  | OLt | OGt | OLeq | OGeq
  | OEq | ONeq
  | OBitAnd
  | OBitXor
  | OBitOr
  | OLogAnd
  | OLogOr
  (* these last 3 work for bitwise or logic to handle assignment *)
  | OGenAnd
  | OGenOr
  | OGenXor
[@@deriving show]

type op_un = ONeg | OPos | OBitNot | OLogNot
[@@deriving show]

(* Node used to build abstract syntax tree, with location metadata and type information *)
(* The Menhir-based parser sets type as "dummy" to defer type-checking *)
(* For statement nodes, type is always dummy since statements don't return values *)
type 'a node = { value: 'a; typ: tp; st_loc: Lexing.position; en_loc: Lexing.position;} 

(* Expression node, wrapped and unwrapped *)
type expr = raw_expr node 
and raw_expr = 
  | ELitInt of int
  | ELitFloat of float
  | ELitChar of char
  | ELitString of string
  | ELitBool of bool
  | EUnary of op_un * expr
  | EBinary of expr * op_bin * expr
  | EAssign of string * op_bin * expr

(* Statement node *)
type stat = raw_stat node
and raw_stat = 
  | SExpr of expr
  | SList of stat list
  | SWhile of expr * stat
  | SFor of stat * expr * stat
  | SDecl of tp * string * expr

(* Utility functions to access elements of $loc tuple in .mly *) 
let fst tup = let (x,_) = tup in x
let snd tup = let (_,y) = tup in y

(* pretty printing of types and operators for use in error handling *)
let show_pretty_t_prim typ = 
  match typ with
  | TInt -> "int" | TChar -> "char"
  | TFloat -> "float" | TString -> "string" 
  | TBool -> "bool" | TVoid -> "void"

let rec show_pretty_tuple tup =
  if List.length tup == 0 then "()" else
  "(" ^ (String.concat "," (List.map show_pretty_tp tup)) ^ ")"

and show_pretty_tp typ = 
  match typ with
  | TPrim prim -> show_pretty_t_prim prim
  | TArray arr -> sprintf "%s[]" (show_pretty_tp arr)
  | TNtuple tup -> show_pretty_tuple tup    
  | TFun (args, ret) -> sprintf "%s -> %s" (show_pretty_tuple args) (show_pretty_tp ret)
  | TClass cname -> cname
  | TDummy -> "DUMMY"
  | TInvalid -> "INVALID"

let show_pretty_op_bin op = 
  match op with
  | OIden -> "iden"
  | OExp -> "**"
  | OMult -> "*" | ODiv -> "/" | OMod -> "%"
  | OAdd -> "+" | OSub -> "-" 
  | OBitl -> "<<" | OBitr -> ">>"
  | OLt -> "<" | OGt -> ">" | OLeq -> "<=" | OGeq -> ">="
  | OEq -> "==" | ONeq -> "!="
  | OBitAnd | OGenAnd -> "&"
  | OBitXor | OGenXor -> "^"
  | OBitOr  | OGenOr -> "|"
  | OLogAnd -> "&&"
  | OLogOr -> "||" 

let show_pretty_op_un op =
  match op with
  | OLogNot -> "!" | OBitNot -> "~" | OPos -> "+" | ONeg -> "-"

(* "show" printing utilities for expressions, and statements *)
let show_position p = sprintf "{ Lexing.pos_fname = %s; pos_lnum = %d; pos_bol = %d; pos_cnum = %d }" 
                        p.pos_fname p.pos_lnum p.pos_bol p.pos_cnum

let rec show_raw_expr ex =
  match ex with
  | ELitInt v           -> sprintf "(Ast.ELitInt %d)" v
  | ELitFloat f         -> sprintf "(Ast.ELitFloat %f)" f
  | ELitChar c          -> sprintf "(Ast.ELitChar %c)" c
  | ELitString str      -> sprintf "(Ast.ELitString \"%s\")" str
  | ELitBool b          -> sprintf "(Ast.ELitBool %s)" (if b then "true" else "false")
  | EBinary (l, op, r)  -> sprintf "(Ast.EBinary %s %s %s)" (show_op_bin op) (show_expr l) (show_expr r)
  | EUnary (op, exp)    -> sprintf "(Ast.EUnary %s %s)" (show_op_un op) (show_expr exp)
  | EAssign (var_name, assign, ex) -> 
      let assign_str = match assign with
        | OIden -> "="
        | OAdd -> "+="
        | OSub -> "-="
        | OMult -> "*="
        | ODiv -> "/="
        | _    -> "invalid_assign_op"
      in
      sprintf "(Ast.EAssign %s %s %s)" var_name assign_str (show_expr ex)
and show_expr ex = sprintf "[%s %s %s]" 
                      (show_raw_expr ex.value) (show_position ex.st_loc) (show_position ex.en_loc)

let rec show_raw_expr_silent ex =
  match ex with
  | ELitInt v           -> sprintf "(Ast.ELitInt %d)" v
  | ELitFloat f         -> sprintf "(Ast.ELitFloat %f)" f
  | ELitChar c          -> sprintf "(Ast.ELitChar %c)" c
  | ELitString str      -> sprintf "(Ast.ELitString \"%s\")" str
  | ELitBool b          -> sprintf "(Ast.ELitBool %s)" (if b then "true" else "false")
  | EBinary (l, op, r)   -> sprintf "(Ast.EBinary %s %s %s)" (show_op_bin op) (show_expr_silent l) (show_expr_silent r)
  | EUnary (op, exp)     -> sprintf "(Ast.EUnary %s %s)" (show_op_un op) (show_expr_silent exp)
  | EAssign (var_name, assign, ex) -> 
      let assign_str = match assign with
        | OIden -> "="
        | OAdd -> "+="
        | OSub -> "-="
        | OMult -> "*="
        | ODiv -> "/="
        | _    -> "invalid_assign_op"
      in
      sprintf "(Ast.EAssign %s %s %s)" var_name assign_str (show_expr_silent ex)
and show_expr_silent ex = sprintf "%s" (show_raw_expr_silent ex.value)

let rec show_raw_stat st =
  match st with
  | SExpr ex            -> sprintf "(Ast.SExpr %s)" (show_expr ex)
  | SList xs            -> sprintf "(Ast.SList [%s])" (String.concat ", " (List.map show_stat xs))  
  | SWhile (cond, body) -> sprintf "(Ast.SWhile cond = %s, body = %s)" (show_expr cond) (show_stat body)
  | SDecl (ty, var_name, ex) -> sprintf "(Ast.SAssign %s %s = %s)" (show_tp ty) var_name (show_expr ex)
  | _                   -> "invalid statement token"
and show_stat st = sprintf "[%s %s %s]" (show_raw_stat st.value) (show_position st.st_loc) (show_position st.en_loc)

let rec show_raw_stat_silent st =
  match st with
  | SExpr ex            -> sprintf "(Ast.SExpr %s)" (show_expr_silent ex)
  | SList xs            -> sprintf "(Ast.SList [%s])" (String.concat ", " (List.map show_stat_silent xs))  
  | SWhile (cond, body) -> sprintf "(Ast.SWhile cond = %s, body = %s)" (show_expr_silent cond) (show_stat_silent body)
  | SDecl (ty, var_name, ex) -> sprintf "(Ast.SAssign %s %s = %s)" (show_tp ty) var_name (show_expr_silent ex)
  | _                   -> "invalid statement token"
and show_stat_silent st = sprintf "%s" (show_raw_stat_silent st.value)