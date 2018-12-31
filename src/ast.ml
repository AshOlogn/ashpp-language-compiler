open Lexing

(* Some type naming terminology and inspiration for node type from Menhir demo: 
https://gitlab.inria.fr/fpottier/menhir/tree/master/demos/calc-ast-dune *)


(* Type data types, support recursive types like int[][] *)
type t_prim = TInt | TChar | TFloat | TString | TBool | TVoid
[@@deriving show]

type t =
  | TArray of t
  | TNtuple of t list
  | TFun of t list * t
  | TPrim of t_prim
[@@deriving show]

(* Operation types *)
type op_bin = OExp | OMult | ODiv | OAdd | OSub
[@@deriving show]

type op_un = ONeg | OPos
[@@deriving show]

(* Nodes used to build abstract syntax tree, with location metadata *)
type 'a node = { value: 'a; st_loc: Lexing.position; en_loc: Lexing.position; } 

type expr = raw_expr node 

(* Just expression without metadata*)
and raw_expr = 
  | ELiteral of int
  | EUnary of op_un * expr
  | EBinary of expr * op_bin * expr

(* Utility functions to access elements of $loc tuple in .mly *) 
let fst tup = let (x,_) = tup in x
let snd tup = let (_,y) = tup in y

(* can't derive show on Lexing.position, so hacky show function for expr *)
let show_position p = Printf.sprintf "{ Lexing.pos_fname = %s; pos_lnum = %d; pos_bol = %d; pos_cnum = %d }" 
                        p.pos_fname p.pos_lnum p.pos_bol p.pos_cnum

let rec show_raw_expr ex =
  match ex with
  | ELiteral v           -> Printf.sprintf "(Ast.ELiteral %d)" v
  | EBinary (l, op, r)   -> Printf.sprintf "(Ast.EBinary %s %s %s)" (show_op_bin op) (show_expr l) (show_expr r)
  | EUnary (op, exp)     -> Printf.sprintf "(Ast.EUnary %s %s)" (show_op_un op) (show_expr exp)
and show_expr ex = Printf.sprintf "[%s %s %s]" 
                      (show_raw_expr ex.value) (show_position ex.st_loc) (show_position ex.en_loc)

let rec show_raw_expr_silent ex =
  match ex with
  | ELiteral v           -> Printf.sprintf "(Ast.ELiteral %d)" v
  | EBinary (l, op, r)   -> Printf.sprintf "(Ast.EBinary %s %s %s)" (show_op_bin op) (show_expr_silent l) (show_expr_silent r)
  | EUnary (op, exp)     -> Printf.sprintf "(Ast.EUnary %s %s)" (show_op_un op) (show_expr_silent exp)
and show_expr_silent ex = Printf.sprintf "%s" (show_raw_expr_silent ex.value)

(* Statement node *)
type stat = raw_stat node
and raw_stat = 
  | SExpr of expr
  | SList of expr list
  | SWhile of expr * stat
  | SFor of stat * expr * stat

