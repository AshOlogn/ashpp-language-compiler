open Ast
open Symtable

(* this function takes an expression node, recursively type-checks *)
(* returns checked expression node *)
val check_expr : (expr * tp symtable) -> (expr * tp symtable)

(* this function maps over list of statements, returning checked list and updated symtable *)
val check_stat_list : stat list -> tp symtable -> (stat list * tp symtable)

(* this function traverses the preliminary AST and does type/scope-checking *)
(* returns an AST with proper type annotations in place of "dummy" *)
val check_stat : (stat * tp symtable) -> (stat * tp symtable)