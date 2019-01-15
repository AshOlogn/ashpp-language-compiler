open Ast
open Checkutils

let dummy = "hello, world"

(* Exceptions to throw during type/scope-checking *)
exception Error_checker of string

(* utility function to functionally update expr node with proper type *)
let fupdate_expr exp typ = { exp with typ = typ }

(* this function takes an expression node, recursively type-checks *)
(* returns checked expression node *)
let rec check_expr src ast = 
  match ast.value with
  | ELitInt _ -> fupdate_expr ast (TPrim TInt)
  | ELitBool _ -> fupdate_expr ast (TPrim TBool)

  | EUnary (op, exp) -> 
    let cexp = check_expr src exp in
    let final_type = check_unary cexp.typ in
    (match final_type with
      | TInvalid -> ast
      | _         -> { ast with value = EUnary (op, cexp); typ = final_type })
  
  | EBinary (exp1, op, exp2) ->
    let cexp1 = check_expr src exp1 in
    let cexp2 = check_expr src exp2 in
    let final_type = check_binary op cexp1.typ cexp2.typ in
    (match final_type with
    | TInvalid -> ast
    | _ -> { ast with value = EBinary (cexp1, op, cexp2); typ = final_type })
  
  | _ -> ast


(* this function traverses the preliminary AST and does type/scope-checking *)
(* returns an AST with proper type annotations in place of "dummy" *)
let rec check_stat src ast =
  match ast.value with
  | SExpr exp -> {ast with value = SExpr (check_expr src exp) }
  | SList exp_list -> { ast with value = SList (List.map (check_stat src) exp_list) }

  | SWhile (cond, stm) -> 
    let ccond = check_expr src cond in
    let cstm = check_stat src stm in
    (match ccond.typ with
    | TPrim TBool -> { ast with value = SWhile (ccond, cstm) }
    | _ -> ast)

  | SFor (stm_init, cond_exp, stm_bod) -> 
    let cstm_init = check_stat src stm_init in
    let ccond_exp = check_expr src cond_exp in
    let cstm_bod = check_stat src stm_bod in
    (match ccond_exp.typ with 
    | TPrim TBool -> { ast with value = SFor (cstm_init, ccond_exp, cstm_bod) }
    | _ -> ast)
  
  | _ -> ast