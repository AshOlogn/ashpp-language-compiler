open Ast
open Checkutils
open Symtable
open Err

(* utility function to functionally update expr node with proper type *)
let fupdate_expr exp typ = { exp with typ = typ }

(* this function takes an expression node, recursively type-checks *)
(* returns checked expression node *)
let rec check_expr (ast, table) = 
  match ast.value with
  | ELitInt _ -> (fupdate_expr ast (TPrim TInt), table)
  | ELitFloat _ -> (fupdate_expr ast (TPrim TFloat), table)
  | ELitChar _ -> (fupdate_expr ast (TPrim TChar), table)
  | ELitString _ -> (fupdate_expr ast (TPrim TString), table)
  | ELitBool _ -> (fupdate_expr ast (TPrim TBool), table)
  | EVar name ->
    (match symtable_find table name with
    | None -> var_not_declared_error ast.st_loc ast.en_loc name
    | Some typ -> (fupdate_expr ast typ, table))
  | EUnary (op, exp) -> 
    let (exp', table') = check_expr (exp, table) in
    let final_type = check_unary op exp'.typ in
    (match final_type with
      | TInvalid -> checker_unop_error ast.st_loc exp'.en_loc op exp'.typ
      | _         -> ({ ast with value = EUnary (op, exp'); typ = final_type }, table'))
  | EBinary (exp1, op, exp2) ->
    let (exp1', table') = check_expr (exp1, table) in
    let (exp2', table'') = check_expr (exp2, table') in
    let final_type = check_binary op exp1'.typ exp2'.typ in
    (match final_type with
    | TInvalid -> checker_binop_error exp1'.st_loc exp2'.en_loc op exp1'.typ exp2'.typ
    | _ -> ({ ast with value = EBinary (exp1', op, exp2'); typ = final_type }, table''))
  | EAssign (name, op, exp) ->
    (match symtable_find table name with
    | None -> var_not_declared_error ast.st_loc ast.en_loc name
    | Some typ -> 
      let (exp', table') = check_expr (exp, table) in
      let final_type = check_binary op typ exp'.typ in
      match final_type with
      | TInvalid -> checker_binop_error exp'.st_loc exp'.en_loc op typ exp'.typ
      | _        -> ({ ast with value = EAssign (name, op, exp')}, table'))

(* this function maps over list of statements, returning checked list and updated symtable *)
let rec check_stat_list stat_list table = 
  match stat_list with 
  | []        -> ([], table)
  | (s :: ss) ->
    let (s', table') = check_stat (s, table) in 
    let (slist, table'') = (check_stat_list ss table') in
    (s' :: slist, table'')

(* this function traverses the preliminary AST and does type/scope-checking *)
(* returns an AST with proper type annotations in place of "dummy" *)
and check_stat (ast, table) =
  match ast.value with
  | SExpr exp -> 
    let (exp', table') = check_expr (exp, table) in
    ({ast with value = SExpr exp' }, table')
  
  | SList stat_list -> 
    let table' = symtable_new_scope table in
    let (stat_list', _) = check_stat_list stat_list table' in
    ({ast with value = SList stat_list'}, table)

  | SWhile (cond, stm) -> 
    let (cond', table') = check_expr (cond, table) in
    let (stm', table'') = check_stat (stm, table') in
    (match cond'.typ with
    | TPrim TBool -> ({ ast with value = SWhile (cond', stm') }, table'')
    | _ -> (ast, table))

  | SFor (stm_init, cond_exp, stm_bod) -> 
    let (stm_init', table') = check_stat (stm_init, table) in
    let (cond_exp', table'') = check_expr (cond_exp, table') in
    let (stm_bod', table''') = check_stat (stm_bod, table'') in
    (match cond_exp'.typ with 
    | TPrim TBool -> ({ ast with value = SFor (stm_init', cond_exp', stm_bod')}, table''')
    | _ -> (ast, table))

  | SDecl (typ, name, decl) ->
    (match symtable_find_within_scope table name with
    | Some _ -> var_mult_declared_error ast.st_loc ast.en_loc name
    | None ->
      let (decl', table') = check_expr (decl, table) in
      let table'' = symtable_add table' name typ in
      ({ ast with value = SDecl (typ, name, decl') }, table''))