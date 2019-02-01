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
      | _        -> ({ ast with value = EAssign (name, op, exp'); typ = final_type }, table'))
  
  | EFunction (args, body) -> 
  (* add all the function parameters to symbol table in new scope *)
  (* TODO: make sure function args have unique names *)
  let num_args = List.length args in
  let ref_table = ref (symtable_new_scope table) in
  for i = 0 to (num_args-1) do
    let arg = List.nth args i in 
    ref_table := symtable_add !ref_table (snd arg) (fst arg);
  done;
  (* now check the function body *)
  let (body', _) = check_stat (body, !ref_table) in
  (* now do return value analysis assuming function body is checker *)
  let (ret_type, complete) = check_return body' (TPrim TVoid, true) in
    if not complete then 
      (incomplete_return_error ast.st_loc ast.en_loc ret_type)
    else
      let fun_type = TFun ((List.map fst args), ret_type) in
      ({ast with value = EFunction (args, body'); typ = fun_type }, table)

(* this function checks whether a function returns a value of a required type *)
(* assumes function body is checked for type/scope errors *)
(* ast - function body, ret_type - function return type (inferred), complete - if every code path returns something *)
and check_return ast (ret_type, complete) = 
  match ast.value with
  (* expression don't have return type *)
  | SExpr _                 -> (TPrim TVoid, true)
  | SList stat_list         -> 
    let num_stats = List.length stat_list in
    let complete'' = ref complete in
    let ret_type'' = ref ret_type in
    (* loop through all statements in list, updating completeness and return type if void now *)
    for i = 0 to (num_stats-1) do
      let curr_stat = List.nth stat_list i in
      let (ret_type', complete') = check_return curr_stat (ret_type, complete) in
      if (ret_type != (TPrim TVoid)) && (ret_type != ret_type') then 
        (inconsistent_return_error curr_stat.st_loc curr_stat.en_loc ret_type ret_type')
      else
        (* only complete if every sub-statement is complete *)
        complete'' := !complete'' && complete';
        (* update type if void right now but not void in statement body *)
        ret_type'' := if !ret_type'' == (TPrim TVoid) then ret_type' else !ret_type'' 
    done;
    (!ret_type'', !complete'') 
        
  | SWhile (_, stm)         -> 
    let (ret_type', complete') = check_return stm (ret_type, complete) in
    if (ret_type != (TPrim TVoid)) && (ret_type != ret_type') then
      (inconsistent_return_error stm.st_loc stm.en_loc ret_type ret_type')
    else ((if ret_type != (TPrim TVoid) then ret_type else ret_type'), complete && complete')
  
  | SReturn exp             -> (exp.typ, true)
  | _ -> (TPrim TVoid, true)


(* this function maps over list of statements, returning checked list and updated symtable *)
and check_stat_list stat_list table = 
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

  | SFor (stm_init, cond_exp, stm_bod, stm_update) -> 
    let (stm_init', table') = check_stat (stm_init, table) in
    let (cond_exp', table'') = check_expr (cond_exp, table') in
    let (stm_bod', table''') = check_stat (stm_bod, table'') in
    let (stm_update', table'''') = check_stat (stm_update, table''') in
    (match cond_exp'.typ with 
    | TPrim TBool -> ({ ast with value = SFor (stm_init', cond_exp', stm_bod', stm_update')}, table'''')
    | _ -> (ast, table))

  | SDecl (typ, name, decl) ->
    (match symtable_find_within_scope table name with
    | Some _ -> var_mult_declared_error ast.st_loc ast.en_loc name
    | None ->
      let (decl', table') = check_expr (decl, table) in
      match check_binary OIden typ decl'.typ with
      | TInvalid -> checker_binop_error ast.st_loc ast.en_loc OIden typ decl'.typ
      | _        ->  
        let table'' = symtable_add table' name typ in 
        ({ ast with value = SDecl (typ, name, decl') }, table''))

  | SReturn exp -> 
      let (exp', _) = check_expr (exp, table) in 
      ({ ast with value = (SReturn exp')}, table)