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
  | EFunction fenv -> 
    (* extract args and body from function environment *)
    let params = fenv.params in 
    let body = fenv.body in

    (* add all the function parameters to symbol table in new scope *)
    let num_params = List.length params in
    let ref_table = ref (symtable_new_scope table) in
    for i = 0 to (num_params-1) do
      let param = List.nth params i in 
      ref_table := 
        (match symtable_find_within_scope !ref_table (snd param) with 
        | None -> symtable_add !ref_table (snd param) (fst param)
        | Some _ -> var_mult_param_declared_error ast.st_loc ast.en_loc (snd param))
      ; 
    done;
    (* now check the function body *)
    let (body', _) = check_stat (body, !ref_table) in
    (* now do return value analysis assuming function body is checker *)
    let (ret_type, complete, body'') = check_return body' in
      if not complete && (ret_type != (TPrim TVoid)) then 
        (incomplete_return_error ast.st_loc ast.en_loc ret_type)
      else
        let fun_type = TFun ((List.map fst params) @ [ret_type]) in
        ({ast with value = EFunction {params=params; body=body''}; typ = fun_type }, table)
  
  | EFunctionCall (fname, args) -> 
      let ftype = symtable_find table fname in
      let num_args = List.length args in 
      let stripped_args = strip_arg_names args in 
      match ftype with 
      | Some (TFun ftypes) -> 
          let num_params = List.length ftypes in 
          if num_params < num_args then 
            checker_function_numarg_error ast.st_loc ast.en_loc fname num_params num_args 
          else 
            (* now evaluate all the arguments and make sure that they are correctly typed *)
            let ref_table = ref table in 
            for i = 0 to (num_args-1) do 
              let (checked_arg, table') = check_expr ((List.nth stripped_args i), !ref_table) in 

              (* evaluated arg and associated param must have same type *)
              let curr_param_type = List.nth ftypes i in 
              let arg_type =  checked_arg.typ in 
              if not (equal_tp curr_param_type arg_type) then 
                raise (CoreError "hello")
              else 
                ref_table := table';
            done;

            (* now calculate type of output, since partial function application is allowed *)
            let rec list_suffix x_list n = 
              if n = 0 then x_list else 
              (match x_list with 
              | (_ :: xs) -> list_suffix xs (n-1)
              | [] ->  raise (CoreError "queried suffix longer than input list length"))
            in
            let ftype_suffix = list_suffix ftypes (List.length args) in 
            (fupdate_expr ast (if (List.length ftype_suffix) = 1 then
                (List.hd ftype_suffix) else (TFun ftype_suffix)), !ref_table) 

      | Some t -> checker_function_call_error ast.st_loc ast.en_loc fname t 
      | None ->  var_not_declared_error ast.st_loc ast.en_loc fname

(* this function checks whether a function returns a value of a required type *)
(* assumes function body is checked for type/scope errors *)
(* ast - function body, ret_type - function return type (inferred), complete - if every code path returns something *)
and check_return ast  = 
  match ast.value with
  (* expression don't have return type *)
  | SExpr _                 -> (TPrim TVoid, false, ast)

  (* loop through statements, perhaps infer return type *)
  | SList stat_list         ->
    let num_stats = List.length stat_list in
    (* container to keep track/update function return value *)
    let ret_type' = ref (TPrim TVoid) in
    (* whether this statement list does return *)
    let complete' = ref false in
    (* rebuild stat list to prune stuff after return statement *)
    let stat_list' = ref [] in
    let i = ref 0 in
    while !i < num_stats do
      let curr_stat = List.nth stat_list !i in
      (* recursively check the current statement *)
      let (ret_type, _, curr_stat') = check_return curr_stat in
      if (!ret_type' != (TPrim TVoid)) && (!ret_type' != ret_type) then
        (* if return types contradict, then throw an error *)
        (inconsistent_return_error curr_stat.st_loc curr_stat.en_loc ret_type !ret_type')
      else
        (* otherwise, update return type and check for completeness *)
        ret_type' := if !ret_type' == (TPrim TVoid) then ret_type else !ret_type';
        stat_list' := curr_stat' :: !stat_list';
      
      (* depending on statement just checked, destermine completeness of this stat list *)
      complete' :=
        (match curr_stat.value with 
        | SReturn _ -> true
        | _         -> false)
      ;

      (* if the statement is complete, then break out, otherwise increment *)
      i := if !complete' then num_stats else !i+1;
    done;
    (!ret_type', !complete', { ast with value = SList (List.rev !stat_list') } )
    
  | SWhile (cond, stm)         -> 
    let (return_type, complete, stm') = check_return stm in 
    (return_type, complete, { ast with value = SWhile (cond, stm') })
  | SReturn exp             -> (exp.typ, true, ast)
  | _ -> (TPrim TVoid, false, ast)

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
    let (stat_list', table'') = check_stat_list stat_list table' in
    ({ast with value = SList stat_list'}, symtable_leave_scope table'')

  | SWhile (cond, stm) -> 
    let (cond', table') = check_expr (cond, symtable_new_scope table) in
    (match cond'.typ with
    | TPrim TBool -> 
      let (stm', _) = check_stat (stm, table') in
      ({ ast with value = SWhile (cond', stm') }, symtable_leave_scope table')
    | _ -> while_cond_error cond.st_loc cond.en_loc cond'.typ)

  | SFor (stm_init, cond_exp, stm_bod, stm_update) -> 
    let (stm_init', table') = check_stat (stm_init, symtable_new_scope table) in
    let (cond_exp', table'') = check_expr (cond_exp, table') in
    (match cond_exp'.typ with 
    | TPrim TBool ->
      (* body evaluates in a new scope *)
      let (stm_bod', table''') = check_stat (stm_bod, table'') in
      let (stm_update', table'''') = check_stat (stm_update, table''') in
      ({ ast with value = SFor (stm_init', cond_exp', stm_bod', stm_update')}, symtable_leave_scope table'''')
    | _ -> for_cond_error cond_exp.st_loc cond_exp.en_loc cond_exp'.typ)

  | SIf (cond, body) -> 
    let (cond', table') = check_expr (cond, symtable_new_scope table) in
    (match cond'.typ with
    | TPrim TBool -> 
      let (body', _) = check_stat (body, table') in 
      ({ ast with value = SIf (cond', body')}, symtable_leave_scope table')
    | _ -> for_cond_error cond.st_loc cond.en_loc cond'.typ)

  | SIfElse (cond, body, catch) ->
    let (cond', table') = check_expr (cond, symtable_new_scope table) in
    (match cond'.typ with
    | TPrim TBool -> 
      let (body', _) = check_stat (body, table') in
      let (catch', _) = check_stat (catch, table') in
      ({ ast with value = SIfElse (cond', body', catch')}, symtable_leave_scope table')
    | _ -> for_cond_error cond.st_loc cond.en_loc cond'.typ)
  
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