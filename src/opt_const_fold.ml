open Ast
open Symtable
open Opt_const_fold_utils

let dummy = "hello, world"

(* value type in symbol table that either holds a literal or expression marker *)
(* if variable lookup yields literal, directly substitute that into ast, else keep original logic *)
type literal_value = 
  | ValueInt of int
  | ValueFloat of float
  | ValueChar of char
  | ValueString of string
  | ValueBool of bool
  | ValueNone

(* this function evaluates constant expressions and variable substitutions to 
yield more efficient machine code, ASSUMES type and scope checking is done *)

let rec const_fold_expr (ast, table) = 
  match ast.value with
  (* easy cases, already a literal *)
  | ELitInt _ 
  | ELitFloat _ 
  | ELitChar _ 
  | ELitString _ 
  | ELitBool _ -> (ast, table)

  (* if the variable is mapped to constant value in symbol table, just sub it in *)
  | EVar name ->
    (match symtable_find table name with
    (* this case should never be called *)
    | None -> (ast, table)
    | Some value ->
      (match value with
      | ValueInt v -> ({ ast with value = ELitInt v; typ = TPrim TInt; }, table) 
      | ValueFloat v -> ({ ast with value = ELitFloat v; typ = TPrim TFloat; }, table) 
      | ValueChar v -> ({ ast with value = ELitChar v; typ = TPrim TChar; }, table) 
      | ValueString v -> ({ ast with value = ELitString v; typ = TPrim TString; }, table) 
      | ValueBool v -> ({ ast with value = ELitBool v; typ = TPrim TBool; }, table) 
      | ValueNone -> (ast, table)))
  
  | EUnary (op, exp) -> 
    let (exp', table') = const_fold_expr (exp, table) in
    let exp''_val = eval_unary op exp'.value in 
    (match exp''_val with
    | None -> ({ ast with value = EUnary (op, exp') }, table')
    | Some exp''_value        -> ({ ast with value = exp''_value }, table'))
    
  | EBinary (exp1, op, exp2) ->
    let (exp1', table') = const_fold_expr (exp1, table) in
    let (exp2', table'') = const_fold_expr (exp2, table') in
    let exp'_val = eval_binary op exp1'.value exp2'.value in
    (match exp'_val with
    | None     -> ({ ast with value = EBinary (exp1', op, exp2') }, table'')
    | Some exp'_value        -> ({ ast with value = exp'_value }, table''))

  | EAssign (name, op, exp) ->

    (* fold the right-hand side first *)
    let (exp', table') = const_fold_expr (exp, table) in

    (match op with
    (* in this case, doesn't matter what value variable has *)
    | OIden ->
      let lit_value = 
      (match exp'.value with 
      | ELitInt i -> ValueInt i
      | ELitFloat f -> ValueFloat f
      | ELitChar c -> ValueChar c
      | ELitString s -> ValueString s
      | ELitBool b -> ValueBool b
      | _          -> ValueNone)
      in 
      let table'' = symtable_add table' name lit_value in
      ({ ast with value = exp'.value; }, table'')
    
    | _    -> 
      (match symtable_find table name with
      (* this case should never occur *)
      | None -> (ast, table)

      (* v = some value representation, maybe none *)
      | Some v -> 
        (match v with
        (* variable is not literal, so can't fold *)
        | ValueNone -> ({ ast with value = EAssign (name, op, exp') }, table')

        (* fold, based on specific type *)
        | ValueInt i -> 
          let exp''_val = eval_binary op (ELitInt i) exp'.value in
          (match exp''_val with
          | None -> 
            (* now variable's value is not literal, so reflect that *)
            let table'' = symtable_add table' name ValueNone in
            ({ ast with value = EAssign (name, op, exp') }, table'')
          | Some lit ->
            (match lit with 
            | ELitInt i ->
              let table'' = symtable_add table' name (ValueInt i) in
              ({ ast with value = lit }, table'')
            (* this should never be called *)
            | _         -> (ast,table) ))

        | ValueFloat f ->
          let exp''_val = eval_binary op (ELitFloat f) exp'.value in
          (match exp''_val with
          | None -> 
            (* now variable's value is not literal, so reflect that *)
            let table'' = symtable_add table' name ValueNone in
            ({ ast with value = EAssign (name, op, exp') }, table'')
          | Some lit ->
            (match lit with 
            | ELitFloat f ->
              let table'' = symtable_add table' name (ValueFloat f) in
              ({ ast with value = lit }, table'')
            (* this should never be called *)
            | _         -> (ast,table) ))

        | ValueChar c ->
          let exp''_val = eval_binary op (ELitChar c) exp'.value in
          (match exp''_val with
          | None -> 
            (* now variable's value is not literal, so reflect that *)
            let table'' = symtable_add table' name ValueNone in
            ({ ast with value = EAssign (name, op, exp') }, table'')
          | Some lit ->
            (match lit with 
            | ELitChar c ->
              let table'' = symtable_add table' name (ValueChar c) in
              ({ ast with value = lit }, table'')
            (* this should never be called *)
            | _         -> (ast,table) ))

        | ValueString _ 
        | ValueBool _ -> (ast, table))))
    
    | EFunction (args, body) -> 
      (* first add args to the symbol table *)
      let num_args = List.length args in
      let ref_table = ref (symtable_new_scope table) in
      for i = 0 to (num_args-1) do
        let arg = List.nth args i in 
        ref_table := symtable_add !ref_table (snd arg) ValueNone;
      done;
      (* replace body field with the folded one *)
      let (body', _) = const_fold_stat (body, !ref_table) in 
      ({ast with value = EFunction (args, body'); }, table)

(* this function maps over list of statements, returning checked list and updated symtable *)
and const_fold_stat_list stat_list table = 
  match stat_list with 
  | []        -> ([], table)
  | (s :: ss) ->
    let (s', table') = const_fold_stat (s, table) in 
    let (slist, table'') = (const_fold_stat_list ss table') in
    (s' :: slist, table'')

(* this function traverses the preliminary AST and does type/scope-checking *)
(* returns an AST with proper type annotations in place of "dummy" *)
and const_fold_stat (ast, table) =
  match ast.value with
  | SExpr exp -> 
    let (exp', table') = const_fold_expr (exp, table) in
    ({ast with value = SExpr exp' }, table')
  
  | SList stat_list -> 
    let table' = symtable_new_scope table in
    let (stat_list', table'') = const_fold_stat_list stat_list table' in
    ({ast with value = SList stat_list'}, symtable_leave_scope table'')

  | SWhile (cond, stm) -> 
    let (cond', table') = const_fold_expr (cond, symtable_new_scope table) in
    (match cond'.value with 
    | ELitBool false -> ({ast with value = SList []}, symtable_leave_scope table')
    | _ ->
      let (stm', table'') = const_fold_stat (stm, table') in
      ({ ast with value = SWhile (cond', stm') }, symtable_leave_scope table''))

  | SFor (stm_init, cond_exp, stm_bod, stm_update) -> 
    let (stm_init', table') = const_fold_stat (stm_init, symtable_new_scope table) in
    let (cond_exp', table'') = const_fold_expr (cond_exp, table') in
    let (stm_bod', table''') = const_fold_stat (stm_bod, table'') in
    let (stm_update', table'''') = const_fold_stat (stm_update, table''') in
    ({ ast with value = SFor (stm_init', cond_exp', stm_bod', stm_update')}, symtable_leave_scope table'''')

  | SIf (cond, body) -> 
    let (cond', table') = const_fold_expr (cond, symtable_new_scope table) in
    (match cond'.value with 
    | ELitBool true -> 
        (* if true, just return the body *)
        let (body', table'') = const_fold_stat (body, table') in 
        ({ast with value = SList [body'] }, symtable_leave_scope table'')
    | ELitBool false -> 
        (* if false, return empty list *)
        ({ast with value = SList [] }, symtable_leave_scope table')      
    | _ -> 
      let (body', table'') = const_fold_stat (body, table') in 
      ({ ast with value = SIf (cond', body')}, symtable_leave_scope table''))
  
  | SIfElse (cond, body, catch) ->
    let (cond', table') = const_fold_expr (cond, symtable_new_scope table) in
    (match cond'.value with 
    | ELitBool true ->
        (* if true, only evaluate the body *)
        let (body', table'') = const_fold_stat (body, table') in 
        ({ast with value = SList [body']}, symtable_leave_scope table'')
    | ELitBool false ->
        (* if false, just evaluate the catch statement *)
        let (catch', table'') = const_fold_stat (catch, table') in 
        ({ast with value = SList [catch']}, symtable_leave_scope table'')
    | _ ->
      let (body', table'') = const_fold_stat (body, table') in
      let (catch', table''') = const_fold_stat (catch, table'') in
      ({ ast with value = SIfElse (cond', body', catch')}, symtable_leave_scope table'''))
  
  | SDecl (typ, name, decl) ->
    let (decl', table') = const_fold_expr (decl, table) in
    let var_value =
    (match  decl'.value with
    | ELitInt i     -> ValueInt i
    | ELitFloat f   -> ValueFloat f
    | ELitChar c    -> ValueChar c
    | ELitBool b    -> ValueBool b
    | ELitString s  -> ValueString s
    | _             -> ValueNone)
    in
    let table'' = symtable_add table' name var_value in 
    ({ ast with value = SDecl (typ, name, decl') }, table'')
  | _ -> (ast, table)

(* go through and get rid of all the nested "empty list" constructs
   left behind from optimizations *)
let rec prune_empty ast = 
  match ast.value with 
  | SList xs -> 
      let func {value = value; typ = _; st_loc = _; en_loc = _} = 
        match value with 
        | SList [] -> false
        | _ -> true
      in
      {ast with value = SList (List.filter func (List.map prune_empty xs)) }
  | _ -> ast