open Ast
open Symtable
open Opt_const_fold
open Three_address

let rec gen_three_address_expr ast temp label table = 
    match ast.value with 
    | ELitInt i -> (AddrLitInt i, [], temp, label, table)
    | ELitFloat f -> (AddrLitFloat f, [], temp, label, table) 
    | ELitChar c -> (AddrLitChar c, [], temp, label, table)
    | ELitBool b -> (AddrLitBool b, [], temp, label, table)
    | EVar name -> 
        let scoped_name = symtable_get_scoped_name table name in
        (AddrVariable (scoped_name, ast.typ), [], temp, label, table)
    | EUnary (op, exp) -> 
        let (addr, instructions, temp', label', table') = gen_three_address_expr exp temp label table in 
        let new_temp = AddrVariable ("t" ^ (string_of_int temp'), exp.typ) in 
        let new_ins = ThreeUnary (new_temp, op, addr) in 
        (new_temp, instructions @ [{label = -1; instruction = new_ins; }], temp'+1, label', table')
    | EBinary (exp1, op, exp2) ->
        let (addr, instructions, temp', label', table') = gen_three_address_expr exp1 temp label table in
        let (addr', instructions', temp'', label'', table'') = gen_three_address_expr exp2 temp' label' table' in
        let new_temp = AddrVariable ("t" ^ (string_of_int temp''), exp1.typ) in 
        let new_ins = ThreeBinary (new_temp, addr, op, addr') in
        (new_temp, instructions @ instructions' @ [{label = -1; instruction=new_ins; }], temp''+1, label'', table'')
    | EAssign (name, op, exp) -> 
        let scoped_name = symtable_get_scoped_name table name in 
        let (addr, instructions, temp', label', table') = gen_three_address_expr exp temp label table in
        (match op with
        | OIden ->
            (* e.g. y = 5+3 *)
            let new_value = 
                (match exp.value with
                | ELitInt i     -> ValueInt i
                | ELitFloat f   -> ValueFloat f
                | ELitChar c    -> ValueChar c
                | ELitBool b    -> ValueBool b
                | ELitString s  -> ValueString s
                | _             -> ValueNone)
            in
            let table'' = symtable_set table' name new_value in
            let new_ins = ThreeCopy (AddrVariable (scoped_name, exp.typ), addr) in
            (AddrVariable (scoped_name, exp.typ), instructions @ [{label = -1; instruction=new_ins;}], temp', label', table'') 
        | _ -> 
            (* e.g. y *= 5+3 *)
            let new_temp = AddrVariable ("t" ^ (string_of_int temp'), exp.typ) in 
            let new_ins1 = {label = -1; instruction=ThreeBinary (new_temp, AddrVariable (scoped_name, exp.typ), op, addr)} in 
            let new_ins2 = {label = -1; instruction=ThreeCopy (AddrVariable (scoped_name, exp.typ), new_temp)} in 
            (AddrVariable (scoped_name, exp.typ), instructions @ [new_ins1; new_ins2], temp'+1, label', table'))
    | _ -> (AddrVariable ("dummy", TDummy), [], 1, 1, table)

and gen_three_address_stat_list stat_list temp label table = 
    match stat_list with 
    | [] -> ([], temp, label, table)
    | (s :: ss) -> 
        let (instructions, temp', label', table') = gen_three_address_stat s temp label table in 
        let (instructions', temp'', label'', table'') = gen_three_address_stat_list ss temp' label' table' in
        (instructions @ instructions', temp'', label'', table'')

and gen_three_address_stat ast temp label table = 
    match ast.value with 
    | SExpr expr -> 
        let (_, instructions, temp', label', table') = gen_three_address_expr expr temp label table in 
        (instructions, temp', label', table')
    | SList stat_list -> 
        let table' = symtable_new_scope table in 
        let (instructions, temp', label', table'') = gen_three_address_stat_list stat_list temp label table' in 
        (instructions, temp', label', symtable_leave_scope table'')
    | SDecl (typ, name, expr) ->
        let (addr, instructions, temp', label', table') = gen_three_address_expr expr temp label table in 
        let var_value =
            (match  expr.value with
            | ELitInt i     -> ValueInt i
            | ELitFloat f   -> ValueFloat f
            | ELitChar c    -> ValueChar c
            | ELitBool b    -> ValueBool b
            | ELitString s  -> ValueString s
            | _             -> ValueNone)
        in
        let table'' = symtable_add table' name var_value in 
        let scoped_name = symtable_get_scoped_name table'' name in 
        let new_ins = {label = -1; instruction=ThreeCopy (AddrVariable (scoped_name, typ), addr);} in 
        (instructions @ [new_ins], temp', label', table'')
    | SIf (cond, body) -> 
        let (addr, instructions, temp', label', table') = gen_three_address_expr cond temp label table in
        let label1 = label' in 
        let (instructions', temp'', label'', _) = gen_three_address_stat body temp' (label'+1) table' in
        let instructions_final = 
            instructions @
            [{label = -1; instruction = ThreeCondJump (addr, label1)}] @
            instructions' @ 
            [{label=label1; instruction = ThreeNop}]
        in
        (instructions_final, temp'', label'', table')
    | SIfElse (cond, body, catch) -> 
        let (addr, instructions, temp', label', table') = gen_three_address_expr cond temp label table in 
        let label1 = label' in 
        let label2 = label'+1 in 
        let (instructions1, temp'', label'', _) = gen_three_address_stat catch temp' (label'+2) table' in
        let (instructions2, temp''', label''', _) =  gen_three_address_stat body temp'' label'' table' in
        let instructions_final = 
            instructions @ 
            [{label = -1; instruction= ThreeCondJump (addr, label1)}] @
            instructions1 @ 
            [{label = -1; instruction= ThreeJump label2}] @ 
            [{(List.hd instructions2) with label=label1}] @
            (List.tl instructions2) @
            [{label=label2; instruction=ThreeNop}]
        in
        (instructions_final, temp''', label''', table')
    | SWhile (cond, stm) -> 
        let (addr, instructions, temp', label', table') = gen_three_address_expr cond temp label table in
        let label1 = label' in 
        let label2 = label'+1 in 
        let (instructions', temp'', label'', _) = gen_three_address_stat stm temp' (label'+2) table' in
        let instructions_final = 
            [{(List.hd instructions) with label=label1}] @
            (List.tl instructions) @
            [{label = -1; instruction = 
                ThreeUnary (AddrVariable ("t" ^ (string_of_int temp''), TPrim TBool), OLogNot, addr)};
            {label = -1; instruction = 
                ThreeCondJump (AddrVariable ("t" ^ (string_of_int temp''), TPrim TBool), label2)}] @
            instructions' @
            [{label = label2; instruction = ThreeNop }]
        in
        (instructions_final, temp''+1, label'', table')
    | _ -> ([], temp, label, table)