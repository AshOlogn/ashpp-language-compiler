open Ast
open Three_address

let rec gen_three_address_expr ast temp label = 
    match ast.value with 
    | ELitInt i -> (AddrLitInt i, [], temp, label)
    | ELitFloat f -> (AddrLitFloat f, [], temp, label) 
    | ELitChar c -> (AddrLitChar c, [], temp, label)
    | ELitBool b ->
        (* Booleans represented as int in assembly *) 
        (AddrLitInt (if b then 1 else 0), [], temp, label)
    | EVar name -> 
        let new_temp = AddrVariable ("t" ^ (string_of_int temp), ast.typ) in 
        let new_ins =  ThreeCopy (new_temp, AddrVariable (name, ast.typ)) in
        (new_temp, [{label = None; instruction = new_ins; }], temp+1, label)
    | EUnary (op, exp) -> 
        let (addr, instructions, temp', label') = gen_three_address_expr exp temp label in 
        let new_temp = AddrVariable ("t" ^ (string_of_int temp'), exp.typ) in 
        let new_ins = ThreeUnary (new_temp, op, addr) in 
        (new_temp, instructions @ [{label=None; instruction = new_ins; }], temp'+1, label')
    | EBinary (exp1, op, exp2) ->
        let (addr, instructions, temp', label') = gen_three_address_expr exp1 temp label in
        let (addr', instructions', temp'', label'') = gen_three_address_expr exp2 temp' label' in
        let new_temp = AddrVariable ("t" ^ (string_of_int temp''), exp1.typ) in 
        let new_ins = ThreeBinary (new_temp, addr, op, addr') in
        (new_temp, instructions @ instructions' @ [{label=None; instruction=new_ins; }], temp''+1, label'')
    | EAssign (name, op, exp) -> 
        let (addr, instructions, temp', label') = gen_three_address_expr exp temp label in
        (match op with
        | OIden ->
            (* e.g. y = 5+3 *)
            let new_ins = ThreeCopy (AddrVariable (name, exp.typ), addr) in
            (AddrVariable (name, exp.typ), instructions @ [{label=None; instruction=new_ins;}], temp', label') 
        | _ -> 
            (* e.g. y *= 5+3 *)
            let new_temp = AddrVariable ("t" ^ (string_of_int temp'), exp.typ) in 
            let new_ins1 = {label=None; instruction=ThreeBinary (new_temp, AddrVariable (name, exp.typ), op, addr)} in 
            let new_ins2 = {label=None; instruction=ThreeCopy (AddrVariable (name, exp.typ), new_temp)} in 
            (AddrVariable (name, exp.typ), instructions @ [new_ins1; new_ins2], temp'+1, label'))
    | _ -> (AddrVariable ("dummy", TDummy), [], 1, 1)

and gen_three_address_stat_list stat_list temp label = 
    match stat_list with 
    | [] -> ([], temp, label)
    | (s :: ss) -> 
        let (instructions, temp', label') = gen_three_address_stat s temp  label in 
        let (instructions', temp'', label'') = gen_three_address_stat_list ss temp'  label' in
        (instructions @ instructions', temp'', label'')

and gen_three_address_stat ast temp label = 
    match ast.value with 
    | SExpr expr -> 
        let (_, instructions, temp', label') = gen_three_address_expr expr temp label in 
        (instructions, temp', label')
    | SList stat_list -> gen_three_address_stat_list stat_list temp label
    | SDecl (_, name, expr) ->
        let (addr, instructions, temp', label') = gen_three_address_expr expr temp label in 
        let new_ins = {label=None; instruction=ThreeCopy (AddrVariable (name, expr.typ), addr);} in 
        (instructions @ [new_ins], temp', label')
    | _ -> ([], temp, label)