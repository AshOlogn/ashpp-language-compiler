open Core

(* type that represents symbol table used in type-checking *)
(* the id name is mangled upon insertion, etc. with scope info, i.e. "name", scope 2 = "name!2" *)
type 'a symtable = { scope: int; table: (string, 'a, String.comparator_witness) Map.t }

let symtable_init = { scope = -1; table = Map.empty (module String) }
let symtable_new_scope table = { table with scope = table.scope+1 }

(* general insertion/removal/find in which scope can be specified *)
let symtable_add_scope symtable id typ scope = 
  let new_table = 
    (match Map.add symtable.table ~key: (id ^ "!" ^ (string_of_int scope)) ~data:typ with 
    | `Ok tabl -> tabl
    | _        -> symtable.table)
  in {symtable with table = new_table}

(* find in any scope, so go up if not declared in this one *)
let rec symtable_find_scope symtable id scope = 
  if scope = -1 then None else
  match (Map.find symtable.table (id ^ "!" ^ (string_of_int scope))) with 
  | Some value -> Some value
  | None -> symtable_find_scope symtable id (scope-1)

(* only look for variable in innermost scope (to see if declarations are valid) *)
let symtable_find_within_scope symtable id = 
  Map.find symtable.table (id ^ "!" ^ (string_of_int symtable.scope))

(* the usual add/remove/find, scope taken to be current scope in symbol table *)
let symtable_add symtable id typ = symtable_add_scope symtable id typ symtable.scope
let symtable_find symtable id = symtable_find_scope symtable id symtable.scope