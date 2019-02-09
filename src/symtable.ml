open Core
open Err

(* type that represents symbol table used in type-checking *)
(* the id name is mangled upon insertion, etc. with scope info, i.e. "name", scope 2 = "name!2" *)
type 'a symtable = { scope: int; table: (string, 'a, String.comparator_witness) Map.t }

let symtable_init = { scope = -1; table = Map.empty (module String) }
let symtable_new_scope table = { table with scope = table.scope+1 }

(* removes innermost scope from table and deletes variables declared in it *)
let symtable_leave_scope table = 
  let scope = table.scope in
  let not_filter ~substring str = not (String.is_substring ~substring:substring str) in 
  let filtered_table = Map.filter_keys ~f: (not_filter ~substring:("!" ^ (string_of_int scope))) table.table  in
  {table = filtered_table; scope = scope-1 }

(* general insertion/removal/find in which scope can be specified *)
let symtable_add_scope symtable id value scope = 
  let new_table = 
    Map.set symtable.table ~key: (id ^ "!" ^ (string_of_int scope)) ~data:value
  in {symtable with table = new_table}

(* set variable in innermost scope it's in *)
let rec symtable_set_scope table id value scope = 
  (* this set is only ever called after type-checking *)
  if scope = -1 then raise (CoreError "couldn't find variable already declared in symtable") else
  match (Map.find table.table (id ^ "!" ^ (string_of_int scope))) with 
  | Some _ -> 
    (* found variable in this scope, set it *)
    { table with table = Map.set table.table ~key: (id ^ "!" ^ (string_of_int scope)) ~data:value }
  | None -> symtable_set_scope table id value (scope-1)

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
let symtable_add symtable id value = symtable_add_scope symtable id value symtable.scope
let symtable_find symtable id = symtable_find_scope symtable id symtable.scope
let symtable_set symtable id value = symtable_set_scope symtable id value symtable.scope