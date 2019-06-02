
(** symbol table mapping string id to value *)
type 'a symtable

(** initializes empty symbol table *)
val symtable_init : 'a symtable 

(** adds a new scope to the symbol table *)
val symtable_new_scope : 'a symtable -> 'a symtable

(** removes innermost scope from table and deletes variables declared in it *)
val symtable_leave_scope : 'a symtable -> 'a symtable

(** add specified id-value mapping in specified scope *)
val symtable_add_scope : 'a symtable -> string -> 'a -> int -> 'a symtable

(** returns option of value set in given scope or higher up *)
val symtable_set_scope : 'a symtable -> string -> 'a -> int -> 'a symtable

(** returns option of value mapped to given id in the scope provided or higher up *)
val symtable_find_scope : 'a symtable -> string -> int -> 'a option

(** only look for variable in innermost scope (to see if declarations are valid) *)
val symtable_find_within_scope : 'a symtable -> string -> 'a option

(** return the name of the variable with innermost scope number appended to it *)
val symtable_get_scoped_name_scope: 'a symtable -> string -> int -> string

(** add provided id-value mapping in the innermost declared scope *)
val symtable_add : 'a symtable -> string -> 'a -> 'a symtable

(** return option of value mapped to id in innermost scope *)
val symtable_find : 'a symtable -> string -> 'a option

(** returns option of value set in innermost scope it was declared in *)
val symtable_set : 'a symtable -> string -> 'a -> 'a symtable

val symtable_get_scoped_name: 'a symtable -> string -> string