open OUnit2
open Src.Symtable

(* make sure inserted value can be retrieved *)
let symtable_retrieve_test _ = 
  let sym = symtable_init in
  let sym' = symtable_add sym "x" 5 in
  assert_equal (symtable_find sym' "x") (Some 5)
  
(* make sure finding absent value returns None *)
let symtable_absent_test _ = 
  let sym = symtable_init in
  assert_equal (symtable_find sym "x") None

(* make sure scoping works *)
let symtable_scope_test _ = 
  let sym = symtable_init in
  let sym' = symtable_add sym "x" 5 in 
  let sym'' = symtable_add (symtable_new_scope sym') "x" 6 in 
  let inner = symtable_find sym'' "x" in 
  let outer = symtable_find (symtable_leave_scope sym'') "x" in 
  assert_equal (inner,outer) (Some 6, Some 5)

(* set should only modify innermost scope variable *)
let symtable_set_test _ = 
  let sym = symtable_init in
  let sym' = symtable_add sym "x" 5 in 
  let sym'' = symtable_add (symtable_new_scope sym') "x" 6 in
  let sym''' = symtable_set sym'' "x" 666 in 
  assert_equal (symtable_find sym''' "x") (Some 666);
  (* this variable is unchanged *)
  assert_equal (symtable_find (symtable_leave_scope sym''') "x") (Some 5)

(* define test suite *)
let symtable_test = 
  "symtable tests" >:::[
      "retrieve_test" >:: symtable_retrieve_test;
      "absent_test" >:: symtable_absent_test; 
      "scope_test" >:: symtable_scope_test;
      "set_test" >:: symtable_set_test;
]