open Ast

(** Checks if given arg type and unary operation are compatible *)
(* returns evaluated type if valid, or TInvalid if not *)
val check_unary : op_un -> tp -> tp 

(** Checks if given arg types and binary operation are compatible *)
(* returns evaluated type if valid, or TInvalid if not *)
val check_binary : op_bin -> tp -> tp -> tp