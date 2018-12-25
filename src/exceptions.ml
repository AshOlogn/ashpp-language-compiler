(* General utilities *)
let lined_message line msg = Printf.sprintf "line %d: %s" line msg;;

(* Exceptions in lexer *)
exception Error_lexer of string;;