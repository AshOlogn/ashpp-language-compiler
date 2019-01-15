open Ast


(* type-checking utility functions *)
(* return evaluated type, or invalid if arguments have invalid types *)

let check_unary typ = 
  match typ with
  | TPrim TInt    -> TPrim TInt
  | TPrim TFloat  -> TPrim TFloat
  | _             -> TInvalid

let check_binary op t1 t2 =
  match op with
  | OExp | OMult | ODiv | OMod | OAdd | OSub  ->
    (match (t1, t2) with 
    | (TPrim TInt, TPrim TInt) -> TPrim TInt
    | (TPrim TFloat, TPrim TFloat) -> TPrim TFloat
    | _ -> TInvalid)
  | OBitl | OBitr ->
    (match (t1, t2) with
    | (TPrim TInt, TPrim TInt) -> TPrim TInt
    | _ -> TInvalid)
  | OLt | OGt | OLeq | OGeq ->
    (match (t1, t2) with 
      | (TPrim TInt, TPrim TInt) -> TPrim TInt
      | (TPrim TFloat, TPrim TFloat) -> TPrim TFloat
      | _ -> TInvalid)
  | _ -> TInvalid