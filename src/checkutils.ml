open Ast

(* type-checking utility functions *)
(* return evaluated type, or invalid if arguments have invalid types *)
(* exceptions are thrown by the actual checker functions *)
let check_unary op t = 
  match op with
  | ONeg | OPos ->
    (match t with 
    | TPrim TInt | TPrim TFloat -> t
    | _ -> TInvalid)
  | OBitNot ->
    (match t with
    | TPrim TInt -> t
    | _          -> TInvalid)
  | OLogNot ->
    (match t with 
    | TPrim TBool -> t
    | _     -> TInvalid)

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
  | OEq | ONeq ->
    (match (t1, t2) with 
    | (TPrim TInt, TPrim TInt) -> TPrim TBool
    | (TPrim TFloat, TPrim TFloat) -> TPrim TBool
    | (TPrim TBool, TPrim TBool) -> TPrim TBool
    | _                          -> TInvalid)
  | OBitAnd | OBitXor | OBitOr ->
    (match (t1, t2) with 
    | (TPrim TInt, TPrim TInt) -> TPrim TInt
    | _                        -> TInvalid)
  | OLogAnd | OLogOr ->
    (match (t1, t2) with 
    | (TPrim TBool, TPrim TBool) -> TPrim TBool
    | _                          -> TInvalid)
  | OGenAnd | OGenXor | OGenOr ->
    (match (t1, t2) with 
    | (TPrim TBool, TPrim TBool) -> TPrim TBool
    | (TPrim TInt, TPrim TInt) -> TPrim TInt
    | _                        -> TInvalid)
  | _ -> TInvalid