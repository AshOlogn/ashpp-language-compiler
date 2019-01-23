open Ast

(* helper functions that evaluate literal expressions *)
(* all types are assumed to be correct, so incorrect pattern
combinations merely return the original expression *)

let eval_unary op e =
  match op with
  | OPos -> 
    (match e with
    | ELitInt i   -> Some (ELitInt i)
    | ELitFloat f -> Some (ELitFloat f)
    | _           -> None)
  | ONeg -> 
    (match e with
    | ELitInt i   -> Some (ELitInt (-i))
    | ELitFloat f -> Some (ELitFloat (-1.0 *. f))
    | _           -> None)
  | OBitNot ->
    (match e with 
    | ELitInt i -> Some (ELitInt (lnot i))
    | _         -> None) 
  | OLogNot ->
    (match e with
    | ELitBool b -> Some (ELitBool (not b))
    | _          -> None)

let eval_binary op e1 e2 =
  match op with
  (* OIden just returns the right-hand side *)
  | OIden -> 
    (match e2 with
    | ELitInt _ | ELitFloat _ | ELitBool _ | ELitString _ | ELitChar _ -> Some e2
    | _   -> None)
  | OExp ->
    (match (e1, e2) with
    | (ELitFloat f1, ELitFloat f2) -> Some (ELitFloat (f1 ** f2))
    | _                            -> None)
  | OMult ->
    (match (e1, e2) with
    | (ELitInt i1, ELitInt i2)     -> Some (ELitInt (i1*i2))
    | (ELitFloat f1, ELitFloat f2) -> Some (ELitFloat (f1 *. f2))
    | _                            -> None)
  | ODiv ->
    (match (e1, e2) with
    | (ELitInt i1, ELitInt i2)     -> Some (ELitInt (i1/i2))
    | (ELitFloat f1, ELitFloat f2) -> Some (ELitFloat (f1 /. f2))
    | _                            -> None) 
  | OMod ->
    (match (e1, e2) with
    | (ELitInt i1, ELitInt i2)     -> Some (ELitInt (i1 mod i2))
    | (ELitFloat f1, ELitFloat f2) -> Some (ELitFloat (mod_float f1 f2))
    | _                            -> None) 
  | OAdd -> 
    (match (e1, e2) with
    | (ELitInt i1, ELitInt i2)     -> Some (ELitInt (i1+i2))
    | (ELitFloat f1, ELitFloat f2) -> Some (ELitFloat (f1 +. f2))
    | _                            -> None) 
  | OSub -> 
    (match (e1, e2) with
    | (ELitInt i1, ELitInt i2)     -> Some (ELitInt (i1-i2))
    | (ELitFloat f1, ELitFloat f2) -> Some (ELitFloat (f1 -. f2))
    | _                            -> None)
  | OBitl ->
    (match (e1, e2) with
    | (ELitInt i1, ELitInt i2)     -> Some (ELitInt (i1 lsl i2))
    | _                            -> None)  
  | OBitr ->
    (match (e1, e2) with
    | (ELitInt i1, ELitInt i2)     -> Some (ELitInt (i1 lsr i2))
    | _                            -> None) 
  | OLt -> 
    (match (e1, e2) with
      | (ELitInt i1, ELitInt i2)     -> Some (ELitBool (i1 < i2))
      | (ELitFloat f1, ELitFloat f2) -> Some (ELitBool (f1 < f2))
      | _                            -> None) 
  | OGt ->
    (match (e1, e2) with
    | (ELitInt i1, ELitInt i2)     -> Some (ELitBool (i1 > i2))
    | (ELitFloat f1, ELitFloat f2) -> Some (ELitBool (f1 > f2))
    | _                            -> None)
  | OLeq ->
    (match (e1, e2) with
    | (ELitInt i1, ELitInt i2)     -> Some (ELitBool (i1 <= i2))
    | (ELitFloat f1, ELitFloat f2) -> Some (ELitBool (f1 <= f2))
    | _                            -> None) 
  | OGeq ->
    (match (e1, e2) with
    | (ELitInt i1, ELitInt i2)     -> Some (ELitBool (i1 >= i2))
    | (ELitFloat f1, ELitFloat f2) -> Some (ELitBool (f1 >= f2))
    | _                            -> None)
  | OEq ->
    (match (e1, e2) with
    | (ELitInt i1, ELitInt i2)     -> Some (ELitBool (i1 = i2))
    | (ELitFloat f1, ELitFloat f2) -> Some (ELitBool (f1 = f2))
    | (ELitBool b1, ELitBool b2)   -> Some (ELitBool (b1 = b2))
    | _                            -> None) 
  | ONeq ->
    (match (e1, e2) with
    | (ELitInt i1, ELitInt i2)     -> Some (ELitBool (i1 != i2))
    | (ELitFloat f1, ELitFloat f2) -> Some (ELitBool (f1 != f2))
    | (ELitBool b1, ELitBool b2)   -> Some (ELitBool (b1 != b2))
    | _                            -> None)
  | OBitAnd ->
    (match (e1, e2) with
    | (ELitInt i1, ELitInt i2)     -> Some (ELitInt (i1 land i2))
    | _                            -> None)
  | OBitXor ->
    (match (e1, e2) with
    | (ELitInt i1, ELitInt i2)     -> Some (ELitInt (i1 lxor i2))
    | _                            -> None)
  | OBitOr ->
    (match (e1, e2) with
    | (ELitInt i1, ELitInt i2)     -> Some (ELitInt (i1 lor i2))
    | _                            -> None)
  | OLogAnd ->
    (match (e1, e2) with
    | (ELitBool b1, ELitBool b2)   -> Some (ELitBool (b1 && b2))
    | _                            -> None)
  | OLogOr ->
    (match (e1, e2) with
    | (ELitBool b1, ELitBool b2)   -> Some (ELitBool (b1 || b2))
    | _                            -> None)

  | OGenAnd ->
    (match (e1, e2) with
    | (ELitInt i1, ELitInt i2)   -> Some (ELitInt (i1 land i2))
    | (ELitBool b1, ELitBool b2) -> Some (ELitBool (b1 && b2))
    | _                          -> None)
  | OGenOr ->
    (match (e1, e2) with
      | (ELitInt i1, ELitInt i2)   -> Some (ELitInt (i1 lor i2))
      | (ELitBool b1, ELitBool b2) -> Some (ELitBool (b1 || b2))
      | _                          -> None)
  | OGenXor ->
    (match (e1, e2) with
    | (ELitInt i1, ELitInt i2)   -> Some (ELitInt (i1 lxor i2))
    | _                          -> None)



