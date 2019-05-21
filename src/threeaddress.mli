open Ast

(* Represents address of stored data or literal *)
type address = 
  | AddrVariable of string 
  | AddrLitInt of int
  | AddrLitFloat of float 
  | AddrLitChar of char

type comparison = GT | LT | GEQ | LEQ | EQ | NEQ

(* Represents a three-address instruction *)
type threeadress = 
  | ThreeBinary of address * op_bin * address * address 
  | ThreeUnary of address * op_un * address 
  | ThreeCopy of address * address 
  | ThreeJump of string 
  | ThreeCondJump of address * string
  | ThreeCompJump of address * comparison * address * string
  | ThreeParam of address 
  | ThreeFunctionCall of string * int