open Ast

(* Represents address of stored data or literal *)
type address = 
  | AddrVariable of string * tp
  | AddrLitInt of int
  | AddrLitFloat of float
  | AddrLitChar of char
  | AddrLitString of string
  | AddrLitBool of bool
  | AddrLitFun of fun_env 

type comparison = GT | LT | GEQ | LEQ | EQ | NEQ

(* Represents a three-address instruction *)
type three_address = 
  | ThreeBinary of address * address * op_bin * address
  | ThreeUnary of address * op_un * address
  | ThreeCopy of address * address
  | ThreeJump of int
  | ThreeCondJump of address * int
  | ThreeCompJump of address * comparison * address * int
  | ThreeParam of address
  | ThreeFunctionCall of address * string * int
  | ThreeNop

(* printing utilities *)
val show_address : address -> string 
val show_comparison : comparison -> string 
val show_three_address : three_address -> string 

(* Represents labeled three-address instruction *)
type three_instruction = { label: int; instruction: three_address;} 

(* printing the instructions in a nicely-formatted way *)
val show_three_instructions : three_instruction list -> string 