open Ast

(* Represents address of stored data or literal *)
type address = 
  | AddrVariable of string 
  | AddrLitInt of int
  | AddrLitFloat of float 
  | AddrLitChar of char
  | AddrLitString of string
  | AddrLitBool of bool
[@@deriving show]

type comparison = GT | LT | GEQ | LEQ | EQ | NEQ
[@@deriving show]

(* Represents a three-address instruction *)
type three_address = 
  | ThreeBinary of address * address * op_bin * address
  | ThreeUnary of address * op_un * address
  | ThreeCopy of address * address
  | ThreeJump of string
  | ThreeCondJump of address * string
  | ThreeCompJump of address * comparison * address * string
  | ThreeParam of address
  | ThreeFunctionCall of string * int
[@@deriving show]

(* Represents labeled three-address instruction *)
type three_instruction = { label: string option; instruction: three_address;}

val show_three_instructions : three_instruction list -> string