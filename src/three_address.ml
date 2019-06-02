open Ast

(* Represents address of stored data or literal *)
type address = 
  | AddrVariable of string * tp
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
  | ThreeJump of int
  | ThreeCondJump of address * int
  | ThreeCompJump of address * comparison * address * int
  | ThreeParam of address
  | ThreeFunctionCall of string * int
  | ThreeNop
[@@deriving show]

(* Represents labeled three-address instruction *)
type three_instruction = { label: int; instruction: three_address;} 

(* printing the instructions in a nicely-formatted way *)
let show_three_instructions ins_list = 
  let str = ref "" in 
  let i = ref 0 in 
  while !i < List.length ins_list do
    let {label = label; instruction=ins;} = List.nth ins_list !i in 
    str := !str ^ (string_of_int label) ^ ": " ^ (show_three_address ins) ^ "\n";
    i := !i + 1;
  done;
  !str