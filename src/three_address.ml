open Ast
open Printf

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

(* Represents labeled three-address instruction *)
type three_instruction = { label: int; instruction: three_address;} 

(* printing utilities *)
let show_address addr = 
  match addr with 
  | AddrVariable (name, typ) -> sprintf "(AddrVariable %s %s )" name (show_pretty_tp typ)
  | AddrLitInt literal -> sprintf "(AddrLitInt $%d)" literal 
  | AddrLitFloat literal -> sprintf "(AddrLitFloat $%f)" literal 
  | AddrLitChar literal -> sprintf "(AddrLitChar $%c)" literal
  | AddrLitString literal -> sprintf "(AddrLitChar \"%s\")" literal
  | AddrLitBool literal -> sprintf "(AddrLitBool %B)" literal
  | AddrLitFun fenv -> sprintf "(AddrLitFun %s)" (show_fun_env fenv)

let show_comparison comp = 
  match comp with 
  | GT -> ">" 
  | LT -> "<"
  | GEQ -> ">="
  | LEQ -> "<="
  | EQ -> "=="
  | NEQ -> "!="

let show_three_address addr = 
  match addr with
  | ThreeBinary (addr, addr1, op, addr2) -> 
      sprintf "(ThreeBinary %s = %s %s %s)" 
        (show_address addr) (show_address addr1) (show_pretty_op_bin op) (show_address addr2)
  | ThreeUnary (addr, op, addr') -> 
      sprintf "(ThreeUnary %s = %s %s)" 
        (show_address addr) (show_pretty_op_un op) (show_address addr')
  | ThreeCopy (addr, addr') ->
      sprintf "(ThreeCopy %s = %s)" (show_address addr) (show_address addr') 
  | ThreeJump lnum ->
      sprintf "(ThreeJump label%d)" lnum
  | ThreeCondJump (addr, lnum) ->
      sprintf "(ThreeCondJump if %s to label%d)" (show_address addr) lnum
  | ThreeCompJump (addr, comp, addr', lnum) ->
      sprintf "(ThreeCompJump if %s %s %s to label%d)" 
        (show_address addr) (show_comparison comp) (show_address addr') lnum
  | ThreeParam addr ->
      sprintf "(ThreeParam %s)" (show_address addr)
  | ThreeFunctionCall (addr, name, num_args) ->
      sprintf "(ThreeFunctionCall %s = call %s, %d)" (show_address addr) name num_args
  | ThreeNop -> "(ThreeNop)"

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