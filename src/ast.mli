
(** Primitive type representation *)
type t_prim = TInt | TChar | TFloat | TString | TBool | TVoid
[@@deriving show, eq]

(** Recursive data type representation *)
type tp =
  | TPrim of t_prim
  | TArray of tp
  | TNtuple of tp list
  | TFun of tp list * tp
  | TClass of string
  | TDummy
  | TInvalid
[@@deriving show, eq]

(** Binary operation type *)
type op_bin = 
  | OIden
  | OExp 
  | OMult | ODiv | OMod
  | OAdd | OSub 
  | OBitl | OBitr
  | OLt | OGt | OLeq | OGeq
  | OEq | ONeq
  | OBitAnd
  | OBitXor
  | OBitOr
  | OLogAnd
  | OLogOr
  (* these last 3 work for bitwise or logic to handle assignment *)
  | OGenAnd
  | OGenOr
  | OGenXor
[@@deriving show]

(** Unary operation type *)
type op_un = ONeg | OPos | OBitNot | OLogNot
[@@deriving show]

(** Node used to build abstract syntax tree, with location metadata and type information *)
(* The Menhir-based parser sets type as "dummy" to defer type-checking *)
(* For statement nodes, type is always dummy since statements don't return values *)
type 'a node = { value: 'a; typ: tp; st_loc: Lexing.position; en_loc: Lexing.position;} 

type fun_arg = tp * string

(* Expression node, wrapped and unwrapped *)
type expr = raw_expr node 
and raw_expr = 
  | ELitInt of int
  | ELitFloat of float
  | ELitChar of char
  | ELitString of string
  | ELitBool of bool
  | EVar of string
  | EUnary of op_un * expr
  | EBinary of expr * op_bin * expr
  | EAssign of string * op_bin * expr
  | EFunction of fun_arg list * stat

(* Statement node *)
and stat = raw_stat node
and raw_stat = 
  | SExpr of expr
  | SList of stat list
  | SWhile of expr * stat
  | SFor of stat * expr * stat * stat
  | SDecl of tp * string * expr
  | SReturn of expr

(** Get the first element out of a 2-element tuple *) 
val fst : ('a * 'b) -> 'a

(** Get the second element out of a 2-element tuple *) 
val snd : ('a * 'b) -> 'b

(** pretty printing of primitive type *)
val show_pretty_t_prim : t_prim -> string 

(** pretty printing of tuple type (list of types) *)
val show_pretty_tuple : tp list -> string

(** pretty printing of general type *)
val show_pretty_tp : tp -> string

(** pretty printing of binary operator *)
val show_pretty_op_bin : op_bin -> string

(** pretty printing of primitive type *)
val show_pretty_op_un : op_un -> string

(** pretty printing of raw expression *)
val show_raw_expr_silent : raw_expr -> string

(** pretty printing of expression node *)
val show_expr_silent : expr -> string

(** pretty printing of raw statement *)
val show_raw_stat_silent : raw_stat -> string

(** pretty printing of statement node *)
val show_stat_silent : stat -> string