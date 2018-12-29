(* Type data types  *)
type t_prim = TInt | TChar | TFloat | TString | TBool | TVoid
[@@deriving show]

type t =
  | TArray of t
  | TNtuple of t list
  | TFun of t list * t
  | TPrim of t_prim
[@@deriving show]

(* Operation types *)
type op_bin = OExp | OMult | ODiv | OAdd | OSub
[@@deriving show]

type op_un = ONeg | OPos
[@@deriving show]

(* Nodes used to build abstract syntax tree *)
type expr = 
  | ELiteral of int             (* Generalize this later on *)
  | EUnary of op_un * expr
  | EBinary of expr * op_bin * expr
[@@deriving show]