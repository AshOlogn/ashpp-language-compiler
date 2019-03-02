(*Operators and Symbols, in order of precedence *)
%token LEFT_PAREN RIGHT_PAREN LEFT_BRACKET RIGHT_BRACKET 
%token LEFT_BRACE RIGHT_BRACE PERIOD ADD SUBTRACT NOT BIT_NOT

%token EXPONENT 
%token MULTIPLY DIVIDE MOD
%token BIT_LEFT BIT_RIGHT

%token GREATER LESS GREATER_EQ LESS_EQ
%token EQ_EQ NOT_EQ

%token BIT_AND
%token BIT_XOR
%token BIT_OR

%token AND
%token OR

%token QUESTION COLON
%token EQ ADD_EQ SUBTRACT_EQ EXPONENT_EQ MULTIPLY_EQ DIVIDE_EQ MOD_EQ 
%token AND_EQ XOR_EQ OR_EQ BIT_LEFT_EQ BIT_RIGHT_EQ

%token COMMA SEMICOLON RIGHTARROW 

(* Types, int is just int64_t for now *)
%token <int> INT
%token <char> CHAR
%token <float> FLOAT
%token <bool> BOOL
%token <string> STRING
%token <string> VARIABLE

(* Reserved Words *)
%token FOR WHILE DO IF ELIF ELSE BREAK SWITCH CASE FUNCTION
%token CLASS EXTENDS FUN RETURN NEW PRINT PRINTLN
%token INT_T CHAR_T FLOAT_T BOOL_T STRING_T VOID_T

(* end of program, invalid token *)
%token END INVALID


(* now some precedence and associativity annotations to 
prevent shift-reduce conflicts *)
%left AND_EQ XOR_EQ OR_EQ BIT_LEFT_EQ BIT_RIGHT_EQ
%left EQ ADD_EQ SUBTRACT_EQ EXPONENT_EQ MULTIPLY_EQ DIVIDE_EQ MOD_EQ 
%right QUESTION COLON

%left OR
%left AND

%left BIT_OR
%left BIT_XOR
%left BIT_AND

%left EQ_EQ NOT_EQ
%left GREATER LESS GREATER_EQ LESS_EQ
%left BIT_LEFT BIT_RIGHT
%left MULTIPLY DIVIDE MOD
%left EXPONENT

%start <Ast.stat> main
%{ open Ast %}

%%

(* Trial grammar, not full version *)

let main := 
  wrap_stat (~ = list(s); END; <SList>)

(* Statement grammar *)
let s := 
  | matched_s
  | open_s

let matched_s :=
  | wrap_stat (IF; ~ = e; ~ = matched_s; ELSE; ~ = matched_dummy; <SIfElse>) 
  | other_s

let matched_dummy :=
  | matched_s

let open_s :=
  | wrap_stat (IF; ~ = e; ~ = s; <SIf>)
  | wrap_stat (IF; ~ = e; ~ = matched_s; ELSE; ~ = open_s; <SIfElse>)      

let other_s :=
  | wrap_stat (~ = e; <SExpr>)
  | wrap_stat (RETURN; ~ = e; <SReturn>)
  | wrap_stat (LEFT_BRACE; ~ = list(s); RIGHT_BRACE; <SList>)
  | wrap_stat (WHILE; ~ = e; ~ = s; <SWhile>)
  | wrap_stat (~ = t; ~ = VARIABLE; EQ; ~ = e; <SDecl>)

(* Type grammar *)
let t :=
  | ~ = prim_t; <TPrim>
  | LEFT_BRACKET; ~ = t; RIGHT_BRACKET; <TArray>
  | LEFT_PAREN; ~ = list_delimited(t, COMMA); RIGHT_PAREN; <TNtuple>
  | SEMICOLON; ~ = list_delimited(t, RIGHTARROW); <TFun>
  | ~ = VARIABLE; <TClass>

let prim_t ==
  | INT_T; { TInt }
  | CHAR_T; { TChar }
  | FLOAT_T; { TFloat }
  | STRING_T; { TString }
  | BOOL_T; { TBool }
  | VOID_T; { TVoid }

(* Expression grammar *)
let e == e_assign

let e_assign :=
  | e_log_or
  | wrap_expr (~ = VARIABLE; ~ = assign_op; ~ = e_log_or; <EAssign>)

let e_log_or :=
  | e_log_and
  | wrap_expr (~ = e_log_or; ~ = log_or_op; ~ = e_log_and; <EBinary>) 

let e_log_and :=
  | e_bit_or
  | wrap_expr (~ = e_log_and; ~ = log_and_op; ~ = e_bit_or; <EBinary>) 

let e_bit_or :=
  | e_bit_xor
  | wrap_expr (~ = e_bit_or; ~ = bit_or_op; ~ = e_bit_xor; <EBinary>)

let e_bit_xor :=
  | e_bit_and
  | wrap_expr (~ = e_bit_xor; ~ = bit_xor_op; ~ = e_bit_and; <EBinary>)

let e_bit_and :=
  | e_equality
  | wrap_expr (~ = e_bit_and; ~ = bit_and_op; ~ = e_equality; <EBinary>)

let e_equality :=
  | e_comp
  | wrap_expr (~ = e_equality; ~ = equality_op; ~ = e_comp; <EBinary>)

let e_comp :=
  | e_bit_shift
  | wrap_expr (~ = e_comp; ~ = comp_op; ~ = e_bit_shift; <EBinary>)

let e_bit_shift :=
  | e_add_sub
  | wrap_expr (~ = e_bit_shift; ~ = bit_shift_op; ~ = e_add_sub; <EBinary>)

let e_add_sub :=
  | e_mul_div
  | wrap_expr (~ = e_add_sub; ~ = additive_op; ~ = e_mul_div; <EBinary>) 

let e_mul_div :=
  | e_exponent
  | wrap_expr (~ = e_mul_div; ~ = multiplicative_op; ~ = e_exponent; <EBinary>)

let e_exponent := 
  | e_unop
  | wrap_expr (~ = e_exponent; ~ = exponent_op; ~ = e_unop; <EBinary>)

let e_unop :=
  | e_atom
  | wrap_expr(~ = unary_op; ~ = e_atom; <EUnary> )

let e_atom :=  
  | LEFT_PAREN; ~ = e; RIGHT_PAREN; <>
  | wrap_expr (~ = INT; <ELitInt>)
  | wrap_expr (~ = FLOAT; <ELitFloat>)
  | wrap_expr (~ = CHAR; <ELitChar>)
  | wrap_expr (~ = STRING; <ELitString>)
  | wrap_expr (~ = BOOL; <ELitBool>)
  | wrap_expr (~ = VARIABLE; <EVar>)
  | wrap_expr (FUNCTION; LEFT_PAREN; ~ = list_delimited(fun_arg, COMMA); RIGHT_PAREN; ~ = s; <EFunction>) 

let fun_arg :=
  | x = t; y = VARIABLE; { (x, y) }

(* Operator token conversions *)
let assign_op ==
  | EQ; { OIden }
  | ADD_EQ; { OAdd }
  | SUBTRACT_EQ; { OSub }
  | MULTIPLY_EQ; { OMult }
  | DIVIDE_EQ; { ODiv }
  | MOD_EQ; { OMod }
  | AND_EQ; { OGenAnd }
  | XOR_EQ; { OGenXor }
  | OR_EQ; { OGenOr }
  | BIT_LEFT_EQ; { OBitl }
  | BIT_RIGHT_EQ; { OBitr }
  | EXPONENT_EQ; { OExp }

let log_or_op ==
  | OR; { OLogOr }

let log_and_op ==
  | AND; { OLogAnd }

let bit_and_op ==
  | BIT_AND; { OBitAnd }

let bit_xor_op ==
  | BIT_XOR; { OBitXor }

let bit_or_op ==
  | BIT_OR; { OBitOr }

let equality_op ==
  | EQ_EQ;  { OEq }
  | NOT_EQ; { ONeq }

let comp_op ==
  | LESS; { OLt }
  | LESS_EQ; { OLeq }
  | GREATER; { OGt }
  | GREATER_EQ; { OGeq }

let bit_shift_op ==
  | BIT_LEFT; { OBitl }
  | BIT_RIGHT; { OBitr }

let additive_op ==
  | ADD;      { OAdd }
  | SUBTRACT; { OSub }

let multiplicative_op ==
  | MULTIPLY; { OMult }
  | DIVIDE;   { ODiv }
  | MOD;      { OMod }

let exponent_op ==
  | EXPONENT; { OExp }

let unary_op ==
  | ADD; { OPos }
  | SUBTRACT; { ONeg }
  | NOT; { OLogNot }
  | BIT_NOT; { OBitNot }

(* wrap the expression/statement into a node containing location information *)
let wrap_expr(x) ==
  ~ = x; { {value = x; typ = TDummy; st_loc = fst $loc; en_loc = snd $loc;} }

let wrap_stat(x) ==
  ~ = x; { {value = x; typ = TDummy; st_loc = fst $loc; en_loc = snd $loc;} }

let wrap_stat_toplevel(x) ==
  ~ = x; { {value = x; typ = TDummy; st_loc = fst $loc; en_loc = snd $loc;} }

(* Other utiliies *)
let list_delimited(ltype, delim) :=
  | x = ltype; { [x] }
  | x = ltype; delim; xs = list_delimited(ltype, delim); { x :: xs } 