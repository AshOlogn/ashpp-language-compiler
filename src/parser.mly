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
%token XOR
%token OR

%token QUESTION COLON
%token EQ ADD_EQ SUBTRACT_EQ EXPONENT_EQ MULTIPLY_EQ DIVIDE_EQ MOD_EQ 
%token AND_EQ XOR_EQ OR_EQ BIT_LEFT_EQ BIT_RIGHT_EQ

%token COMMA SEMICOLON RIGHTARROW 

(* Types, int is just int64_t for now *)
%token <int> INT
%token <char> CHAR
%token <float> DOUBLE
%token <bool> BOOL
%token <string> STRING
%token <string> VARIABLE

(* Reserved Words *)
%token FOR WHILE DO IF ELIF ELSE BREAK SWITCH CASE
%token CLASS EXTENDS FUN RETURN NEW PRINT PRINTLN
%token INT_T CHAR_T FLOAT_T BOOL_T STRING_T VOID_T 

(* Boolean literals *)
%token TRUE FALSE

(* end of program invalid Token *)
%token END INVALID

%start <Ast.expr> main
%{ open Ast %}

%%

(* Trial grammar, not full version *)

let main := 
  ~ = e; END; <>

let e == e_add_sub

let additive_op ==
  | ADD;      { OAdd }
  | SUBTRACT; { OSub }

let e_add_sub :=
  | e_mul_div
  | wrap (~ = e_add_sub; ~ = additive_op; ~ = e_mul_div; <EBinary>) 

let multiplicative_op ==
  | MULTIPLY; { OMult }
  | DIVIDE;   { ODiv }

let e_mul_div :=
  | e_atom
  | wrap (~ = e_mul_div; ~ = multiplicative_op; ~ = e_atom; <EBinary>)

let e_atom :=  
  | LEFT_PAREN; ~ = e; RIGHT_PAREN; <>
  | wrap (~ = INT; <ELiteral>)

(* Wrap the expression into a node containing location information *)
let wrap(x) ==
  ~ = x; { {value = x; st_loc = fst $loc; en_loc = snd $loc; } }


