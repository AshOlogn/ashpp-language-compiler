type token_type = 

  (*Operators and Symbols (in order of precedence) *)
  LEFT_PAREN | RIGHT_PAREN | LEFT_BRACKET | RIGHT_BRACKET |
  LEFT_BRACE | RIGHT_BRACE | PERIOD | ADD | SUBTRACT | NOT | BIT_NOT

  | EXPONENT  
  | MULTIPLY | DIVIDE | MOD
  | BIT_LEFT | BIT_RIGHT

  | GREATER | LESS | GREATER_EQ | LESS_EQ
  | EQ_EQ | NOT_EQ

  | BIT_AND
  | BIT_XOR
  | BIT_OR  

  | AND
  | XOR
  | OR

  | QUESTION | COLON
  | EQ | ADD_EQ | SUBTRACT_EQ | EXPONENT_EQ | MULTIPLY_EQ | DIVIDE_EQ | MOD_EQ 
  | AND_EQ | XOR_EQ | OR_EQ | BIT_LEFT_EQ | BIT_RIGHT_EQ

  | COMMA | SEMICOLON | RIGHTARROW 

  (* Types, int is just int64_t for now *)
  | INT of int | CHAR of char | DOUBLE of float | BOOL of bool | STRING of string | VOID | VARIABLE of string

  (* Reserved Words *)
  | FOR | WHILE | DO | IF | ELIF | ELSE | BREAK | SWITCH | CASE
  | CLASS | EXTENDS | FUN | RETURN | NEW | PRINT | PRINTLN

  (* Boolean literals *)
  | TRUE | FALSE

  (* end of program| invalid Token *)
  | END | INVALID
[@@deriving show]

type token = 
  {
    ttype : token_type;
    startLine : int;
    endLine : int;
  }
[@@deriving show]