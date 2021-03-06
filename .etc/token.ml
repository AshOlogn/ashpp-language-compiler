type token = 

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
  | INT of int | CHAR of char | DOUBLE of float | BOOL of bool | STRING of string | VARIABLE of string

  (* Reserved Words *)
  | FOR | WHILE | DO | IF | ELIF | ELSE | BREAK | SWITCH | CASE
  | CLASS | EXTENDS | FUN | RETURN | NEW | PRINT | PRINTLN
  | INT_T | CHAR_T | FLOAT_T | BOOL_T | STRING_T | VOID_T  


  (* Boolean literals *)
  | TRUE | FALSE

  (* end of program| invalid Token *)
  | END | INVALID
[@@deriving show]

type token_struct = 
  {
    ttype : token;
    startLine : int;
    endLine : int;
  }
[@@deriving show]

let keywords_map = Base.Map.of_alist_exn (module Base.String) [("for", FOR); ("while", WHILE); ("do", DO); ("if", IF); ("elif", ELIF); ("else", ELSE); ("break", BREAK);
                ("switch", SWITCH); ("case", CASE); ("class", CLASS); ("extends", EXTENDS); ("fun", FUN); ("return", RETURN); 
                ("new", NEW); ("print", PRINT); ("println", PRINTLN); ("int", INT_T); ("char", CHAR_T); ("float", FLOAT_T); 
                ("bool", BOOL_T); ("string", STRING_T); ("void", VOID_T); ("true", TRUE); ("false", FALSE)];;

let print_token tok = Printf.printf "%s\n" (show_token_struct tok);;
let print_token_list tList = List.iter print_token tList;;

(* let print_lexed str =
  let buffer = Lexing.from_string str in
  let d = ref false in
  let s = ref "" in
  while not !d do
    s := show_token (Lexer.read buffer);
    Printf.printf "token: %s\n" !s;
    d := "Token.END" = !s;
  done


let () = print_lexed "x>>=5.5e5"; *)