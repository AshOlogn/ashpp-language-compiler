{
open Parser
exception Error_lexer of string
}

(* Regular expressions for different patterns *)
let digit = ['0'-'9']
let int = digit+
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit '.' int exp?

(* hacky, will fix later *)
let string = '"' ['0'-'9' 'a'-'z' 'A'-'Z' '!' '@' '#' '$' '%' '^' '&' '*' '(' ')' '-' '_' '+' '=' '\n' '\r' '\t' '{' '}' '[' ']']* '"'

(* Borrowed these from the online book Real World OCaml *)
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read = parse
  | white { read lexbuf }
  | '('  { LEFT_PAREN }
  | ')'  { RIGHT_PAREN }
  | '['  { LEFT_BRACKET }
  | ']'  { RIGHT_BRACKET }
  | '{'  { LEFT_BRACE }
  | '}'  { RIGHT_BRACE }
  | '.'  { PERIOD }
  | '?'  { QUESTION }
  | ':'  { COLON }
  | ';'  { SEMICOLON }
  | ','  { COMMA }
  | "+=" { ADD_EQ }
  | '+'  { ADD }
  | "-=" { SUBTRACT_EQ }
  | "->" { RIGHTARROW }
  | '-'  { SUBTRACT }
  | "**=" { EXPONENT_EQ }
  | "**" { EXPONENT }
  | "*=" { MULTIPLY_EQ }
  | '*'  { MULTIPLY }
  | "%=" { MOD_EQ }
  | '%'  { MOD }
  | "!=" { NOT_EQ }
  | '!'  { NOT }
  | '~'  { BIT_NOT }
  | "<<=" { BIT_LEFT_EQ }
  | "<<" { BIT_LEFT }
  | "<=" { LESS_EQ }
  | '<'  { LESS }
  | ">>=" { BIT_RIGHT_EQ }
  | ">>" { BIT_RIGHT }
  | ">=" { GREATER_EQ }
  | '>'  { GREATER }
  | "==" { EQ_EQ }
  | '='  { EQ }
  | "&&" { AND }
  | "&=" { AND_EQ }
  | '&'  { BIT_AND }
  | "^^" { XOR }
  | "^=" { XOR_EQ }
  | '^'  { BIT_XOR }
  | "||" { OR }
  | "|=" { OR_EQ }
  | '|'  { BIT_OR }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float { DOUBLE (float_of_string (Lexing.lexeme lexbuf)) }
  | string { STRING (Lexing.lexeme lexbuf) }
  | "for" { FOR } | "while" { WHILE } | "do" { DO } | "if" { IF } | "elif" { ELIF } | "else" { ELSE } | "break" { BREAK }
  | "switch" { SWITCH } | "case" { CASE } | "class" { CLASS } | "extends" { EXTENDS } | "fun" { FUN } | "return" { RETURN }
  | "new" { NEW } | "print" { PRINT } | "println" { PRINTLN } | "int" { INT_T } | "char" { CHAR_T } | "float" { FLOAT_T }
  | "bool" { BOOL_T } | "string" { STRING_T } | "void" { VOID_T } | "true" { TRUE } | "false" { FALSE }
  | id { VARIABLE (Lexing.lexeme lexbuf) }
  | _ { raise (Error_lexer ("Invalid character found: " ^ Lexing.lexeme lexbuf)) }
  | eof { END }