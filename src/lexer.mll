{
open Token
exception Error_lexer of string

let parse_id str = match Base.Map.find keywords_map str with
              | Some opt  -> opt
              | None      -> VARIABLE str
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
  | "==" { EQ_EQ }
  | '='  { EQ }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float { DOUBLE (float_of_string (Lexing.lexeme lexbuf)) }
  | string { STRING (Lexing.lexeme lexbuf) }
  | id { parse_id (Lexing.lexeme lexbuf) }
  | _ { raise (Error_lexer ("Invalid character found: " ^ Lexing.lexeme lexbuf)) }
  | eof { END }  

