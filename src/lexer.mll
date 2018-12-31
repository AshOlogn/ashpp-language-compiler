{
  open Lexing
  open Parser
  exception Error_lexer of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
                pos_lnum = pos.pos_lnum + 1
      }
}

(* Regular expressions for different patterns *)
let digit = ['0'-'9']
let int = digit+
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit '.' int exp?

(* Borrowed these from the online book Real World OCaml *)
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let comment = "/*"
let end_comment = "*/"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read = parse
  | white { read lexbuf }
  | newline { next_line lexbuf; read lexbuf }
  | comment { read_comment 1 lexbuf}
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
  | '"' { read_string (Buffer.create 100) lexbuf }
  | "for" { FOR } | "while" { WHILE } | "do" { DO } | "if" { IF } | "elif" { ELIF } | "else" { ELSE } | "break" { BREAK }
  | "switch" { SWITCH } | "case" { CASE } | "class" { CLASS } | "extends" { EXTENDS } | "fun" { FUN } | "return" { RETURN }
  | "new" { NEW } | "print" { PRINT } | "println" { PRINTLN } | "int" { INT_T } | "char" { CHAR_T } | "float" { FLOAT_T }
  | "bool" { BOOL_T } | "string" { STRING_T } | "void" { VOID_T } | "true" { TRUE } | "false" { FALSE }
  | id { VARIABLE (Lexing.lexeme lexbuf) }
  | _ { raise (Error_lexer ("Invalid character found: " ^ Lexing.lexeme lexbuf)) }
  | eof { END }

and read_comment depth = parse
  | comment { read_comment (depth+1) lexbuf }
  | end_comment { if depth = 1 then read lexbuf else read_comment (depth-1) lexbuf }
  | newline { next_line lexbuf; read_comment depth lexbuf }
  | _ { read_comment depth lexbuf }
  | eof { raise (Error_lexer ("Unclosed comment, reached end-of-file")) }

(* Borrowed from an excerpt in Real World OCaml *)
and read_string buf = parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (Error_lexer ("Invalid string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (Error_lexer ("String is not terminated, reached end-of-file")) }