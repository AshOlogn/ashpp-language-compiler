open Token

(* TODO: char literal, += stuff, exponent, string literals, numbers, identifiers, bounds checking, EFFICIENCY *)
let (^$) c s = s ^ Char.escaped c (* append *)

let rec lex_help code index line = 
    match code.[index] with
    | '('       -> { ttype = LEFT_PAREN; startLine = line; endLine = line; } :: (lex_help code (index+1) line)
    | ')'       -> { ttype = RIGHT_PAREN; startLine = line; endLine = line; } :: (lex_help code (index+1) line)
    | '['       -> { ttype = LEFT_BRACKET; startLine = line; endLine = line; } :: (lex_help code (index+1) line)
    | ']'       -> { ttype = RIGHT_BRACKET; startLine = line; endLine = line; } :: (lex_help code (index+1) line)
    | '{'       -> { ttype = LEFT_BRACE; startLine = line; endLine = line; } :: (lex_help code (index+1) line)
    | '}'       -> { ttype = RIGHT_BRACE; startLine = line; endLine = line; } :: (lex_help code (index+1) line)
    | '.'       -> { ttype = PERIOD; startLine = line; endLine = line; } :: (lex_help code (index+1) line)
    | '?'       -> { ttype = QUESTION; startLine = line; endLine = line; } :: (lex_help code (index+1) line)
    | ':'       -> { ttype = COLON; startLine = line; endLine = line; } :: (lex_help code (index+1) line)
    | ';'       -> { ttype = SEMICOLON; startLine = line; endLine = line; } :: (lex_help code (index+1) line)
    | ','       -> { ttype = COMMA; startLine = line; endLine = line; } :: (lex_help code (index+1) line)
    | '+'       -> { ttype = ADD; startLine = line; endLine = line; } :: (lex_help code (index+1) line)
    | '-'       -> { ttype = SUBTRACT; startLine = line; endLine = line; } :: (lex_help code (index+1) line)
    | '*'       -> { ttype = MULTIPLY; startLine = line; endLine = line; } :: (lex_help code (index+1) line)
    | '/'       -> { ttype = DIVIDE; startLine = line; endLine = line; } :: (lex_help code (index+1) line)

    (* parse string literals *)
    (* | '"'       -> 
        let i = index+1 in
        let str = ref "" in
          *)


    (* parse number literals and identifiers now *)
    |  _        -> []