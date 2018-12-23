open Token
open Base

(* TODO: char literal, += stuff, exponent, string literals, numbers, identifiers, bounds checking, EFFICIENCY *)

(* Helper functions and variables *)
let (^$) c s = s ^ Char.escaped c (* append *)
let keywords = [("for", FOR); ("while", WHILE); ("do", DO); ("if", IF); ("elif", ELIF); ("else", ELSE); ("break", BREAK);
                ("switch", SWITCH); ("case", CASE); ("class", CLASS); ("extends", EXTENDS); ("fun", FUN); ("return", RETURN); 
                ("new", NEW); ("print", PRINT); ("println", PRINTLN); ("int", INT_T); ("char", CHAR_T); ("double", DOUBLE_T); 
                ("bool", BOOL_T); ("string", STRING_T); ("void", VOID_T); ("true", TRUE); ("false", FALSE)];;

let rec string_to_token_helper list str =
    match list with
    | (s, tok) :: xs   -> if String.equal str s then tok else (string_to_token_helper xs str)
    | []               -> VARIABLE str

let string_to_token str = string_to_token_helper keywords str

let is_single_char_token c  = 
    match c with
    | '+' | '-' | '*' | '/' | '%' | '&' | '|' | '^' | '!' | '=' | '<' | '>'
    | '?' | ':' | '.' | ',' | ';' | '(' | ')' | '[' | ']' | '{'
    | '}' -> true
    |  _  -> false

let rec lex_help code index line = 
    if index >= (String.length code) then [{ttype = END; startLine = line; endLine = line; }] else
    if Char.is_whitespace code.[index] then 
        (* trim the whitespace *)
        let i = ref index in
        let cLine = ref line in
        while !i < String.length code && Char.is_whitespace code.[!i] do begin
            cLine := if (phys_equal code.[!i] '\n') then !cLine + 1 else !cLine;
            i := !i + 1;
        end done;
        (lex_help code !i !cLine)
    else match code.[index] with
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
    | '%'       -> { ttype = MOD; startLine = line; endLine = line; } :: (lex_help code (index+1) line)
    | '!'       -> { ttype = NOT; startLine = line; endLine = line; } :: (lex_help code (index+1) line)
    | '~'       -> { ttype = BIT_NOT; startLine = line; endLine = line; } :: (lex_help code (index+1) line)
    | '<'       -> { ttype = LESS; startLine = line; endLine = line; } :: (lex_help code (index+1) line)
    | '>'       -> { ttype = GREATER; startLine = line; endLine = line; } :: (lex_help code (index+1) line)
    | '='       -> { ttype = EQ; startLine = line; endLine = line; } :: (lex_help code (index+1) line)
    | '&'       -> { ttype = BIT_AND; startLine = line; endLine = line; } :: (lex_help code (index+1) line)
    | '^'       -> { ttype = BIT_XOR; startLine = line; endLine = line; } :: (lex_help code (index+1) line)
    | '|'       -> { ttype = BIT_OR; startLine = line; endLine = line; } :: (lex_help code (index+1) line)

    (* string literals *)
    | '"'       -> 
        let i = ref (index+1) in
        let cLine = ref line in
        let str = ref "" in
        while not (phys_equal code.[!i] '"') do begin
            (* Update line number in source if new line is found *)
            cLine := if (phys_equal code.[!i] '\n') then !cLine+1 else !cLine;
            str := !str ^ (Char.to_string code.[!i]);
            i := !i + 1;
        end done;
        { ttype = STRING !str; startLine = line; endLine = !cLine; } :: (lex_help code (!i+1) !cLine)

    (* number literals*)
    | '0' .. '9' -> 
        let i = ref index in
        let num_decimals = ref 0 in
        let literal = ref "" in
        while (!i < String.length code) && not (phys_equal code.[!i] ' ') do begin
            (* Increment so we know it's a double *)
            num_decimals := if (phys_equal code.[!i] '.') then !num_decimals+1 else !num_decimals; 
            literal := !literal ^ (Char.to_string code.[!i]);
            i := !i + 1;
        end done;
        if (phys_equal !num_decimals 0) then 
            { ttype = INT (Int.of_string !literal); startLine = line; endLine = line; } :: (lex_help code (!i+1) line)
        else 
            { ttype = DOUBLE (Float.of_string !literal); startLine = line; endLine = line; } :: (lex_help code (!i+1) line)
    
    (*identifiers and keywords *)
    | 'a'..'z' | 'A'..'Z' | '\'' | '_' -> 
        let i = ref index in
        let str = ref "" in
        while (!i < String.length code) && (phys_equal code.[!i] '_' || Char.is_alphanum code.[!i]) do begin  
            str := !str ^ (Char.to_string code.[!i]);
            i := !i + 1;
        end done;
        {ttype = string_to_token !str; startLine = line; endLine = line }:: (lex_help code (!i+1) line) 

    | _ -> []