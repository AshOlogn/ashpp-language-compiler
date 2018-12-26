open Token
open Base

(* append string to another string *)
let (^$) c s = s ^ Char.escaped c;; 

let keywords_map = Map.of_alist_exn (module String) [("for", FOR); ("while", WHILE); ("do", DO); ("if", IF); ("elif", ELIF); ("else", ELSE); ("break", BREAK);
                ("switch", SWITCH); ("case", CASE); ("class", CLASS); ("extends", EXTENDS); ("fun", FUN); ("return", RETURN); 
                ("new", NEW); ("print", PRINT); ("println", PRINTLN); ("int", INT_T); ("char", CHAR_T); ("double", DOUBLE_T); 
                ("bool", BOOL_T); ("string", STRING_T); ("void", VOID_T); ("true", TRUE); ("false", FALSE)];;

(* Exceptions in lexer *)
exception Error_lexer of string;;

let lined_message line msg = Printf.sprintf "line %d: %s" line msg;;

(* lexing function *)
let rec lex_help code index line = 
    let code_len = String.length code in
    if index >= (code_len) then [{ttype = END; startLine = line; endLine = line; }] else
    if Char.is_whitespace code.[index] then 
        (* trim the whitespace *)
        let i = ref index in
        let cLine = ref line in
        while !i < code_len && Char.is_whitespace code.[!i] do begin
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
    | '+'       -> 
        if index+1 < code_len && phys_equal code.[index+1] '=' then 
            { ttype = ADD_EQ; startLine = line; endLine = line; } :: (lex_help code (index+2) line)
        else
            { ttype = ADD; startLine = line; endLine = line; } :: (lex_help code (index+1) line)
    | '-'       -> 
        if index+1 < code_len && phys_equal code.[index+1] '=' then 
            { ttype = SUBTRACT_EQ; startLine = line; endLine = line; } :: (lex_help code (index+2) line)
        else if index+1 < code_len && phys_equal code.[index+1] '>' then 
            { ttype = RIGHTARROW; startLine = line; endLine = line; } :: (lex_help code (index+2) line)
        else
            { ttype = SUBTRACT; startLine = line; endLine = line; } :: (lex_help code (index+1) line)    
    | '*'       ->
        if index+2 < code_len && phys_equal code.[index+1] '*' && phys_equal code.[index+2] '=' then
            { ttype = EXPONENT_EQ; startLine = line; endLine = line; } :: (lex_help code (index+3) line)
        else if index+1 < code_len && phys_equal code.[index+1] '*' then
            { ttype = EXPONENT; startLine = line; endLine = line; } :: (lex_help code (index+2) line)
        else if index+1 < code_len && phys_equal code.[index+1] '=' then
            { ttype = MULTIPLY_EQ; startLine = line; endLine = line; } :: (lex_help code (index+2) line)
        else
            { ttype = MULTIPLY; startLine = line; endLine = line; } :: (lex_help code (index+1) line)
    | '/'       ->
        if index+1 < code_len && phys_equal code.[index+1] '=' then
            { ttype = DIVIDE_EQ; startLine = line; endLine = line; } :: (lex_help code (index+2) line)
        else if index+1 < code_len && phys_equal code.[index+1] '*' then
            (* handle nested multiline comments *)
            let depth = ref 1 in
            let i = ref (index+2) in
            let cLine = ref line in
            while !depth > 0 do begin
                if !i >= code_len then
                    raise (Error_lexer (lined_message !cLine "comment not closed"))
                else if !i+1 < code_len && phys_equal code.[!i] '/' && phys_equal code.[!i+1] '*' then begin
                    depth := !depth+1;
                    i := !i+2;
                    end
                else if !i+1 < code_len && phys_equal code.[!i] '*' && phys_equal code.[!i+1] '/' then begin
                    depth := !depth-1;
                    i := !i+2;
                    end
                else
                    cLine := if phys_equal code.[!i] '\n' then !cLine+1 else !cLine;
                    i := !i+1;
            end done;
            lex_help code !i !cLine
        else 
            { ttype = DIVIDE; startLine = line; endLine = line; } :: (lex_help code (index+1) line)
    | '%'       -> 
        if index+1 < code_len && phys_equal code.[index+1] '=' then
            { ttype = MOD_EQ; startLine = line; endLine = line; } :: (lex_help code (index+2) line)
        else
            { ttype = MOD; startLine = line; endLine = line; } :: (lex_help code (index+1) line)
    | '!'       -> 
        if index+1 < code_len && phys_equal code.[index+1] '=' then
            { ttype = NOT_EQ; startLine = line; endLine = line; } :: (lex_help code (index+2) line)
        else
            { ttype = NOT; startLine = line; endLine = line; } :: (lex_help code (index+1) line)
    | '~'       -> { ttype = BIT_NOT; startLine = line; endLine = line; } :: (lex_help code (index+1) line)
    | '<'       -> 
        if index+2 < code_len && phys_equal code.[index+1] '<' && phys_equal code.[index+2] '=' then
            { ttype = BIT_LEFT_EQ; startLine = line; endLine = line; } :: (lex_help code (index+3) line)
        else if index+1 < code_len && phys_equal code.[index+1] '<' then
            { ttype = BIT_LEFT; startLine = line; endLine = line; } :: (lex_help code (index+2) line)
        else if index+1 < code_len && phys_equal code.[index+1] '=' then
            { ttype = LESS_EQ; startLine = line; endLine = line; } :: (lex_help code (index+2) line)
        else
            { ttype = LESS; startLine = line; endLine = line; } :: (lex_help code (index+1) line)
    | '>'       -> 
        if index+2 < code_len && phys_equal code.[index+1] '>' && phys_equal code.[index+2] '=' then
            { ttype = BIT_RIGHT_EQ; startLine = line; endLine = line; } :: (lex_help code (index+3) line)
        else if index+1 < code_len && phys_equal code.[index+1] '>' then
            { ttype = BIT_RIGHT; startLine = line; endLine = line; } :: (lex_help code (index+2) line)
        else if index+1 < code_len && phys_equal code.[index+1] '=' then
            { ttype = GREATER_EQ; startLine = line; endLine = line; } :: (lex_help code (index+2) line)
        else
            { ttype = GREATER; startLine = line; endLine = line; } :: (lex_help code (index+1) line)
    | '='       ->
        if index+1 < code_len && phys_equal code.[index+1] '=' then
            { ttype = EQ_EQ; startLine = line; endLine = line; } :: (lex_help code (index+2) line)
        else
            { ttype = EQ; startLine = line; endLine = line; } :: (lex_help code (index+1) line)
    | '&'       ->
        if index+1 < code_len && phys_equal code.[index+1] '&' then
            { ttype = AND; startLine = line; endLine = line; } :: (lex_help code (index+2) line)
        else if index+1 < code_len && phys_equal code.[index+1] '=' then
            { ttype = AND_EQ; startLine = line; endLine = line; } :: (lex_help code (index+2) line)
        else
            { ttype = BIT_AND; startLine = line; endLine = line; } :: (lex_help code (index+1) line)            
    | '^'       ->
        if index+1 < code_len && phys_equal code.[index+1] '^' then
            { ttype = XOR; startLine = line; endLine = line; } :: (lex_help code (index+2) line)
        else if index+1 < code_len && phys_equal code.[index+1] '=' then
            { ttype = XOR_EQ; startLine = line; endLine = line; } :: (lex_help code (index+2) line)
        else
            { ttype = BIT_XOR; startLine = line; endLine = line; } :: (lex_help code (index+1) line)            
    | '|'       ->
        if index+1 < code_len && phys_equal code.[index+1] '|' then
            { ttype = OR; startLine = line; endLine = line; } :: (lex_help code (index+2) line)
        else if index+1 < code_len && phys_equal code.[index+1] '=' then
            { ttype = OR_EQ; startLine = line; endLine = line; } :: (lex_help code (index+2) line)
        else
            { ttype = BIT_OR; startLine = line; endLine = line; } :: (lex_help code (index+1) line)            

    (* string literals *)
    | '"'       -> 
        let i = ref (index+1) in
        let cLine = ref line in
        let str = ref "" in

        (* Include some hacky bounds checking here *)
        if !i = code_len then
           raise (Error_lexer (lined_message !cLine "string literal not closed with \""));
        while not (phys_equal code.[!i] '"') do begin
            (* Update line number in source if new line is found *)
            cLine := if (phys_equal code.[!i] '\n') then !cLine+1 else !cLine;
            str := !str ^ (Char.to_string code.[!i]);
            i := !i + 1;

            if !i = code_len then
                raise (Error_lexer (lined_message !cLine "string literal not closed with \""));
        end done;
        { ttype = STRING !str; startLine = line; endLine = !cLine; } :: (lex_help code !i !cLine)

    (* number literals*)
    | '0' .. '9' -> 
        let i = ref index in
        let num_decimals = ref 0 in
        let literal = ref "" in
        while (!i < code_len) && (phys_equal code.[!i] '.' || Char.is_digit code.[!i]) do begin
            (* Increment so we know it's a double *)
            if phys_equal code.[!i] '.' then
                (* If we already have a decimal, error *)
                if !num_decimals > 0 then
                    raise (Error_lexer (lined_message line "number can have at most 1 decimal point"))
                else
                    num_decimals := 1; 
                
            num_decimals := if (phys_equal code.[!i] '.') then !num_decimals+1 else !num_decimals; 
            literal := !literal ^ (Char.to_string code.[!i]);
            i := !i + 1;
        end done;
        if (phys_equal !num_decimals 0) then 
            { ttype = INT (Int.of_string !literal); startLine = line; endLine = line; } :: (lex_help code !i line)
        else 
            { ttype = DOUBLE (Float.of_string !literal); startLine = line; endLine = line; } :: (lex_help code !i line)
    
    (*identifiers and keywords *)
    | 'a'..'z' | 'A'..'Z' | '_' -> 
        let i = ref index in
        let str = ref "" in
        while (!i < code_len) && (phys_equal code.[!i] '_' || Char.is_alphanum code.[!i]) do begin  
            str := !str ^ (Char.to_string code.[!i]);
            i := !i + 1;
        end done;
        let ttype = match Map.find keywords_map !str with
                    | Some opt  -> opt
                    | None      -> VARIABLE !str
        in {ttype = ttype; startLine = line; endLine = line }:: (lex_help code !i line) 

    | _ -> raise (Error_lexer (lined_message line "undefined character found" ))