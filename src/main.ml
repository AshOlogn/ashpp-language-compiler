open Token

let print_lexed str =
  let buffer = Lexing.from_string str in
  let d = ref false in
  let s = ref "" in
  while not !d do
    s := show_token (Lexer.read buffer);
    Printf.printf "token: %s\n" !s;
    d := "Token.END" = !s;
  done


let () = print_lexed "x = 5.5e5";