let rec lex lexbuf tokens =
  match Lexer.tokenize lexbuf with
  | None -> ()
  | Some token ->
    (* print_string (Token.to_string token); *)
    (* print_string "\n"; *)
    Queue.push token tokens;
    if token = Token.Semicolon then () else lex lexbuf tokens
;;

let repl =
  while true do
    print_string "> ";
    flush stdout;
    let lexbuf = Lexing.from_channel stdin in
    let tokens = Queue.create () in
    lex lexbuf tokens;
    try Parser.parse_top (Queue.copy tokens) with
    | Parser.ParseFailure e ->
      print_endline e;
      print_endline "[Tokens dump]:";
      Queue.iter (fun t -> print_endline (Token.string_of_token t)) tokens
    | Failure e -> print_endline e
  done
;;
