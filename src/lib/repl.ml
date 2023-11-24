let rec lex lexbuf tokens =
  match Lexer.tokenize lexbuf with
  | None -> ()
  | Some token ->
    (* print_string (Token.to_string token); *)
    (* print_string "\n"; *)
    Queue.push token tokens;
    if token = Token.Semicolon then () else lex lexbuf tokens
;;

(* top ::= definition | external | expression | ';' *)
let parse_top tokens =
  let open Parser in
  let compiler_ = Compiler.create () in
  let gen = Codegen.create compiler_.tm in
  match Queue.peek_opt tokens with
  | Some Token.Eof -> ()
  | Some Token.Semicolon -> ()
  | Some Token.Def ->
    Llvm.dump_value (Codegen.function_codegen gen (parse_definition tokens));
    print_endline "Parsed a function definition."
  | Some Token.Extern ->
    Llvm.dump_value (Codegen.prototype_codegen gen (parse_extern tokens));
    print_endline "Parsed an extern."
  | _ ->
    Llvm.dump_value (Codegen.function_codegen gen (parse_top_level tokens));
    print_endline "Parsed a top-level expr."
;;

let repl () =
  while true do
    print_string "> ";
    flush stdout;
    let lexbuf = Lexing.from_channel stdin in
    let tokens = Queue.create () in
    lex lexbuf tokens;
    try parse_top (Queue.copy tokens) with
    | Parser.ParseFailure e ->
      print_endline e;
      print_endline "[Tokens dump]:";
      Queue.iter (fun t -> print_endline (Token.string_of_token t)) tokens
    | Failure e -> print_endline e
  done
;;
