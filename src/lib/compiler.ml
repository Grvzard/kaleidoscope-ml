open Llvm_target

exception KCompileFailure

type t = { tm : TargetMachine.t }

let create () =
  let target_triple = Target.default_triple () in
  Llvm_all_backends.initialize ();
  let target = Target.by_triple target_triple in
  let tm = TargetMachine.create ~triple:target_triple ~cpu:"generic" target in
  { tm }
;;

let compile_file file =
  let compiler_ = create () in
  let gen = Codegen.create ~modulename:file compiler_.tm in
  Codegen.config_fpm gen;
  let ic = open_in file in
  let lexbuf = Lexing.from_channel ic in
  let tokens = Queue.create () in
  let rec lex lexbuf tokens =
    match Lexer.tokenize lexbuf with
    | None -> ()
    | Some token ->
      (* print_string (Token.to_string token); *)
      (* print_string "\n"; *)
      Queue.push token tokens;
      if token = Token.Eof then () else lex lexbuf tokens
  in
  lex lexbuf tokens;
  let rec parse gen tokens =
    match Queue.peek_opt tokens with
    | Some Token.Eof -> ()
    | Some Token.Def ->
      ignore (Codegen.function_codegen gen (Parser.parse_definition tokens));
      parse gen tokens
    | Some Token.Extern ->
      ignore (Codegen.prototype_codegen gen (Parser.parse_extern tokens));
      parse gen tokens
    | _ -> raise KCompileFailure
  in
  parse gen tokens;
  (* Llvm.dump_module gen.module_; *)
  Codegen.dump_obj_to_file gen (file ^ ".o")
;;
