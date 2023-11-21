exception ParseFailue of string

let consume_token (token : Token.t) tokens =
  match Queue.take_opt tokens with
  | Some t ->
    if t = token
    then ()
    else
      raise (ParseFailue (Printf.sprintf "Expected '%s'" (Token.string_of_token token)))
  | None ->
    raise (ParseFailue (Printf.sprintf "Expected '%s'" (Token.string_of_token token)))
;;

let binop_precedence token =
  let open Token in
  match token with
  | Less -> 10
  (* | Greater *)
  | Plus -> 20
  | Minus -> 20
  | Star -> 40
  (* | Slash *)
  | _ -> -1
;;

let binop_of_token token =
  let open Token in
  match token with
  | Less -> Ast.Less
  (* | Greater *)
  | Plus -> Ast.Plus
  | Minus -> Ast.Minus
  | Star -> Ast.Multiply
  | _ -> assert false
;;

(* numberexpr ::= number *)
let rec parse_number tokens =
  match Queue.take_opt tokens with
  | Some (Token.Number n) -> Ast.NumberExpr n
  | _ -> assert false

(* parenexpr ::= '(' expression ')' *)
and parse_paren tokens =
  consume_token Token.LParen tokens;
  let expr = parse_expr tokens in
  consume_token Token.RParen tokens;
  expr

and parse_comma_separated_exprs tokens =
  let expr = parse_expr tokens in
  match Queue.peek_opt tokens with
  | Some Token.Comma ->
    ignore (Queue.pop tokens);
    expr :: parse_comma_separated_exprs tokens
  | _ -> [ expr ]

(* identifierexpr
   ::= identifier
   ::= identifier '(' (expression (',' expression)* )? ')' *)
and parse_identifier tokens =
  let open Ast in
  match Queue.take_opt tokens with
  | Some (Token.Identifier id) ->
    (match Queue.peek_opt tokens with
     | Some Token.LParen ->
       (* eat '(' *)
       ignore (Queue.pop tokens);
       let exprs =
         match Queue.peek_opt tokens with
         | Some Token.RParen -> []
         | Some _ -> parse_comma_separated_exprs tokens
         | None -> raise (Failure "Expect more tokens when parsing a function call.")
       in
       consume_token Token.RParen tokens;
       CallExpr (id, exprs)
     | _ -> VariableExpr id)
  | _ -> assert false

(* primary
   ::= identifierexpr
   ::= numberexpr
   ::= parenexpr *)
and parse_primary tokens =
  match Queue.peek_opt tokens with
  | Some (Token.Identifier _) -> parse_identifier tokens
  | Some (Token.Number _) -> parse_number tokens
  | Some Token.LParen -> parse_paren tokens
  | Some Token.If -> parse_if tokens
  | Some Token.For -> parse_for tokens
  | Some t ->
    raise
      (ParseFailue
         (Printf.sprintf
            "unknown token '%s' when expecting an expression."
            (Token.string_of_token t)))
  | None -> raise (ParseFailue "no tokens when expecting an expression")

(* binop_rhs ::= binop primary *)
and parse_binop_rhs prec lhs tokens =
  let open Ast in
  match Queue.peek_opt tokens with
  | Some t ->
    let t_prec = binop_precedence t in
    if t_prec < prec
    then lhs
    else (
      let binop_token = Queue.take tokens in
      let rhs = parse_primary tokens in
      match Queue.peek_opt tokens with
      | Some t2 ->
        let rhs' =
          if t_prec < binop_precedence t2
          then parse_binop_rhs (t_prec + 1) rhs tokens
          else rhs
        in
        BinaryExpr (binop_of_token binop_token, lhs, rhs')
      | None -> raise (ParseFailue "Expect a expression after a binary operator."))
  | None -> raise (ParseFailue "Expect some tokens after a primary epxression.")

(* expression ::= primary (binoprhs)* *)
and parse_expr tokens =
  let lhs = parse_primary tokens in
  parse_binop_rhs 0 lhs tokens

(* prototype ::= id '(' id* ')' *)
and parse_prototype tokens =
  match Queue.take_opt tokens with
  | Some (Token.Identifier id) ->
    consume_token Token.LParen tokens;
    let rec parse_ids tokens =
      match Queue.peek_opt tokens with
      | Some (Token.Identifier id) ->
        ignore (Queue.pop tokens);
        id :: parse_ids tokens
      | _ -> []
    in
    let args = parse_ids tokens in
    consume_token Token.RParen tokens;
    Ast.Prototype (id, args)
  | _ -> raise (ParseFailue "Expected function name in prototype")

(* definition ::= 'def' prototype expression *)
and parse_definition tokens =
  consume_token Token.Def tokens;
  let proto = parse_prototype tokens in
  let e = parse_expr tokens in
  Ast.Function (proto, e)

(* external ::= 'extern' prototype *)
and parse_extern tokens =
  consume_token Token.Extern tokens;
  parse_prototype tokens

(* ifexpr ::= 'if' expression 'then' expression 'else' expression *)
and parse_if tokens =
  consume_token Token.If tokens;
  let cond_ = parse_expr tokens in
  consume_token Token.Then tokens;
  let then_ = parse_expr tokens in
  consume_token Token.Else tokens;
  let else_ = parse_expr tokens in
  Ast.IfExpr (cond_, then_, else_)

(* forexpr ::= 'for' identifier '=' expr ',' expr (',' expr)? 'in' expression *)
and parse_for tokens =
  consume_token Token.For tokens;
  match Queue.take_opt tokens with
  | Some (Token.Identifier id) ->
    consume_token Token.Equal tokens;
    let start_ = parse_expr tokens in
    consume_token Token.Comma tokens;
    let end_ = parse_expr tokens in
    let step_ =
      match Queue.peek_opt tokens with
      | Some Token.Comma ->
        ignore (Queue.pop tokens);
        Some (parse_expr tokens)
      | _ -> None
    in
    consume_token Token.In tokens;
    let body_ = parse_expr tokens in
    Ast.ForExpr (id, start_, end_, step_, body_)
  | _ -> raise (ParseFailue "expected identifier after for")

(* toplevelexpr ::= expression *)
and parse_top_level tokens =
  let e = parse_expr tokens in
  let proto = Ast.Prototype ("", []) in
  Ast.Function (proto, e)

(* top ::= definition | external | expression | ';' *)
and parse_top tokens =
  match Queue.peek_opt tokens with
  | Some Token.Eof -> ()
  | Some Token.Semicolon -> ()
  | Some Token.Def ->
    Llvm.dump_value (Codegen.function_codegen (parse_definition tokens));
    print_endline "Parsed a function definition."
  | Some Token.Extern ->
    Llvm.dump_value (Codegen.prototype_codegen (parse_extern tokens));
    print_endline "Parsed an extern."
  | _ ->
    Llvm.dump_value (Codegen.function_codegen (parse_top_level tokens));
    print_endline "Parsed a top-level expr."
;;
