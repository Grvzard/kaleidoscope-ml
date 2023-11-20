exception ParseFailue

let consume_token (token : Token.t) tokens =
  match Queue.take_opt tokens with
  | Some t -> if t = token then () else raise ParseFailue
  | None -> raise ParseFailue
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
  | Some t ->
    (match t with
     | Token.Number n -> Ast.NumberExpr n
     | _ -> raise ParseFailue)
  | None -> raise ParseFailue

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
   ::= identifier '(' (expression (',' expression)* )* ')' *)
and parse_identifier tokens =
  let open Ast in
  match Queue.take_opt tokens with
  | Some (Token.Identifier id) ->
    (match Queue.peek_opt tokens with
     | Some Token.LParen ->
       let exprs = parse_comma_separated_exprs tokens in
       (match Queue.take_opt tokens with
        | Some Token.RParen -> CallExpr (id, exprs)
        | _ -> raise ParseFailue)
     | _ -> VariableExpr id)
  | _ -> raise ParseFailue

(* primary
   ::= identifierexpr
   ::= numberexpr
   ::= parenexpr *)
and parse_primary tokens =
  match Queue.peek_opt tokens with
  | Some (Token.Identifier _) -> parse_identifier tokens
  | Some (Token.Number _) -> parse_number tokens
  | Some Token.LParen -> parse_paren tokens
  | _ -> raise ParseFailue

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
      | None -> raise ParseFailue)
  | None -> raise ParseFailue

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
  | _ -> raise ParseFailue

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
