{
  let id_token_map = Hashtbl.create 16
  let _ =
    List.iter (fun (id, tok) -> Hashtbl.add id_token_map id tok)
              [ "def", Token.Def;
                "extern", Token.Extern;
                "if", Token.If;
                "then", Token.Then;
                "else", Token.Else;
                "for", Token.For;
                "in", Token.In;
                "binary", Token.Binary;
                "Unary", Token.Unary;
              ]
}
(* let number = /-?\d+(\.\d+)?/ *)
let number = ['0'-'9']['0'-'9']* ('.'['0'-'9']+)?
let neg_number = '-' number
let identifier = ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9']*
let white = [' ' '\t' '\r']

rule tokenize = parse
  | white { tokenize lexbuf }
  | '\n' { tokenize lexbuf }
  | '#' { comment lexbuf }
  | '=' { Some Token.Equal }
  | '(' { Some Token.LParen }
  | ')' { Some Token.RParen }
  | ',' { Some Token.Comma }
  | ';' { Some Token.Semicolon }
  | identifier as id {
    match Hashtbl.find_opt id_token_map id with
    | Some t -> Some t
    | None -> Some (Token.Identifier id) }
  | number as n { Some (Token.Number (float_of_string n)) }
  (* | '(' (neg_number as n) ')' { Some (Token.Number (float_of_string n)) } *)
  | '+' { Some Token.Plus }
  | '-' { Some Token.Minus }
  | '*' { Some Token.Star }
  (* | '/' { Some Token.Slash } *)
  | '<' { Some Token.Less }
  | _ as c { Some (Token.AnyChar c) }
  | eof  { Some Token.Eof }

and comment = parse
  | '\n' { tokenize lexbuf }
  | _ { comment lexbuf }
