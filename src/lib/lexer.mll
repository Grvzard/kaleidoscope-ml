(* let number = /-?\d+(\.\d+)?/ *)
let number = ['0'-'9']['0'-'9']* ('.'['0'-'9']+)?
let neg_number = '-' number
let identifier = ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9']*
let white = [' ' '\t' '\r']

rule tokenize = parse
  | white { tokenize lexbuf }
  | '\n' { tokenize lexbuf }
  | '#' { comment lexbuf }
  | "def" { Some Token.Def }
  | "extern" { Some Token.Extern }
  | '(' { Some Token.LParen }
  | ')' { Some Token.RParen }
  | ',' { Some Token.Comma }
  | ';' { Some Token.Semicolon }
  | identifier as id { Some (Token.Identifier id) }
  | number as n { Some (Token.Number (float_of_string n)) }
  (* | '(' (neg_number as n) ')' { Some (Token.Number (float_of_string n)) } *)
  | '+' { Some Token.Plus }
  | '-' { Some Token.Minus }
  | '*' { Some Token.Star }
  (* | '/' { Some Token.Slash } *)
  | '<' { Some Token.Less }
  | eof  { Some Token.Eof }

and comment = parse
  | '\n' { tokenize lexbuf }
  | _ { comment lexbuf }
