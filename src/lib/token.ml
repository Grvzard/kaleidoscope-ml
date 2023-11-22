type t =
  | Eof
  | Def
  | Extern
  | If
  | Then
  | Else
  | For
  | In
  | Binary
  | Unary
  | Equal
  | Identifier of string
  | Number of float
  | LParen
  | RParen
  | Comma
  | Semicolon
  | Plus
  | Minus
  | Star
  | Less
  | AnyChar of char

let string_of_token (token : t) =
  match token with
  | Eof -> "eof"
  | Def -> "def"
  | Extern -> "extern"
  | If -> "if"
  | Then -> "then"
  | Else -> "else"
  | For -> "for"
  | In -> "in"
  | Binary -> "binary"
  | Unary -> "unary"
  | Equal -> "="
  | LParen -> "("
  | RParen -> ")"
  | Comma -> ","
  | Semicolon -> ";"
  | Plus -> "+"
  | Minus -> "-"
  | Star -> "*"
  | Less -> "<"
  | AnyChar c -> Printf.sprintf "Char<%c>" c
  | Identifier id -> "Identifier<" ^ id ^ ">"
  | Number n -> "Number<" ^ string_of_float n ^ ">"
;;
