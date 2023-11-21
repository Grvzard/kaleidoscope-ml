type t =
  | Eof
  | Def
  | Extern
  | If
  | Then
  | Else
  | For
  | In
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
  | Equal -> "="
  | LParen -> "("
  | RParen -> ")"
  | Comma -> ","
  | Semicolon -> ";"
  | Plus -> "+"
  | Minus -> "-"
  | Star -> "*"
  | Less -> "<"
  | Identifier id -> "Identifier<" ^ id ^ ">"
  | Number n -> "Number<" ^ string_of_float n ^ ">"
  (* | _ -> "{unnamed token}" *)
;;
