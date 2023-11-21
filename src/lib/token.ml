type t =
  | Eof
  | Def
  | Extern
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
