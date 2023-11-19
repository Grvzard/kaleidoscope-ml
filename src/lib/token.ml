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
  | Identifier id -> "Identifier<" ^ id ^ ">"
  | Number n -> "Number<" ^ string_of_float n ^ ">"
  | _ -> "etc."
;;
