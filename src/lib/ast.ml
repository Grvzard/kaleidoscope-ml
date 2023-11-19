type binop =
  | Plus
  | Minus
  | Multiply
  | Less

type expr_ =
  | NumberExpr of float
  | VariableExpr of string
  | BinaryExpr of binop * expr_ * expr_
  | CallExpr of string * expr_ list

type prototype_ = Prototype of string * string list
type function_ = Function of prototype_ * expr_
