type expr_ =
  | NumberExpr of float
  | VariableExpr of string
  | BinaryExpr of Op_prec.op_t * expr_ * expr_
  | UnaryExpr of Op_prec.op_t * expr_
  | CallExpr of string * expr_ list
  (* cond, then, else *)
  | IfExpr of expr_ * expr_ * expr_
  (* name of 'idx', start, end, step, body *)
  | ForExpr of string * expr_ * expr_ * expr_ option * expr_
  (* (name, initializer) list, body *)
  | VarExpr of (string * expr_ option) list * expr_

(* name, params, precedence (if a binary op) *)
type prototype_ = Prototype of string * string list * int option
type function_ = Function of prototype_ * expr_

let is_unary_op = function
  | Prototype (_name, params, Some _prec) -> List.length params = 1
  | _ -> false
;;

let is_binary_op = function
  | Prototype (_name, params, Some _prec) -> List.length params = 2
  | _ -> false
;;

let get_operator_name = function
  | Prototype (name, params, Some _prec) -> String.get name (List.length params - 1)
  | _ -> assert false
;;

let get_binary_precedence = function
  | Prototype (_name, _params, Some prec) -> prec
  | _ -> assert false
;;
