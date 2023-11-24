type op_t =
  | OpC of char
  | OpS of string

let default_precedence = 30
let binop_precedence_map : (op_t, int) Hashtbl.t = Hashtbl.create 8

let () =
  List.iter
    (fun (tok, prec) -> Hashtbl.add binop_precedence_map tok prec)
    [ OpC '=', 2; OpC '<', 10; OpC '+', 20; OpC '-', 20; OpC '*', 40 ]
;;

let get (op : op_t) =
  match Hashtbl.find_opt binop_precedence_map op with
  | Some prec -> prec
  | None -> -1
;;

let set (op : op_t) (prec : int) =
  match op with
  | OpC _ -> Hashtbl.replace binop_precedence_map op prec
  | _ -> assert false
;;
