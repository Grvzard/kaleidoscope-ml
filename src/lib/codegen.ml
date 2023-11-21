open Ast
open Llvm
open Llvm_analysis

exception CodegenFailure of string

let context_ = global_context ()
let builder_ = builder context_
let module_ = create_module context_ "TheModule"
let named_tuple : (string, llvalue) Hashtbl.t = Hashtbl.create ~random:true 8
let llt_double = double_type context_

let proto_args_check params_arr params_list =
  if Array.length params_arr <> List.length params_list
  then false
  else
    Array.for_all2
      (fun p1 p2 -> value_name p1 = p2)
      params_arr
      (Array.of_list params_list)
;;

let rec expr_codegen = function
  | NumberExpr n -> const_float llt_double n
  | VariableExpr varname ->
    (match Hashtbl.find_opt named_tuple varname with
     | Some var -> var
     | None -> raise (CodegenFailure "Unknown variable name"))
  | BinaryExpr (op, lhs, rhs) ->
    let l = expr_codegen lhs in
    let r = expr_codegen rhs in
    (match op with
     | Plus -> build_fadd l r "addtmp" builder_
     | Minus -> build_fsub l r "subtmp" builder_
     | Multiply -> build_fmul l r "multmp" builder_
     | Less ->
       let l' = build_fcmp Fcmp.Ult l r "cmptmp" builder_ in
       build_uitofp l' llt_double "booltmp" builder_)
  | CallExpr (callee, args) ->
    (match lookup_function callee module_ with
     | None -> raise (CodegenFailure "Unknown function referenced")
     | Some f ->
       if Array.length (params f) <> List.length args
       then raise (CodegenFailure "Incorrect # arguments passed")
       else (
         let args_arr = Array.of_list (List.map expr_codegen args) in
         build_call f args_arr "calltmp" builder_))
;;

let prototype_codegen = function
  | Prototype (name, args) ->
    let doubles = Array.make (List.length args) llt_double in
    let ft = function_type llt_double doubles in
    let f = declare_function name ft module_ in
    let assign_name argnames arg =
      match argnames with
      | name :: ls ->
        set_value_name name arg;
        ls
      | [] -> assert false
    in
    ignore (fold_left_params assign_name args f);
    f
;;

let function_codegen = function
  | Function ((Prototype (name, args) as proto), body) ->
    let llfunc_ =
      match lookup_function name module_ with
      | Some llfunc_ ->
        if proto_args_check (params llfunc_) args
        then llfunc_
        else raise (CodegenFailure "Unknown variable name.")
      | None -> prototype_codegen proto
    in
    if Array.length (basic_blocks llfunc_) <> 0
    then raise (Failure "Function cannot be redefined.")
    else (
      let bb = append_block context_ "entry" llfunc_ in
      position_at_end bb builder_;
      Hashtbl.clear named_tuple;
      Array.iter (fun p -> Hashtbl.replace named_tuple (value_name p) p) (params llfunc_);
      try
        let ret_val = expr_codegen body in
        ignore (build_ret ret_val builder_);
        if verify_function llfunc_
        then llfunc_
        else raise (CodegenFailure "function verification failed")
      with
      | e ->
        dump_value (llfunc_);
        delete_function llfunc_;
        raise e)
;;
