open Ast
open Llvm
open Llvm_analysis

exception CodegenFailure of string

let context_ = global_context ()
let builder_ = builder context_
let module_ = create_module context_ "TheModule"
let named_tuple : (string, llvalue) Hashtbl.t = Hashtbl.create ~random:true 8
let llt_double = double_type context_
let llv_double_0 = const_float llt_double 0.0
let llv_double_1 = const_float llt_double 1.0

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
  | IfExpr (cond_, then_, else_) ->
    let cond_v = ref (expr_codegen cond_) in
    cond_v := build_fcmp Fcmp.One !cond_v llv_double_0 "ifcond" builder_;
    let the_func = block_parent (insertion_block builder_) in
    let then_bb = ref (append_block context_ "then" the_func) in
    let else_bb = ref (append_block context_ "else" the_func) in
    let merge_bb = append_block context_ "ifcont" the_func in
    ignore merge_bb;
    ignore (build_cond_br !cond_v !then_bb !else_bb builder_);
    position_at_end !then_bb builder_;
    let then_v = expr_codegen then_ in
    ignore (build_br merge_bb builder_);
    let then_bb' = insertion_block builder_ in
    position_at_end !else_bb builder_;
    let else_v = expr_codegen else_ in
    ignore (build_br merge_bb builder_);
    let else_bb' = insertion_block builder_ in
    position_at_end merge_bb builder_;
    let pn = build_empty_phi llt_double "iftmp" builder_ in
    add_incoming (then_v, then_bb') pn;
    add_incoming (else_v, else_bb') pn;
    pn
  | ForExpr (idxname, start_, end_, step_, body_) ->
    let start_v = expr_codegen start_ in
    let the_func = block_parent (insertion_block builder_) in
    let preheader_bb = insertion_block builder_ in
    let loop_bb = append_block context_ "loop" the_func in
    ignore (build_br loop_bb builder_);
    position_at_end loop_bb builder_;
    let idx_pn = build_empty_phi llt_double idxname builder_ in
    add_incoming (start_v, preheader_bb) idx_pn;
    (* remember the Value that we are potentially shadowing *)
    let old_idx = Hashtbl.find_opt named_tuple idxname in
    Hashtbl.replace named_tuple idxname idx_pn;
    ignore (expr_codegen body_);
    let step_v =
      match step_ with
      | Some s -> expr_codegen s
      | None -> llv_double_1
    in
    let next_var = build_fadd idx_pn step_v "nextvar" builder_ in
    let end_v = expr_codegen end_ in
    let end_v' = build_fcmp Fcmp.One end_v llv_double_0 "loopcond" builder_ in
    let loopend_bb = insertion_block builder_ in
    let after_bb = append_block context_ "afterloop" the_func in
    ignore (build_cond_br end_v' loop_bb after_bb builder_);
    position_at_end after_bb builder_;
    add_incoming (next_var, loopend_bb) idx_pn;
    (* OCaml stdlib Hashtbl has the potential ability of shadowing *)
    (* but we chose to make the code more like the origin cpp one *)
    (match old_idx with
     | Some v -> Hashtbl.replace named_tuple idxname v
     | None -> Hashtbl.remove named_tuple idxname);
    llv_double_0 (* always returns 0.0 *)
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
        dump_value llfunc_;
        delete_function llfunc_;
        raise e)
;;
