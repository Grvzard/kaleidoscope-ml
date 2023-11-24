open Ast
open Llvm
open Llvm_analysis

exception CodegenFailure of string

let context_ = global_context ()
let builder_ = builder context_
let module_ = create_module context_ "TheModule"

let fpm_ =
  let fpm = PassManager.create_function module_ in
  (* this pass always cause Segmentation fault. weired. *)
  (* Llvm_scalar_opts.add_instruction_combination fpm; *)
  Llvm_scalar_opts.add_memory_to_register_promotion fpm;
  Llvm_scalar_opts.add_reassociation fpm;
  Llvm_scalar_opts.add_gvn fpm;
  Llvm_scalar_opts.add_cfg_simplification fpm;
  fpm
;;

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

let create_entry_block_alloca f varname =
  let tmp_builder = builder_at context_ (instr_begin (entry_block f)) in
  build_alloca llt_double varname tmp_builder
;;

let rec expr_codegen = function
  | NumberExpr n -> const_float llt_double n
  | VariableExpr varname ->
    (match Hashtbl.find_opt named_tuple varname with
     | Some alloca_inst -> build_load alloca_inst varname builder_
     | None -> raise (CodegenFailure ("Unknown variable name" ^ varname)))
  | BinaryExpr (op, lhs, rhs) ->
    if op = Op_prec.OpC '='
    then (
      match lhs with
      | VariableExpr varname ->
        let val_ = expr_codegen rhs in
        let var_alloca =
          match Hashtbl.find_opt named_tuple varname with
          | Some v -> v
          | None -> raise (CodegenFailure ("Unknown variable name: " ^ varname))
        in
        ignore (build_store val_ var_alloca builder_);
        val_
      | _ -> raise (CodegenFailure "destination of '=' must be a variable"))
    else (
      let l = expr_codegen lhs in
      let r = expr_codegen rhs in
      match op with
      | Op_prec.OpS _ -> assert false
      | Op_prec.OpC '+' -> build_fadd l r "addtmp" builder_
      | Op_prec.OpC '-' -> build_fsub l r "subtmp" builder_
      | Op_prec.OpC '*' -> build_fmul l r "multmp" builder_
      | Op_prec.OpC '<' ->
        let l' = build_fcmp Fcmp.Ult l r "cmptmp" builder_ in
        build_uitofp l' llt_double "booltmp" builder_
      | Op_prec.OpC c ->
        (match lookup_function (Utils.cat_str_char "binary" c) module_ with
         | Some llfunc_ -> build_call llfunc_ [| l; r |] "binop" builder_
         (* | None -> raise (CodegenFailure "binary operator not found!"))) *)
         | None -> assert false))
  | UnaryExpr (OpS _, _) -> assert false
  | UnaryExpr (OpC c, operand) ->
    let operand_v = expr_codegen operand in
    (match lookup_function (Utils.cat_str_char "unary" c) module_ with
     | Some llfunc_ -> build_call llfunc_ [| operand_v |] "unop" builder_
     (* | None -> raise (CodegenFailure "Unknown unary operator"))) *)
     | None -> assert false)
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
    let the_func = block_parent (insertion_block builder_) in
    (* at entry block *)
    let alloca = create_entry_block_alloca the_func idxname in
    let start_v = expr_codegen start_ in
    ignore (build_store start_v alloca builder_);
    (* append loop block *)
    let loop_bb = append_block context_ "loop" the_func in
    ignore (build_br loop_bb builder_);
    position_at_end loop_bb builder_;
    (* at loop block *)

    (* let old_idx = Hashtbl.find_opt named_tuple idxname in
       Hashtbl.replace named_tuple idxname idx_pn; *)
    ignore (expr_codegen body_);
    let step_v =
      match step_ with
      | Some s -> expr_codegen s
      | None -> llv_double_1
    in
    let end_v =
      build_fcmp Fcmp.One (expr_codegen end_) llv_double_0 "loopcond" builder_
    in
    let curr_idx = build_load alloca idxname builder_ in
    let next_idx = build_fadd curr_idx step_v "nextvar" builder_ in
    ignore (build_store next_idx alloca builder_);
    (* build_cond_br *)
    let after_bb = append_block context_ "afterloop" the_func in
    ignore (build_cond_br end_v loop_bb after_bb builder_);
    position_at_end after_bb builder_;
    (* always returns 0.0 *)
    llv_double_0
  | VarExpr (var_pairs, body) ->
    let the_func = block_parent (insertion_block builder_) in
    List.iter
      (fun (name, init) ->
        let init_v =
          match init with
          | Some init_e -> expr_codegen init_e
          | None -> llv_double_0
        in
        let alloca = create_entry_block_alloca the_func name in
        ignore (build_store init_v alloca builder_);
        (* OCaml stdlib Hashtbl is good enough for shadowing *)
        Hashtbl.add named_tuple name alloca)
      var_pairs;
    let body_v = expr_codegen body in
    List.iter (fun (name, _init) -> Hashtbl.remove named_tuple name) var_pairs;
    body_v
;;

let prototype_codegen = function
  | Prototype (name, args, _) ->
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
  | Function ((Prototype (name_, params_, _prec_opt) as proto), body) ->
    let llfunc_ =
      match lookup_function name_ module_ with
      | Some llfunc_ ->
        if proto_args_check (params llfunc_) params_
        then llfunc_
        else raise (CodegenFailure "Unknown variable name.")
      | None -> prototype_codegen proto
    in
    if Array.length (basic_blocks llfunc_) <> 0
    then raise (Failure "Function cannot be redefined.");
    if is_binary_op proto
    then Op_prec.set (Op_prec.OpC (get_operator_name proto)) (get_binary_precedence proto);
    let bb = append_block context_ "entry" llfunc_ in
    position_at_end bb builder_;
    Hashtbl.clear named_tuple;
    Array.iter
      (fun arg ->
        let argname = value_name arg in
        let alloca = create_entry_block_alloca llfunc_ argname in
        ignore (build_store arg alloca builder_);
        Hashtbl.replace named_tuple argname alloca)
      (params llfunc_);
    (try
       let ret_val = expr_codegen body in
       ignore (build_ret ret_val builder_);
       if verify_function llfunc_
       then (
         ignore (PassManager.run_function llfunc_ fpm_);
         llfunc_)
       else raise (CodegenFailure "function verification failed")
     with
     | e ->
       dump_value llfunc_;
       delete_function llfunc_;
       raise e)
;;
