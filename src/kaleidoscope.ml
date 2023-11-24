let usage_msg = "kaleidoscope [<src file>] [-o <obejct output file>]"
let src_filename = ref ""

(* let output_filename = ref "" *)
let anon_fun filename = src_filename := filename
(* let speclist = [ "-o", Arg.Set_string output_filename, "obejct output" ] *)

let () =
  Arg.parse [] anon_fun usage_msg;
  if !src_filename = ""
  then Kaleidoscope.Repl.repl ()
  else Kaleidoscope.Compiler.compile_file !src_filename
;;
