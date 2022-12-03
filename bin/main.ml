let usage_msg = "onijn <file1> [<file2>] ... -o <output file>"

let input_files : (string list) ref = ref []
let output_file = ref ""

let icfg = Compiler.get_initial_config ()

let anon_cmd filename =
  input_files := filename :: !input_files

let spec_list = [
  ("-o", Arg.Set_string output_file, "Set the output filename for the compiled coq file.")
]

(* The main "function" *)
let () =
  Arg.parse spec_list anon_cmd usage_msg;

  let compiler_config : Compiler.config = {
    format = Compiler.WANDA;
    tt = Compiler.POLY
  } in Compiler.set_compiler compiler_config;

  print_endline ("Number of files to be processed: "
    ^ (Int.to_string (List.length !input_files)));

  Utils.Lists.print_list (fun x -> x) !input_files;

  List.iter (fun x ->
    Utils.Files.write_to_file !output_file
    (
     Compiler.compile (Utils.Files.get_file_content x)
    )
  ) !input_files
