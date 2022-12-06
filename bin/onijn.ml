let usage_msg = "onijn <file> -o <output file>"

let input_files : (string list) ref = ref []

let output_file = ref ""

let anon_cmd filename =
  input_files := filename :: !input_files

let spec_list = [
  (
  "-o",
  Arg.Set_string output_file,
  "Set the output filename for the compiled coq file."
  )
]

(* The "main" function. *)
let () =
  (* Parse user's arguments and save them in their proper reference values. *)
  Arg.parse spec_list anon_cmd usage_msg;
  (* We then set the file format to Wanda's and termination technique to Poly.
    Note: that's the only format we accept up to now.
    So this is why this configuration is hard-coded in the main function. *)
  let compiler_config : Compiler.config = {
    format = Compiler.WANDA;
    tt = Compiler.POLY
  } in (
    Compiler.set_compiler compiler_config;
    print_newline ();
    print_endline "Running onijn with the following configuration.";
    print_endline (Compiler.config_to_string compiler_config)
  );

  print_endline "\n\n Processing file: ";
  Utils.Lists.print_list Fun.id !input_files;

  List.iter (fun x ->
    Utils.Files.write_to_file !output_file
    (
     Compiler.compile (Utils.Files.get_file_content x)
    )
  ) !input_files
