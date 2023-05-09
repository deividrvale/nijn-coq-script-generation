let usage_msg =
"usage: onijn <file> -o <output file>\n
This version only supports the file format given at:
https://deividrvale.github.io/nijn-coq-script-generation/onijn/index.html#input-file-format\n

This file format only supports tracing polynomial interpretation proofs.
Hence, no further configuration options are available.
See below the list of possible calls to onijn.
"

let version_msg =
"The onijn proof trace compiler, version 1.0.0\n
This version was used in the paper \"Certifying Higher-Order Polynomial Interpretations\",
by Niels van der Weide, Deivid Vale, and Cynthia Kop.
"

let print_version () =
  print_endline version_msg;
  exit 0

let input_files : (string list) ref = ref []

let output_file = ref ""

let anon_cmd filename =
  input_files := filename :: !input_files

let spec_list = [
  (
  "-o",
  Arg.Set_string output_file,
  "Set the output filename for the compiled coq file."
  );
  (
  "-v",
  Arg.Unit print_version ,
  "Print versioning information."
  )
]

let () =
  (* Parse user's arguments and save them in their proper reference values. *)
  Arg.parse spec_list anon_cmd usage_msg;
  (* We then set the file format to Wanda's and termination technique to Poly.
    Note: that's the only format we accept up to now.
    So this is why this configuration is hard-coded in the main function. *)

  match !input_files with
  | [] -> print_endline "onijn: error: no input file.\n"; Arg.usage spec_list usage_msg; exit (1)
  | _ -> begin
    let compiler_config : Compiler.config = {
    format = Compiler.WANDA;
    tt = Compiler.POLY
    } in (
      Compiler.set_compiler compiler_config;
      print_newline ();
      print_endline "Running onijn with the following configuration.";
      print_endline (Compiler.config_to_string compiler_config)
    );
    print_endline "\n\nProcessing file: ";
    Utils.Lists.print_list Fun.id !input_files;
    List.iter (fun x ->
      Utils.Files.write_to_file !output_file
      (
      Compiler.compile (Utils.Files.get_file_content x)
      )
    ) !input_files
  end
