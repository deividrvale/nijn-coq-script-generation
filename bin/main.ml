let usage_msg = "onijn [-verbose] <file1> [<file2>] ... -o <file>"

let verbose = ref false

let input_files : (string list) ref = ref []

let output_file = ref ""

let anon_cmd filename =
  input_files := filename :: !input_files

let spec_list = [
  ("-verbose", Arg.Set verbose, "Output debug information");
  ("-o", Arg.Set_string output_file, "Set the output filename for the compiled coq file.")
]

(* The main "function" *)
let () =
  Arg.parse spec_list anon_cmd usage_msg;
  print_endline ("\n Number of files to be processed: "
    ^ (Int.to_string (List.length !input_files)));
  Utils.Lists.print_list (fun x -> "\n" ^ x) !input_files;
  List.iter (fun x -> print_endline (Utils.Files.get_file_content x) ) !input_files
