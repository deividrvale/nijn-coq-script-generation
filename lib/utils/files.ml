let oc = open_in "file"

let get_file_content file_path =
  let ch = open_in file_path in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s
