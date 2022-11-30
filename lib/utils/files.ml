
let get_file_content (file_path : string) =
  let ch = open_in file_path in
    let file_content = really_input_string ch (in_channel_length ch) in
    close_in ch;
    file_content
