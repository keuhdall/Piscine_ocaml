let ft_print_alphabet () =
  let begin_char = int_of_char 'a' in
  let rec loop curr_char =
    if (curr_char < int_of_char 'z') then
      begin
        print_char (char_of_int curr_char);
        loop (curr_char + 1)
      end
    else
      print_char '\n'
    in
    loop begin_char

let () = ft_print_alphabet ()