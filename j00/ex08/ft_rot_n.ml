let ft_rot_n n s =
  let rot_char c =
    if (int_of_char c >= int_of_char 'a' && int_of_char c <= int_of_char 'z') then
      char_of_int ((int_of_char c - int_of_char 'a' + n) mod 26 + int_of_char 'a')
    else if (int_of_char c >= int_of_char 'A' && int_of_char c <= int_of_char 'Z') then
      char_of_int ((int_of_char c - int_of_char 'A' + n) mod 26 + int_of_char 'A')
    else
      c
    in
  String.map rot_char s

let () =
  print_endline (ft_rot_n 1 "abcdefghijklmnopqrstuvwxyz");
  print_endline (ft_rot_n 13 "abcdefghijklmnopqrstuvwxyz");
  print_endline (ft_rot_n 42 "0123456789");
  print_endline (ft_rot_n 2 "OI2EAS67B9");
  print_endline (ft_rot_n 0 "Damned !");
  print_endline (ft_rot_n 42 "");
  print_endline (ft_rot_n 1 "NBzlk qnbjr !")