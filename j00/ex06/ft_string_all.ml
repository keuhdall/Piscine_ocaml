let is_digit c = c >= '0' && c <= '9'

let ft_string_all f s =
  let rec loop i =
    if (i < String.length s) then
      if (f (String.get s i) = false) then
        false
      else
        loop (i + 1)
    else
      true
  in
  loop 0

let () =
  print_endline (string_of_bool (ft_string_all is_digit ""));
  print_endline (string_of_bool (ft_string_all is_digit "0123456"));
  print_endline (string_of_bool (ft_string_all is_digit "abcd"));
  print_endline (string_of_bool (ft_string_all is_digit "0123abc"))