let ft_is_palindrome s =
  let rec loop i =
    if (i < (String.length s) / 2) then
      if (String.get s i = String.get s (String.length s - i - 1)) then
        loop (i + 1)
      else
        false
    else
      true
  in
  loop 0

let () =
  print_endline (string_of_bool (ft_is_palindrome "radar"));
  print_endline (string_of_bool (ft_is_palindrome "madam"));
  print_endline (string_of_bool (ft_is_palindrome "aaaaaaaaaaaaaaaaaaaaaa"));
  print_endline (string_of_bool (ft_is_palindrome "aaaaaaaaaaaIaaaaaaaaaaa"));
  print_endline (string_of_bool (ft_is_palindrome "aaaaaaaaaaaaIaaaaaaaaaa"));
  print_endline (string_of_bool (ft_is_palindrome "kayak"));
  print_endline (string_of_bool (ft_is_palindrome "a"));
  print_endline (string_of_bool (ft_is_palindrome "aa"));
  print_endline (string_of_bool (ft_is_palindrome ""));
  print_endline (string_of_bool (ft_is_palindrome "blablabla"));