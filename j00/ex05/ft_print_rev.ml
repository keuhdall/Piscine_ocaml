let ft_print_rev s =
  let len = (String.length s - 1) in
  let rec loop i =
    if (i >= 0) then
      begin
        print_char (String.get s i);
        loop (i - 1)
      end
    else
      print_char '\n'
    in
    loop len

let () =
  ft_print_rev "Hello world";
  ft_print_rev "";
  ft_print_rev "coucou"
