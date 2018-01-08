let ft_countdown x =
  let rec loop x =
      if (x > 0) then
        begin
          print_int x;
          print_char '\n';
          loop (x - 1)
        end
      else
        print_int 0
  in
  loop x;
  print_char '\n'

let () =
  ft_countdown 5;
  print_endline "===";
  ft_countdown ~-5;
  print_endline "===";
  ft_countdown 15;
  print_endline "===";
  ft_countdown ~-273;
  print_endline "===";
  ft_countdown 0