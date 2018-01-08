let ft_power num pow =
  if (num = 0) then
    0
  else
    let rec loop x y =
      if (y = 0) then
        1
      else
        num * (loop num (y - 1))
      in
      loop num pow
  
let () = 
  print_int (ft_power 5 5); print_char '\n';
  print_int (ft_power 0 5); print_char '\n';
  print_int (ft_power 5 0); print_char '\n';
  print_int (ft_power 3 2); print_char '\n'