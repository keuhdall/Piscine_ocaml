let ft_sum f s e =
  if (s > e) then
    nan
  else
    let rec loop f s e acc =
      if (s = e + 1) then
        acc
      else
        loop f (s + 1) e (acc +. (f s))
    in
    loop f s e 0.0

let () =
  print_float (ft_sum (fun i -> float_of_int (i * i)) 1 10);
  print_char '\n'