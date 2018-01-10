let leibniz_pi max =
  if (max < 0.0) then
    -1
  else
    let ref_pi = (4.0 *. (atan 1.0)) in
    let rec calc_pi i acc_pi =
      if (i = ~-1) then
        (4.0 *. acc_pi)
      else
        calc_pi (i - 1) (acc_pi +. ((-1. ** (float_of_int i)) /. float_of_int (2 * i + 1)))
    in
    let rec calc_delta j =
      if ((calc_pi j 0.0) -. ref_pi >= max) then
        begin
          print_float (calc_pi j 0.0);
          print_char '\n';
          print_float ref_pi;
          print_char '\n';
          j
        end
      else
        calc_delta (j + 1)
    in
    calc_delta 0

let () =
  print_int (leibniz_pi 0.01);
  print_char '\n'