let leibniz_pi max =
  if (max < 0) then
    -1
  else
    let ref_pi = (4.0 *. (atan 1)) in
    let rec pow n pow =
      if (n = 0) then
        0
      else
        let rec pow_loop n pow pow_acc=
          if (pow = 0) then
            pow_acc
          else
            loop n (pow - 1) (pow_acc + (n * pow))
    in
    let rec calc_pi i acc_pi =
      if (i = ~-1) then
        (4.0 *. acc_pi)
      else
        loop_pi (i - 1) (acc_pi +. ((float_of_int (pow -1 i)) /. (float_of_int (2 * i + 1))))
    in
    let rec calc_delta j =
      if ((calc_pi j 0.0) -. ref_pi <= float_of_int max) then
        j
      else
        calc_delta (j + 1)
    in
    calc_delta 0

let () =
  print_int (leibniz_pi 0.01);
  print_char '\n'