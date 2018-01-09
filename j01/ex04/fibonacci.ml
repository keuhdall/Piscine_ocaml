let fibonacci i =
  if (i < 0) then
    -1
  else
    let rec loop i acc1 acc2 =
      if (i <= 0) then
        acc1
      else if (i = 1) then
        acc2
      else
        loop (i - 1) acc2 (acc1 + acc2)
    in
    loop i 0 1

let () =
  print_int (fibonacci ~-42);
  print_char '\n';
  print_int (fibonacci 1);
  print_char '\n';
  print_int (fibonacci 3);
  print_char '\n';
  print_int (fibonacci 6);
  print_char '\n';
  print_int (fibonacci 32);
  print_char '\n'