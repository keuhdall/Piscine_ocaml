let iter f x n =
  if (n < 0) then
    -1
  else
    let rec loop f x n =
      if (n = 0) then
        x
      else
        loop f (f x) (n - 1)
    in
    loop f x n

let () =
  print_int (iter (fun x -> x * x) 2 4);
  print_char '\n';
  print_int (iter (fun x -> x * 2) 2 4);
  print_char '\n';
  print_int (iter (fun x -> x * 2) 2 0);
  print_char '\n';
  print_int (iter (fun x -> x * 2) 2 1);
  print_char '\n';
  print_int (iter (fun x -> x * 2) 2 ~-1);
  print_char '\n'