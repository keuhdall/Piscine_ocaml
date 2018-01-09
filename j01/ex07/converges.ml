let converges f x n =
  if (n < 0) then
    false
  else
    let rec loop f x i =
      if (i = ~-1) then
        false
      else if (x = (f x)) then
        true
      else
        loop f (f x) (i - 1)
    in
    loop f x n

let () =
  print_endline (string_of_bool (converges (( * ) 2) 2 3));
  print_endline (string_of_bool (converges (fun x -> x / 2) 2 3));
  print_endline (string_of_bool (converges (fun x -> x / 2) 2 2))