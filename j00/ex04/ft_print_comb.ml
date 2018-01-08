let ft_print_comb () = 
  let rec loop x y z =
    print_int x;
    print_int y;
    print_int z;
    if (x < 7) then
      print_string ", ";
        if (z < 9) then
          loop x y (z + 1)
        else if (y < 8) then
          loop x (y + 1) (y + 2)
        else if (x < 7) then
          loop (x + 1) (x + 2) (x + 3)
    else print_string "\n"
    in
    loop 0 1 2

let () = ft_print_comb ()