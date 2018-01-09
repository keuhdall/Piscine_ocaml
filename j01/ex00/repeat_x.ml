let repeat_x i =
  let rec loop i s =
    if (i < 0) then
      "Error"
    else if (i = 0) then
      s
    else
      loop (i - 1) (s ^ "x")
    in loop i ""

let () =
  print_endline (repeat_x ~-5);
  print_endline (repeat_x 0);
  print_endline (repeat_x 5)