let repeat_string ?(str="x") i =
  let rec loop i s =
    if (i < 0) then
      "Error"
    else if (i = 0) then
      s
    else
      loop (i - 1) (s ^ str)
    in loop i ""

let () =
  print_endline (repeat_string 5);
  print_endline (repeat_string ~str:"coucou" 5);
  print_endline (repeat_string 0);
  print_endline (repeat_string ~str:"coucou" 0);  
  print_endline (repeat_string ~-1);
  print_endline (repeat_string ~str:"coucou" ~-1)  