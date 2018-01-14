let get_grid x y =
  (3 * (x / 3)) + (y / 3)

let get_case x y =
  (3 * (x mod 3)) + (y mod 3)

let rec get_inner_grid grid n i = match grid with
  | [] -> invalid_arg "WTF"
  | head::tail -> if i = n then head else get_inner_grid tail n (i + 1)

let rec get_nth_case (lst:Grid.state list) n i = match lst with
  | [] -> invalid_arg "WTF"
  | head::tail -> if i = n then head else get_inner_grid tail n (i + 1)

let print_case_content (case:Grid.state) = match case with
  | X -> print_char 'X'
  | O -> print_char 'O'
  | Pending -> print_char '_'
  | Null -> ()

let print_case (grid:Grid.outer_grid) x y =
  let inner_grid = (get_inner_grid grid.outer_content (get_grid x y) 0) in
  print_case_content (get_nth_case inner_grid.inner_content (get_case x y) 0);
  print_char ' '

let rec print_grid grid n =
  print_case grid n 0;
  print_case grid n 1;
  print_case grid n 2;
  print_string "| ";
  print_case grid n 3;
  print_case grid n 4;
  print_case grid n 5;
  print_string "| ";
  print_case grid n 6;
  print_case grid n 7;
  print_case grid n 8;
  print_char '\n';
  if n = 2 || n = 5 then print_endline ("---------------------");
  if n = 8 then print_string "\n"
  else print_grid grid (n + 1)