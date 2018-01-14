let get_grid x y =
  (3 * (x / 3)) + (y / 3)

let get_case x y =
  (3 * (x mod 3)) + (y mod 3)

let rec get_inner_grid grid n i = match grid with
  | [] -> invalid_arg "WTF"
  | head::tail -> if i = n then head else get_inner_grid tail n (i + 1)

let rec get_nth_case (lst:Grid.cell_state list) n i = match lst with
  | [] -> invalid_arg "WTF"
  | head::tail -> if i = n then head else get_inner_grid tail n (i + 1)

let print_case (case:Grid.cell_state) = match case with
  | X -> print_char 'X'
  | O -> print_char 'O'
  | Pending -> print_char '_'

let print_row (grid:Grid.outer_grid) x y =
  let inner_grid = (get_inner_grid grid.outer_content (get_grid x y) 0) in
  print_case (get_nth_case inner_grid.inner_content (get_case x y) 0)

let rec print_grid grid n =
  print_row grid n 0;
  if n = 8 then print_string "\n"
  else print_grid grid (n + 1)