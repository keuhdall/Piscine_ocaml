let get_grid x y =
  (3 * (x / 3)) + (y / 3)

let get_case x y =
  (3 * (x mod 3)) + (y mod 3)

let rec get_inner_grid grid n i = match grid with
  | [] -> invalid_arg "WTF"
  | head::tail -> if i = n then head else get_inner_grid tail n (i + 1)

let print_row grid x y =
  get_inner_grid grid.outer_content (get_grid x y) 0

let rec print_grid grid n =
  print_row grid.outer_content n 0
  if n = 8 then print_string ""
  else print_grid grid (n + 1)