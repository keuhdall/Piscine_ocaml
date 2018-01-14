let is_num c = c >= '0' && c <= '9'

let is_string_num str =
  let len = String.length str in
  let rec tail_check n =
    if is_num (String.get str n) = false then false
    else if (n + 1) = len then true
    else tail_check (n + 1)
  in tail_check 0

let check_input lst = match lst with
  | head::tail::[] -> (is_string_num head) && (is_string_num tail)
  | _ -> false

let get_coordonates lst = match lst with
  | head::tail::[] -> ((int_of_string head - 1), (int_of_string tail - 1))
  | _ -> ((-1), (-1))

let parse_coordonates (x, y) grid_size =
  if x < 0 || x > (grid_size - 1) then false
  else if y < 0 || y > (grid_size - 1) then false
  else true

let get_grid x y =
  (3 * (x / 3)) + (y / 3)

let get_case x y =
  (3 * (x mod 3)) + (y mod 3)

let get_grid_and_case (x, y) =
  (get_grid x y, get_case x y)
  
let parse grid grid_size =
  let input = read_line () in
  let list_input = (String.split_on_char ' ' input) in
  if (check_input list_input) = false then ((-1), (-1))
  else
    let coordonates = (get_coordonates list_input) in
    if parse_coordonates coordonates grid_size = false then ((-1), (-1))
    else get_grid_and_case coordonates