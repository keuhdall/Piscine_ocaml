let grid_size = 9 (* Will be changed later *)
let blank_inner_grid = Grid.newInnerGrid [Grid.Pending;Grid.Pending;Grid.Pending;Grid.Pending;Grid.Pending;Grid.Pending;Grid.Pending;Grid.Pending;Grid.Pending] Pending
let blank_outer_grid = Grid.newOuterGrid [blank_inner_grid;blank_inner_grid;blank_inner_grid;blank_inner_grid;blank_inner_grid;blank_inner_grid;blank_inner_grid;blank_inner_grid;blank_inner_grid] Pending

let main () =
  Display.print_grid blank_outer_grid 0;
  Reader_parser.parse blank_outer_grid grid_size

let () = main ()