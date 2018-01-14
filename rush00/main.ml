let grid_size = 9 (* Will be changed later *)
let inner_grid0 = Grid.newInnerGrid [Grid.X;Grid.Pending;Grid.Pending;Grid.Pending;Grid.Pending;Grid.Pending;Grid.Pending;Grid.Pending;Grid.O] Pending
let inner_grid1 = Grid.newInnerGrid [Grid.Pending;Grid.O;Grid.Pending;Grid.Pending;Grid.Pending;Grid.Pending;Grid.Pending;Grid.Pending;Grid.O] Pending
let inner_grid2 = Grid.newInnerGrid [Grid.X;Grid.Pending;Grid.X;Grid.Pending;Grid.Pending;Grid.Pending;Grid.Pending;Grid.Pending;Grid.O] Pending
let inner_grid3 = Grid.newInnerGrid [Grid.X;Grid.Pending;Grid.Pending;Grid.X;Grid.Pending;Grid.Pending;Grid.Pending;Grid.Pending;Grid.O] Pending
let inner_grid4 = Grid.newInnerGrid [Grid.X;Grid.Pending;Grid.Pending;Grid.Pending;Grid.Pending;Grid.Pending;Grid.X;Grid.Pending;Grid.O] Pending
let inner_grid5 = Grid.newInnerGrid [Grid.X;Grid.Pending;Grid.Pending;Grid.Pending;Grid.O;Grid.Pending;Grid.Pending;Grid.Pending;Grid.X] Pending
let inner_grid6 = Grid.newInnerGrid [Grid.X;Grid.Pending;Grid.Pending;Grid.Pending;Grid.Pending;Grid.Pending;Grid.Pending;Grid.O;Grid.X] Pending
let inner_grid7 = Grid.newInnerGrid [Grid.X;Grid.Pending;Grid.Pending;Grid.Pending;Grid.Pending;Grid.O;Grid.Pending;Grid.Pending;Grid.X] Pending
let inner_grid8 = Grid.newInnerGrid [Grid.X;Grid.Pending;Grid.Pending;Grid.Pending;Grid.Pending;Grid.Pending;Grid.X;Grid.Pending;Grid.X] Pending
let blank_outer_grid = Grid.newOuterGrid [inner_grid0;inner_grid1;inner_grid2;inner_grid3;inner_grid4;inner_grid5;inner_grid6;inner_grid7;inner_grid8] Pending

let main () =
  Display.print_grid blank_outer_grid 0;
  Reader_parser.parse blank_outer_grid grid_size

let () = main ()