(*
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
*)
let grid_size = 9

let isGameOver (grid:Grid.outer_grid) = grid.outer_state <> Grid.Pending
 

let announcePlayer p = match p with
  | Grid.X -> print_endline "X to play"
  | Grid.O -> print_endline "O to play"
  | _ -> ()

let is_valid tuple = match tuple with
  | ((-1), (-1)) -> print_endline "Incorrect format."; false
  | (_, _) -> true

let is_legal tuple (grid:Grid.outer_grid) =
  if (Grid.isCellEmpty tuple grid = true) then true
    else begin print_endline "Illegal move." ; false end

let rec getMove (grid:Grid.outer_grid) =
  let move = Reader_parser.parse grid grid_size in
  if (is_valid move = true && is_legal move grid = true) then move
  else getMove grid

let main () =
  let grid = Grid.initOuterGrid () in
  let rec game_loop grid p1 p2 =
    if (isGameOver grid = false) then
      begin
        Display.print_grid grid 0;
        announcePlayer p1;
        let move = getMove grid in
        game_loop (Grid.updateOuterGrid grid move p1) p2 p1
      end
    else print_endline ("Game Over")
  in
  game_loop grid X O

let () = main ()