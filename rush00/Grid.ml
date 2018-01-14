type state = X | O | Pending | Null

type inner_grid = {
  inner_content : state list ;
  inner_state : state
}

type outer_grid = {
  outer_content : inner_grid list ;
  outer_state : state
}

let newInnerGrid newContent newState = {
  inner_content = newContent ;
  inner_state = newState
}

let newOuterGrid newContent newState = {
  outer_content = newContent ;
  outer_state = newState
}

let rec initOuterGridContent l i =
  if (i <= 8) then
    ((newInnerGrid [Pending; Pending; Pending; Pending; Pending; Pending; Pending; Pending; Pending] Pending)::l)
    initOuterGridContent l (i + 1)
  else
    l

let initOuterGrid () =
  newOuterGrid (initOuterGridContent [] 0) Pending

let rec parse_tmp (l:state list) = match l with
  | first::second::third::[] when (first = second) && (second = third) -> first
  | _ -> Pending

(* Vertical Lines *)
let rec checkOneVerticalLine l tmp i j = match l with
  | first::remaining when (i + 1) mod 3 = j -> checkOneVerticalLine remaining (first::tmp) (i + 1) j
  | first::remaining                        -> checkOneVerticalLine remaining tmp (i + 1) j
  | []  -> tmp

let rec checkAllVerticalLines grid i =
  let v_line_value = parse_tmp (checkOneVerticalLine grid.inner_content [] 0 i) in
  if (i <= 2) then
    if (v_line_value <> Pending) then v_line_value
    else checkAllVerticalLines grid (i + 1)
  else Pending

(* Horizontal Lines *)
let rec checkOneHorizontalLine l tmp i j = match l with
  | first::remaining when i >= (j * 3) && i <= (j * 3 + 2)  -> checkOneHorizontalLine remaining (first::tmp) (i + 1) j
  | first::remaining                                        -> checkOneHorizontalLine remaining tmp (i + 1) j
  | []  -> tmp

let rec checkAllHorizontalLines grid i =
  if (i <= 2) then
    let h_line_value = parse_tmp (checkOneHorizontalLine grid.inner_content [] 0 i) in
    if (h_line_value <> Pending) then h_line_value
    else checkAllHorizontalLines grid (i + 1)
  else Pending

(* Diagonals *)
let rec checkFirstDiagonal l tmp i = match l with
  | first::remaining when i = 0 || i = 4 || i = 8 -> checkFirstDiagonal remaining (first::tmp) (i + 1)
  | first::remaining                              -> checkFirstDiagonal remaining tmp (i + 1)
  | []  -> tmp

let rec checkSecondDiagonal l tmp i = match l with
  | first::remaining when i = 2 || i = 4 || i = 6 -> checkSecondDiagonal remaining (first::tmp) (i + 1)
  | first::remaining                              -> checkSecondDiagonal remaining tmp (i + 1)
  | []  -> tmp

let checkDiagonals grid =
  let first_diag_val = parse_tmp (checkFirstDiagonal grid.inner_content [] 0) in
  let second_diag_val = parse_tmp (checkSecondDiagonal grid.inner_content [] 0) in
  if (first_diag_val <> Pending) then first_diag_val
  else if (second_diag_val <> Pending) then second_diag_val
  else Pending
(* ---------- *)

let rec isGridCompleted (l:state list) = match l with
  | first::remaining when first = Pending -> Pending
  | first::remaining                      -> isGridCompleted remaining
  | []  -> Null

let isGridWon grid =
  let h_lines_val = checkAllHorizontalLines grid 0 in
  let v_lines_val = checkAllVerticalLines grid 0 in
  let d_lines_val = checkDiagonals grid in
  if (h_lines_val <> Pending) then h_lines_val
  else if (v_lines_val <> Pending) then v_lines_val
  else if (d_lines_val <> Pending) then d_lines_val
  else Pending

let rec getInnerGridContent l i j = match l with
  | first::remaining when i = j -> first.inner_content
  | _ -> getInnerGridContent l (i + 1) j

let rec getInnerGridState l i j = match l with
  | first::remaining when i = j -> first.inner_state
  | _ -> getInnerGridState l (i + 1) j

let rec getCell l i j = match l with
  | first::remaining when i = j -> first
  | _ -> getCell l (i + 1) j

let isCellEmpty (x, y) o_grid =
  if (getInnerGridState o_grid 0 x = Null) then false
  else
    let grid = getInnerGridContent o_grid 0 x in
    if (getCell grid 0 y <> Pending) then false
    else true

(* Returns the new inner_content of the record inner_grid *)
let rec getNewInnerGridContent l tmp i j sign = match l with
  | first::remaining when i = j   -> getNewInnerGridContent remaining (sign::tmp) (i + 1) j sign
  | first::remaining              -> getNewInnerGridContent remaining (first::tmp) (i + 1) j sign
  | []                            -> List.rev tmp

let getNewInnerGridState grid =
  let isWon = isGridWon grid in
  if (isWon <> Pending) then isWon
  else isGridCompleted grid.inner_content

let updateInnerGrid grid i sign =
  let newGridIteration = getNewInnerGridContent grid.inner_content [] 0 i sign in
  let newGridState = getNewInnerGridState (newInnerGrid newGridIteration grid.inner_state) in
  newInnerGrid newGridIteration newGridState

let rec parseOuterGridContent l tmp i j newGrid = match l with
  | first::remaining when i = j   -> parseOuterGridContent remaining (newGrid::tmp) (i + 1) j newGrid
  | first::remaining              -> parseOuterGridContent remaining (first::tmp) (i + 1) j newGrid
  | [] -> tmp

let updateOuterGrid o_grid (x, y) sign =
  parseOuterGridContent o_grid [] 0 x (updateInnerGrid (newInnerGrid (getInnerGridContent o_grid 0 x) (getInnerGridState o_grid 0 x)) y sign)


  