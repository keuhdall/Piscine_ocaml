type cell_state = X | O | Pending
type grid_state = X | O | Pending | Null

type inner_grid = {
  inner_content : cell_state list ;
  inner_state : grid_state
}

type outer_grid = {
  outer_content : inner_grid list ;
  outer_state : grid_state
}

let newInnerGrid newContent newState = {
  inner_content = newContent ;
  inner_state = newState
}

let newOuterGrid newContent newState = {
  outer_content = newContent ;
  outer_state = newState
}

let rec parse_tmp (l:cell_state list) = match l with
  | first::second::third::[] when (first = second) && (second = third) -> first
  | _ -> Pending

(* Vertical Lines *)
let rec checkOneVerticalLine l tmp i j = match l with
  | first::remaining when (i + 1) mod 3 = j -> checkOneVerticalLine remaining (first::tmp) (i + 1) j
  | first::remaining                        -> checkOneVerticalLine remaining tmp (i + 1) j
  | []  -> tmp

let rec checkAllVerticalLines grid i =
  if (i <= 2) then
    if (parse_tmp (checkOneVerticalLine grid.inner_content [] 0 i) <> Pending) then
      parse_tmp (checkOneVerticalLine grid.inner_content [] 0 i)
    else checkAllVerticalLines grid (i + 1)
  else Pending

(* Horizontal Lines *)
let rec checkOneHorizontalLine l tmp i j = match l with
  | first::remaining when i >= (j * 3) && i <= (j * 3 + 2)  -> checkOneHorizontalLine remaining (first::tmp) (i + 1) j
  | first::remaining                                        -> checkOneHorizontalLine remaining tmp (i + 1) j
  | []  -> tmp

let rec checkAllHorizontalLines grid i =
  if (i <= 2) then
    if (parse_tmp (checkOneHorizontalLine grid.inner_content [] 0 i) <> Pending) then
      parse_tmp (checkOneHorizontalLine grid.inner_content [] 0 i)
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
  if (parse_tmp (checkFirstDiagonal grid.inner_content [] 0) <> Pending) then
    parse_tmp (checkFirstDiagonal grid.inner_content [] 0)
  else if (parse_tmp (checkSecondDiagonal grid.inner_content [] 0) <> Pending) then
    parse_tmp (checkSecondDiagonal grid.inner_content [] 0)
  else Pending

let rec getInnerGrid l i j = match l with
  | first::remaining when i = j -> first.inner_content
  | _ -> getInnerGrid l (i + 1) j

let rec getInnerGridState l i j = match l with
  | first::remaining when i = j -> first.inner_state
  | _ -> getInnerGridState l (i + 1) j

let rec getCell l i j = match l with
  | first::remaining when i = j -> first
  | _ -> getCell l (i + 1) j

let isCellEmpty (x, y) o_grid =
  if (getInnerGridState o_grid 0 x = Null) then false
  else
    let grid = getInnerGrid o_grid 0 x in
    if (getCell grid 0 y <> Pending) then false
    else true

