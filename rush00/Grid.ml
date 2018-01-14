type state = X | O | Pending | Null

(* Records *)
type inner_grid = {
  inner_content : state list ;
  inner_state : state
}

type outer_grid = {
  outer_content : inner_grid list ;
  outer_state : state
}
(* ------- *)

(* Constructors *)
let newInnerGrid newContent newState = {
  inner_content = newContent ;
  inner_state = newState
}

let newOuterGrid newContent newState = {
  outer_content = newContent ;
  outer_state = newState
}
(* ---------- *)

let rec initOuterGridContent l i =
  if (i <= 8) then
    initOuterGridContent ((newInnerGrid [Pending; Pending; Pending; Pending; Pending; Pending; Pending; Pending; Pending] Pending)::l) (i + 1)
  else l

let initOuterGrid () =
  newOuterGrid (initOuterGridContent [] 0) Pending

let rec parse_tmp (l:state list) = match l with
  | first::second::third::[] when (first = second) && (second = third) -> first
  | _ -> Pending

(* Inner Vertical Lines *)
let rec in_checkOneVerticalLine l tmp i j = match l with
  | first::remaining when (i + 1) mod 3 = j -> in_checkOneVerticalLine remaining (first::tmp) (i + 1) j
  | first::remaining                        -> in_checkOneVerticalLine remaining tmp (i + 1) j
  | []  -> tmp

let rec in_checkAllVerticalLines grid i =
  let v_line_value = parse_tmp (in_checkOneVerticalLine grid.inner_content [] 0 i) in
  if (i <= 2) then
    if (v_line_value <> Pending) then v_line_value
    else in_checkAllVerticalLines grid (i + 1)
  else Pending
(* ---------- *)

(* Inner Horizontal Lines *)
let rec in_checkOneHorizontalLine l tmp i j = match l with
  | first::remaining when i >= (j * 3) && i <= (j * 3 + 2)  -> in_checkOneHorizontalLine remaining (first::tmp) (i + 1) j
  | first::remaining                                        -> in_checkOneHorizontalLine remaining tmp (i + 1) j
  | []  -> tmp

let rec in_checkAllHorizontalLines grid i =
  if (i <= 2) then
    let h_line_value = parse_tmp (in_checkOneHorizontalLine grid.inner_content [] 0 i) in
    if (h_line_value <> Pending) then h_line_value
    else in_checkAllHorizontalLines grid (i + 1)
  else Pending
(* ---------- *)

(* Inner Diagonals *)
let rec in_checkFirstDiagonal l tmp i = match l with
  | first::remaining when i = 0 || i = 4 || i = 8 -> in_checkFirstDiagonal remaining (first::tmp) (i + 1)
  | first::remaining                              -> in_checkFirstDiagonal remaining tmp (i + 1)
  | []  -> tmp

let rec in_checkSecondDiagonal l tmp i = match l with
  | first::remaining when i = 2 || i = 4 || i = 6 -> in_checkSecondDiagonal remaining (first::tmp) (i + 1)
  | first::remaining                              -> in_checkSecondDiagonal remaining tmp (i + 1)
  | []  -> tmp

let in_checkDiagonals grid =
  let first_diag_val = parse_tmp (in_checkFirstDiagonal grid.inner_content [] 0) in
  let second_diag_val = parse_tmp (in_checkSecondDiagonal grid.inner_content [] 0) in
  if (first_diag_val <> Pending) then first_diag_val
  else if (second_diag_val <> Pending) then second_diag_val
  else Pending
(* ---------- *)

(* Outer Vertical Lines *)
let rec out_checkOneVerticalLine l tmp i j = match l with
  | first::remaining when (i + 1) mod 3 = j -> out_checkOneVerticalLine remaining (first.inner_state::tmp) (i + 1) j
  | first::remaining                        -> out_checkOneVerticalLine remaining tmp (i + 1) j
  | []  -> tmp

let rec out_checkAllVerticalLines grid i =
  let v_line_value = parse_tmp (out_checkOneVerticalLine grid.outer_content [] 0 i) in
  if (i <= 2) then
    if (v_line_value <> Pending) then v_line_value
    else out_checkAllVerticalLines grid (i + 1)
  else Pending
(* ---------- *)

(* Outer Horizontal Lines *)
let rec out_checkOneHorizontalLine l tmp i j = match l with
  | first::remaining when i >= (j * 3) && i <= (j * 3 + 2)  -> out_checkOneHorizontalLine remaining (first.inner_state::tmp) (i + 1) j
  | first::remaining                                        -> out_checkOneHorizontalLine remaining tmp (i + 1) j
  | []  -> tmp

let rec out_checkAllHorizontalLines grid i =
  if (i <= 2) then
    let h_line_value = parse_tmp (out_checkOneHorizontalLine grid.outer_content [] 0 i) in
    if (h_line_value <> Pending) then h_line_value
    else out_checkAllHorizontalLines grid (i + 1)
  else Pending
(* ---------- *)

(* Outer Diagonals *)
let rec out_checkFirstDiagonal l tmp i = match l with
  | first::remaining when i = 0 || i = 4 || i = 8 -> out_checkFirstDiagonal remaining (first.inner_state::tmp) (i + 1)
  | first::remaining                              -> out_checkFirstDiagonal remaining tmp (i + 1)
  | []  -> tmp

let rec out_checkSecondDiagonal l tmp i = match l with
  | first::remaining when i = 2 || i = 4 || i = 6 -> out_checkSecondDiagonal remaining (first.inner_state::tmp) (i + 1)
  | first::remaining                              -> out_checkSecondDiagonal remaining tmp (i + 1)
  | []  -> tmp

let out_checkDiagonals grid =
  let first_diag_val = parse_tmp (out_checkFirstDiagonal grid.outer_content [] 0) in
  let second_diag_val = parse_tmp (out_checkSecondDiagonal grid.outer_content [] 0) in
  if (first_diag_val <> Pending) then first_diag_val
  else if (second_diag_val <> Pending) then second_diag_val
  else Pending
(* ---------- *)

let rec in_isGridCompleted (l:state list) = match l with
  | first::remaining when first = Pending -> Pending
  | first::remaining                      -> in_isGridCompleted remaining
  | []  -> Null

let in_isGridWon grid =
  let h_lines_val = in_checkAllHorizontalLines grid 0 in
  let v_lines_val = in_checkAllVerticalLines grid 0 in
  let d_lines_val = in_checkDiagonals grid in
  if (h_lines_val <> Pending) then h_lines_val
  else if (v_lines_val <> Pending) then v_lines_val
  else if (d_lines_val <> Pending) then d_lines_val
  else Pending

  let out_isGridWon o_grid =
    let h_lines_val = out_checkAllHorizontalLines o_grid 0 in
    let v_lines_val = out_checkAllVerticalLines o_grid 0 in
    let d_lines_val = out_checkDiagonals o_grid in
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
  let isWon = in_isGridWon grid in
  if (isWon <> Pending) then isWon
  else in_isGridCompleted grid.inner_content

let updateInnerGrid grid i sign =
  let newGridIteration = getNewInnerGridContent grid.inner_content [] 0 i sign in
  let newGridState = getNewInnerGridState (newInnerGrid newGridIteration grid.inner_state) in
  newInnerGrid newGridIteration newGridState

let rec parseOuterGridContent l tmp i j newGrid = match l with
  | first::remaining when i = j   -> parseOuterGridContent remaining (newGrid::tmp) (i + 1) j newGrid
  | first::remaining              -> parseOuterGridContent remaining (first::tmp) (i + 1) j newGrid
  | [] -> List.rev tmp

let updateOuterGrid o_grid (x, y) sign =
  let updatedContent = parseOuterGridContent o_grid.outer_content [] 0 x (updateInnerGrid (newInnerGrid (getInnerGridContent o_grid.outer_content 0 x) (getInnerGridState o_grid.outer_content 0 x)) y sign) in
  newOuterGrid updatedContent (out_isGridWon o_grid)