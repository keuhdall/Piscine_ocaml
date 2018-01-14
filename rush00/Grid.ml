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

let rec isGridCompleted (l:inner_grid list) = match l with
  | first::remaining when first.inner_state = Pending -> Pending
  | first::remaining                      -> isGridCompleted remaining
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

  let rec get_inner_grid (lst:inner_grid list) n i = match lst with
  | [] -> invalid_arg "WTF"
  | head::tail -> if i = n then head else get_inner_grid tail n (i + 1)

  let rec get_nth_case (lst:state list) n i = match lst with
  | [] -> invalid_arg "WTF"
  | head::tail -> if i = n then head else get_nth_case tail n (i + 1)

  let case_state_content (case:state) = match case with
  | X -> false
  | O -> false
  | Pending -> true
  | Null -> true

  let isCellEmpty (x, y) o_grid =
    let i_grid = (get_inner_grid o_grid.outer_content x 0) in
    let case = get_nth_case i_grid.inner_content y 0 in
    if (i_grid.inner_state <> Pending) then false
    else case_state_content case

let rec updatedInnerGridContent (lst:state list) (x, y) (sign:state) (ret:state list) i = match lst with
  | [] -> List.rev ret
  | head::tail -> if (i = y)  then updatedInnerGridContent tail (x, y) sign ([sign]@ret) (i + 1)
                              else updatedInnerGridContent tail (x, y) sign (head::ret) (i + 1)

let getUpdatedInner grid (x, y) (sign:state) =
  let new_grid  = newInnerGrid (updatedInnerGridContent grid.inner_content (x, y) sign [] 0) grid.inner_state in
  newInnerGrid new_grid.inner_content (in_isGridWon new_grid)

let rec updatedOuterGridContent (lst:inner_grid list) (x, y) (sign:state) (ret:inner_grid list) i = match lst with
  | [] -> List.rev ret
  | head::tail -> if (i = x)  then updatedOuterGridContent tail (x, y) sign ([(getUpdatedInner head (x, y) sign)]@ret) (i + 1)
                              else updatedOuterGridContent tail (x, y) sign (head::ret) (i + 1)

let updateOuterGrid o_grid (x, y) sign =
  let new_grid = newOuterGrid (updatedOuterGridContent o_grid.outer_content (x, y) sign [] 0) o_grid.outer_state in
  newOuterGrid new_grid.outer_content (out_isGridWon new_grid)