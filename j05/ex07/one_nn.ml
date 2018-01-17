type radar = float array * string

let eu_dist a1 a2 =
  let len = Array.length a1 in
  let value = ref 0. in
  for i = 0 to (len - 1) do
    value := !value +. (a1.(i) -. a2.(i)) ** 2.
  done; sqrt !value

let count_lines name = 
  let count = ref 0 in
  let ic = try open_in name with Sys_error err -> print_endline "An error occured openning the file ; exiting" ; exit 1 in
  try
    while true; do
      ignore (input_line ic) ;
      incr count
    done; !count
  with 
  | End_of_file     -> close_in ic; !count
  | Sys_error err   -> print_endline "An error occured openning the file ; exiting" ; exit 1


let get_example_line name i = 
  let count = ref 0 in
  let str_list = ref [] in
  let ic = try open_in name with Sys_error err -> print_endline "An error occured openning the file ; exiting" ; exit 1 in
  try
    while true; do
      if (!count = i) then
        let s = input_line ic in
        begin str_list := String.split_on_char ',' s ; incr count end
      else ignore (input_line ic) ; incr count
    done; !str_list
  with 
  | End_of_file     -> close_in ic; !str_list
  | Sys_error err   -> print_endline "An error occured openning the file ; exiting" ; exit 1

let rec fill_array l a i = match l with
  | hd::tl -> a.(i) <- hd ; fill_array tl a (i + 1)
  | [] -> a

let slice_first l = match l with
  | hd::tl  -> tl
  | []      -> failwith "Error in content file"

let extract_elem l = match l with
  | hd::tl  -> hd
  | []      -> failwith "Error in content file"

let example_of_file name i =
  let numLines = count_lines name in
  let lineContent_tmp = get_example_line name i in
  let lastElem = List.hd (List.rev lineContent_tmp) in
  let lineContent_str = List.rev (slice_first (List.rev lineContent_tmp)) in
  let lineContent_float = List.map float_of_string lineContent_str in
  let float_array = fill_array lineContent_float (Array.make (List.length lineContent_float) 0.0) 0 in
  (float_array, lastElem)

let one_nn rl r =
