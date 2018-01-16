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

let fill_array array name =
  let count = ref 0 in
  let ic = try open_in name with Sys_error err -> print_endline "An error occured openning the file ; exiting" ; exit 1 in
  try
    while true; do
      array.(!count) <- input_line ic ;
      incr count
    done; array
  with 
  | End_of_file     -> close_in ic; array

let () =
  let argv = Sys.argv in
  let name = try argv.(1) with Invalid_argument err -> print_endline "Please provide at least one argument" ; exit 1 in
  let numLines = count_lines name in
  let jokes = fill_array (Array.make numLines "")  name in
  Random.self_init () ;
  try print_endline (jokes.(Random.int numLines)) with Invalid_argument err -> print_endline "Error : empty file" ; exit 1