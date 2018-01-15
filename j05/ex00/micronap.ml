let my_sleep () = Unix.sleep 1

let () =
  let argv = Sys.argv in
  begin
    try
      for i = 1 to int_of_string argv.(1) do
        my_sleep ()
      done
      with
        | Invalid_argument err  -> print_endline "Please provide at least one argument"
        | Failure err           -> print_endline "Please provide an integer as parameter"
    end