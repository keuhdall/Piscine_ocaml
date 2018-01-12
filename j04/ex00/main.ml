let () =
  let rec print_cards l = match l with
    | first::remaining -> print_string (Color.toString first) ;
                          print_string " : " ;
                          print_string (Color.toStringVerbose first) ;
                          print_char '\n' ;
                          print_cards (remaining)
    | []              -> ()
  in
  print_cards (Color.all)