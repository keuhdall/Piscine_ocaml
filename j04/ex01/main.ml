let () =
  let rec print_cards l = match l with
    | first::remaining -> 
                          print_int (Value.toInt first) ;
                          print_string " : " ;
                          print_string (Value.toString first) ;
                          print_string " : " ;
                          print_string (Value.toStringVerbose first) ;
                          print_string " ; next is : " ;
                          print_string (Value.toStringVerbose (Value.next first)) ;
                          print_string " ; previous is : " ;
                          print_string (Value.toStringVerbose (Value.previous first)) ;
                          print_char '\n' ;
                          print_cards (remaining)
    | []              -> ()
  in
  print_cards (Value.all)