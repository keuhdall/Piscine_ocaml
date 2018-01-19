let () = 
  let mtn = new Alkane.methane in
  let mtn2 = new Alkane.methane in
  let etn = new Alkane.ethane in
  let otn = new Alkane.octane in
  print_endline (mtn#to_string) ;
  print_endline (etn#to_string) ;
  print_endline (otn#to_string) ;
  print_endline "is CH4 = methane ?" ;
  print_endline (string_of_bool (mtn#equals mtn2)) ;
  print_endline "is CH4 = octane ?" ;
  print_endline (string_of_bool (mtn#equals otn)) ;