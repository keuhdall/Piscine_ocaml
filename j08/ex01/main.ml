let () = 
  let h2o = new Molecule.water in
  let water = new Molecule.water in
  let co2 = new Molecule.carbon_dioxyde in
  let serotonin = new Molecule.serotonin in
  let coffee = new Molecule.cafein in
  let effexor = new Molecule.venlafaxine in
  
  print_endline (h2o#to_string) ;
  print_endline (co2#to_string) ;
  print_endline (serotonin#to_string) ;
  print_endline (coffee#to_string) ;
  print_endline (effexor#to_string) ;
  print_endline "is h2o = water ?" ;
  print_endline (string_of_bool (h2o#equals water)) ;
  print_endline "is h2o = coffee ?" ;
  print_endline (string_of_bool (h2o#equals coffee))