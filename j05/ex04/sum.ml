let sum f1 f2 = f1 +. f2

let () =
  print_float (sum 1.0 1.0) ; print_char '\n' ;
  print_float (sum 0.0 1.0) ; print_char '\n' ;
  print_float (sum 1.0 1.0) ; print_char '\n' ;
  print_float (sum 1.0 (-1.0)) ; print_char '\n' ;
  print_float (sum 999999.1666 1.99) ; print_char '\n'