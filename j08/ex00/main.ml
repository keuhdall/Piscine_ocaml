let () = 
  let h =  new Atom.hydrogen in
  let h2 =  new Atom.hydrogen in
  let c = new Atom.carbon in
  let o =  new Atom.oxygen in
  let he = new Atom.helium in
  let n =  new Atom.nitrogen in
  let s = new Atom.sodium in
  h#to_string () ;
  h2#to_string () ;
  c#to_string () ;
  o#to_string () ;
  he#to_string () ;
  n#to_string () ;
  s#to_string () ;
  print_endline "h = h2 ?" ;
  print_endline (string_of_bool (h#equals h2)) ;
  print_endline "h = he ?" ;
  print_endline (string_of_bool (h#equals he))