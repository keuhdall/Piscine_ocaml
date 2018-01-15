let () =
  let jokes = Array.make 5 "" in
  Random.self_init () ;
  jokes.(0) <- "Quel oiseau adore le miel ? L'autruche, parce qu'à côté d'une ruche il y a toujours une autruche" ;
  jokes.(1) <- "Qu'est-ce qu'une baguette avec une boussole ? DU PAIN PERDU !!" ;
  jokes.(2) <- "Qu'est-ce qui est court et qui se jette ? UNE COURGETTE" ;
  jokes.(3) <- "Tu prends une gousse d'ail, tu la lance, elle te reviens dans la gueule : C'EST LE RETOUR DU JET D'AIL" ;
  jokes.(4) <- "Comment Ruquier retire sa capote ? BAH EN PETANT ! ISSOU !!" ;
  print_endline (jokes.(Random.int 5))