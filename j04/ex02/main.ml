let () =
  let rec print_cards l = match l with
    | first::remaining -> 
                          print_endline ("getValue : " ^ Card.Value.toStringVerbose (Card.getValue first) ^ " ; getColor : " ^ Card.Color.toStringVerbose (Card.getColor first)) ;
                          print_endline ("toString : " ^ (Card.toString first) ^ " ; toStringVerbose : " ^ (Card.toStringVerbose first)) ;
                          print_endline "======" ;
                          print_cards remaining
    | []              -> ()
  in
  let card2S = Card.newCard Card.Value.T2 Card.Color.Spade
  in
  let card10H = Card.newCard Card.Value.T10 Card.Color.Heart
  in
  let cardKS = Card.newCard Card.Value.King Card.Color.Spade
  in
  let cardQD = Card.newCard Card.Value.Queen Card.Color.Diamond
  in
  let cardAD = Card.newCard Card.Value.As Card.Color.Diamond
  in
  let cardAC = Card.newCard Card.Value.As Card.Color.Club
  in
  let cardList = [card2S ; card10H ; cardKS ; cardQD ; cardAD ; cardAC]
  in
  print_cards (Card.all) ;
  print_endline "=== compare tests ===" ;
  print_endline "compare 2S and 10H :" ;
  print_endline ("DEBUG : t1.value = " ^ string_of_int (Card.Value.toInt (Card.getValue card2S)) ^ " t2.value : " ^ string_of_int (Card.Value.toInt (Card.getValue card10H))) ;
  print_endline (string_of_int (compare card2S card10H)) ;
  print_endline "compare KS and QD :" ;
  print_endline ("DEBUG : t1.value = " ^ string_of_int (Card.Value.toInt (Card.getValue cardKS)) ^ " t2.value : " ^ string_of_int (Card.Value.toInt (Card.getValue cardQD))) ;
  print_endline (string_of_int (compare cardKS cardQD)) ;

  print_endline "compare AD and AC : " ;
  print_endline ("DEBUG : t1.value = " ^ string_of_int (Card.Value.toInt (Card.getValue cardAD)) ^ " t2.value : " ^ string_of_int (Card.Value.toInt (Card.getValue cardAC))) ;
  print_endline (string_of_int (compare cardAD cardAC)) ;
  print_endline "=== max tests ===" ;
  print_endline "max of KS and 10H : " ;
  print_endline (Card.toStringVerbose (max cardKS card10H)) ;
  print_endline "max of 2S and 10H : " ;
  print_endline (Card.toStringVerbose (max card2S card10H)) ;
  print_endline "max of AC and AD : " ;
  print_endline (Card.toStringVerbose (max cardAC cardAD)) ;
  print_endline "=== min tests ===" ;
  print_endline "min of KS and 10H : " ;
  print_endline (Card.toStringVerbose (min cardKS card10H)) ;
  print_endline "min of 2S and 10H : " ;
  print_endline (Card.toStringVerbose (min card2S card10H)) ;
  print_endline "min of AC and AD : " ;
  print_endline (Card.toStringVerbose (min cardAC cardAD)) ;
  print_endline "=== best test ===" ;
  print_endline (Card.toStringVerbose (Card.best cardList)) ;
  print_endline "=== isOf tests ===";
  print_endline "is 10H of S ?" ;
  print_string (string_of_bool (Card.isOf card10H Card.Color.Spade)) ;
  print_char '\n' ;
  print_endline "is 10H of H ?" ;
  print_string (string_of_bool (Card.isOf card10H Card.Color.Heart)) ;
  print_char '\n' ;
  print_endline "=== isX tests ===" ;
  print_endline "is 10H Spade ?" ;
  print_string (string_of_bool (Card.isSpade card10H)) ;
  print_char '\n' ;
  print_endline "is 10H Heart ?" ;
  print_string (string_of_bool (Card.isHeart card10H)) ;
  print_char '\n' ;
  print_endline "is 10H isDiamond ?" ;
  print_string (string_of_bool (Card.isDiamond card10H)) ;
  print_char '\n' ;
  print_endline "is 10H Club ?" ;
  print_string (string_of_bool (Card.isClub card10H)) ;
  print_char '\n'