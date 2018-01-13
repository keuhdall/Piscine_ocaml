let () =
  let rec print_deck l = match l with
  | first::remaining -> 
                        print_endline (Deck.Card.toString first) ;
                        print_deck remaining
  | []              -> () in
  let rec print_deck_string l = match l with
  | first::remaining -> 
                        print_endline first ;
                        print_deck_string remaining
  | []              -> () in
  let rec print_deck_string_v l = match l with
  | first::remaining -> 
                        print_endline first ;
                        print_deck_string_v remaining
  | []              -> () in
  let deck1 = Deck.newDeck () in
  let deck2 = Deck.newDeck () in
  let cardDraw (c, l) =
    print_endline (Deck.Card.toStringVerbose c) ;
    print_deck_string_v l
  in 
  print_deck_string (Deck.toStringList deck1) ;
  print_deck_string_v (Deck.toStringListVerbose deck1)