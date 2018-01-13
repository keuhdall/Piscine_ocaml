let rec print_string_list lst = match lst with
| [] -> print_endline ""
| head::tail -> print_endline head; print_string_list tail

let print_card_deck_tuple (x, y) =
print_endline (Deck.Card.toStringVerbose x);
print_endline "";
print_string_list (Deck.toStringList y)

let main () =
let deck1 = Deck.newDeck () in
let deck2 = Deck.newDeck () in
print_string_list (Deck.toStringList deck1);
print_string_list (Deck.toStringListVerbose deck2);
print_card_deck_tuple (Deck.drawCard deck2)

let () = main ()