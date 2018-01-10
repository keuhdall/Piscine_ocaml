let encode l =
  let rec parse_list l i acc = match l with
    | first::second::remaining when first = second -> parse_list (second::remaining) (i + 1) acc
    | first::second::remaining                     -> parse_list (second::remaining) 0 (acc @ [((i + 1), first)])
    | first::remaining                             -> parse_list [] 0 (acc @ [((i + 1), first)])
    | []                                           -> acc
  in
  parse_list l 0 []


let () =
  let print_val_int (i, n) =
    print_int i;
    print_string " ; ";
    print_int n;
    print_string "\n"
  in
  let print_val_str (i, s) =
    print_int i;
    print_string " ; ";
    print_endline s
  in
  let rec print_list_int l = match l with
    | curr::remaining -> print_val_int curr ; print_list_int remaining
    | []              -> print_endline "====="
  in
  let rec print_list_str l = match l with
    | curr::remaining -> print_val_str curr ; print_list_str remaining
    | []              -> print_endline "====="
  in
  print_list_int (encode (1::2::2::3::3::3::[]));
  print_list_str (encode ("1"::"2"::"2"::"3"::"3"::"3"::[]))