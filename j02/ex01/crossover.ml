

let crossover l1 l2 =
  let rec is_in_list l e = match l with
  | first::remaining when first = e -> true
  | first::remaining                -> is_in_list remaining e
  | []                              -> false
  in
  let rec check_lists l1 l2 acc = match l1 with
  | first::remaining when is_in_list l2 first = true && is_in_list acc first = false -> check_lists remaining l2 (acc @ [first])
  | first::remaining                                                                 -> check_lists remaining l2 acc
  | []                                                                               -> acc
  in check_lists l1 l2 []

let () =
  let rec print_list_int l = match l with
    | curr::remaining -> print_int curr ; print_char ' ' ; print_list_int remaining
    | []              -> print_endline ""
  in
  let rec print_list_str l = match l with
    | curr::remaining -> print_string curr ; print_char ' ' ; print_list_str remaining
    | []              -> print_endline ""
  in
  print_list_int (crossover (1::1::1::3::5::7::[]) (5::7::[]));
  print_list_str (crossover ("1"::"1"::"1"::"3"::"5"::"7"::[]) ("1"::"1"::"1"::"2"::[]))