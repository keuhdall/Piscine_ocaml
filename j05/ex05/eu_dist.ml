let eu_dist a1 a2 =
  let len = Array.length a1 in
  let value = ref 0. in
  for i = 0 to (len - 1) do
    value := !value +. (a1.(i) -. a2.(i)) ** 2.
  done; sqrt !value

let () =
  print_float (eu_dist [|5.5 ; 9.6 ; 4.1 ; 0.1|] [|3.14 ; 10.55 ; 9.1 ; 8.3|]) ; print_char '\n' ;
  print_float (eu_dist [||] [||]) ; print_char '\n'