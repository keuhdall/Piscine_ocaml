type 'a ft_ref = {mutable value : 'a}

let return i = {value = i}

let get i = i.value

let set i j = i.value <- j

let bind i f : 'b ft_ref = f i.value

let () =
  let test1 = return 12 in
  let test2 = bind test1 (fun x -> return (string_of_int (get test1))) in
  print_int (get test1) ;
  print_char '\n' ;
  set test1 21 ;
  print_int (get test1) ;
  print_char '\n' ;
  print_endline (get test2)