let () =
  Random.self_init () ;
  let sk1 = new People.people "Rose Tyler" in
  let doc1 = new Doctor.doctor "Christopher Eccleston" 878 sk1 in
  let sk2 = new People.people "Donna Noble" in
  let doc2 = new Doctor.doctor "David Tenant" 1533 sk2 in
  let sk3 = new People.people "Amelia Pond" in
  let doc3 = new Doctor.doctor "Matt Smith" 909 sk3 in
  let dalek1 = new Dalek.dalek in
  let dalek2 = new Dalek.dalek in
  let dalek3 = new Dalek.dalek in
  let doc_list = [doc1 ; doc2 ; doc3] in
  let sk_list = [sk1 ; sk2 ; sk3] in
  let dalek_list = [dalek1 ; dalek2 ; dalek3] in
  let doc_army = new Army.army doc_list in
  let sk_army = new Army.army sk_list in
  let dalek_army = new Army.army dalek_list in
  let rec print_army l = match l with
    | hd::tl  -> hd#to_string ; print_army tl
    | []      -> ()
  in
  print_army doc_army#getList ;
  print_endline "============================" ;
  print_army sk_army#getList ;
  print_endline "============================" ;
  print_army dalek_army#getList ;
  print_endline "============================" ;
  dalek_army#add (new Dalek.dalek) ;
  print_army dalek_army#getList ;
  print_endline "============================" ;
  dalek_army#delete () ;
  dalek_army#delete () ;
  print_army dalek_army#getList ;