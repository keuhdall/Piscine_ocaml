let () =
  let sidekick = new People.people "Amelia Pond" in
  let doc = new Doctor.doctor "Matt Smith" 909 sidekick in
  doc#to_string ;
  doc#talk ;
  doc#travel_in_time 2018 1939 ;
  print_endline "OH MY GOD, SOME DALEKS ! QUICK, LET'S USE THE SONIC SCREWDRIVER !" ;
  doc#use_sonic_screwdriver