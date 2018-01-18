let () =
  Random.self_init () ;
  let rory = new People.people "Rory Williams" in
  let sidekick = new People.people "Amelia Pond" in
  let doctor = new Doctor.doctor "Matt Smith" 909 sidekick in
  let dalek = new Dalek.dalek in
  dalek#to_string ;
  doctor#to_string ;
  doctor#talk ;
  dalek#talk ;
  dalek#exterminate rory ;
  dalek#to_string ;
  doctor#use_sonic_screwdriver ;
  dalek#die ;
  doctor#travel_in_time 2018 2018