class doctor name age sidekick =
  object
  val _name:string = name
  val _age:int = age
  val mutable _hp:int = 100
  val _sidekick:People.people = sidekick

  initializer print_endline ("A new Doctor has been created, his name is " ^ _name)

  method to_string = print_endline ("This Doctor's name is " ^ _name ^ " ; he's " ^ (string_of_int _age) ^ " years old ; he has " ^ (string_of_int _hp) ^ "hp and his sidekick will now introduce herself : ") ; _sidekick#to_string
  method talk = print_endline "Hi! I’m the Doctor!"
  method travel_in_time (start:int) (arrival:int) = print_endline "  _______(_@_)_______\n  | POLICE      BOX |\n  |_________________|\n   | _____ | _____ |\n   | |###| | |###| |\n   | |###| | |###| | \n   | _____ | _____ |\n   | || || | || || |\n   | ||_|| | ||_|| |\n   | _____ |$_____ |\n   | || || | || || |\n   | ||_|| | ||_|| |\n   | _____ | _____ |\n   | || || | || || | \n   | ||_|| | ||_|| | \n   |       |       | \n   *****************" ; print_endline ("The doctor traveled from " ^ (string_of_int start) ^ " to " ^ (string_of_int arrival)) ; print_endline "His age remains the same as he's a timelord and travels inside the TARDIS"
  method use_sonic_screwdriver = print_endline "Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii"

  method private regenerate = _hp <- 100
end