class people name =
  object
  val _name:string = name
  val _hp:int = 100

  initializer print_endline ("A person was born. Name : " ^ _name ^ ". HP : " ^ string_of_int _hp)

  method to_string = print_endline ("I'm " ^ _name ^ "! Do you know the Doctor ?")
  method die = print_endline "Aaaarghh!"
end