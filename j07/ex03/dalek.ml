class dalek =
  object
  val _name:string = "Dalek" ^ String.make 1 (char_of_int ((Random.int 26) + 65)) ^ String.make 1 (char_of_int ((Random.int 26) + 97)) ^ String.make 1 (char_of_int ((Random.int 26) + 97))
  val _hp:int = 100
  val mutable _shield:bool = true

  method to_string =
    if (_shield) then
      print_endline ("This Dalek's name is " ^ _name ^ " he has " ^ (string_of_int _hp) ^ "hp and his shield is enabled")
    else
      print_endline ("This Dalek's name is " ^ _name ^ " he has " ^ (string_of_int _hp) ^ "hp and his shield is disabled")
  method talk =
    let rand = (Random.int 3) in
    match rand with
      | 0 -> print_endline "Explain! Explain!"
      | 1 -> print_endline "Exterminate! Exterminate!"
      | 2 -> print_endline "I Obey!"
      | _ -> print_endline "You are the Doctor! You are the enemy of the Daleks!"
  
  method exterminate (ppl:People.people) =
  if (_shield) then _shield <- false else _shield <- true ; ppl#die

  method die = print_endline "Emergency Temporal Shift!"
end