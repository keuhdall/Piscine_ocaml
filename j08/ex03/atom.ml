class virtual atom =
  object (this)
    method virtual name : string
    method virtual symbol : string
    method virtual atomic_number : int

    method to_string () = print_endline ("Name : " ^ this#name ^ " ; Symbol : " ^ this#symbol ^ " ; Atomic number : " ^ string_of_int this#atomic_number)
    method equals (a:atom) = if (this#name = a#name) then true else false
end

class hydrogen =
  object
    inherit atom
    method name = "Hydrogen"
    method symbol = "H"
    method atomic_number = 1
end

class carbon =
  object
    inherit atom
    method name = "Carbon"
    method symbol = "C"
    method atomic_number = 6
end

class oxygen =
  object
    inherit atom
    method name = "Oxygen"
    method symbol = "O"
    method atomic_number = 8
end

class helium =
object
  inherit atom
  method name = "Helium"
  method symbol = "He"
  method atomic_number = 2
end

class nitrogen =
object
  inherit atom
  method name = "Nitrogen"
  method symbol = "N"
  method atomic_number = 7
end

class sodium =
object
  inherit atom
  method name = "Sodium"
  method symbol = "Na"
  method atomic_number = 11
end