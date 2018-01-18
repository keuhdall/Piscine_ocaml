class ['a] army (a:'a list) =
  object
  val mutable l:('a list) = a

  method add (e:'a) = l <- (e::l)
  method delete () = match l with
    | hd::tl  -> l <- tl
    | []      -> l <- []
  
  method getList = l
end