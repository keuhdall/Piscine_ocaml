module Card =
  struct
    module Color =
      struct
        type t = Spade | Heart | Diamond | Club
      
        let all = [Spade ; Heart ; Diamond ; Club]

        let toString t = match t with
          | Spade   -> "S"
          | Heart   -> "H"
          | Diamond -> "D"
          | Club    -> "C"

        let toStringVerbose t = match t with
          | Spade   -> "Spade"
          | Heart   -> "Heart"
          | Diamond -> "Diamond"
          | Club    -> "Club"
      end

    module Value =
      struct
        type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

        let all = [T2 ; T3 ; T4 ; T5 ; T6 ; T7 ; T8 ; T9 ; T10 ; Jack ; Queen ; King ; As]

        let toInt t = match t with
          | T2     -> 1
          | T3     -> 2
          | T4     -> 3
          | T5     -> 4
          | T6     -> 5
          | T7     -> 6
          | T8     -> 7
          | T9     -> 8
          | T10    -> 9
          | Jack   -> 10
          | Queen  -> 11
          | King   -> 12
          | As     -> 13

        let toString t = match t with
          | T2     -> "2"
          | T3     -> "3"
          | T4     -> "4"
          | T5     -> "5"
          | T6     -> "6"
          | T7     -> "7"
          | T8     -> "8"
          | T9     -> "9"
          | T10    -> "10"
          | Jack   -> "J"
          | Queen  -> "Q"
          | King   -> "K"
          | As     -> "A"

        let toStringVerbose t = match t with
          | T2     -> "2"
          | T3     -> "3"
          | T4     -> "4"
          | T5     -> "5"
          | T6     -> "6"
          | T7     -> "7"
          | T8     -> "8"
          | T9     -> "9"
          | T10    -> "10"
          | Jack   -> "Jack"
          | Queen  -> "Queen"
          | King   -> "King"
          | As     -> "As"

        let next t = match t with
          | T2     -> T3
          | T3     -> T4
          | T4     -> T5
          | T5     -> T6
          | T6     -> T7
          | T7     -> T8
          | T8     -> T9
          | T9     -> T10
          | T10    -> Jack
          | Jack   -> Queen
          | Queen  -> King
          | King   -> As
          | As     -> invalid_arg "Error"

        let previous t = match t with
          | T2     -> invalid_arg "Error"
          | T3     -> T2
          | T4     -> T3
          | T5     -> T4
          | T6     -> T5
          | T7     -> T6
          | T8     -> T7
          | T9     -> T8
          | T10    -> T9
          | Jack   -> T10
          | Queen  -> Jack
          | King   -> Queen
          | As     -> King
      end
    type t = {
      color:Color.t ;
      value:Value.t
    }

    let newCard newValue newColor = {
      value = newValue ;
      color = newColor
    }

    let allSpades = [
      {color = Spade ; value = T2       } ;
      {color = Spade ; value = T3       } ;
      {color = Spade ; value = T4       } ;
      {color = Spade ; value = T5       } ;
      {color = Spade ; value = T6       } ;
      {color = Spade ; value = T7       } ;
      {color = Spade ; value = T8       } ;
      {color = Spade ; value = T9       } ;
      {color = Spade ; value = T10      } ;
      {color = Spade ; value = Jack     } ;
      {color = Spade ; value = Queen    } ;
      {color = Spade ; value = King     } ;
      {color = Spade ; value = As       }
    ]

    let allHearts = [
      {color = Heart ; value = T2       } ;
      {color = Heart ; value = T3       } ;
      {color = Heart ; value = T4       } ;
      {color = Heart ; value = T5       } ;
      {color = Heart ; value = T6       } ;
      {color = Heart ; value = T7       } ;
      {color = Heart ; value = T8       } ;
      {color = Heart ; value = T9       } ;
      {color = Heart ; value = T10      } ;
      {color = Heart ; value = Jack     } ;
      {color = Heart ; value = Queen    } ;
      {color = Heart ; value = King     } ;
      {color = Heart ; value = As       }
    ]

    let allDiamonds = [
      {color = Diamond ; value = T2     } ;
      {color = Diamond ; value = T3     } ;
      {color = Diamond ; value = T4     } ;
      {color = Diamond ; value = T5     } ;
      {color = Diamond ; value = T6     } ;
      {color = Diamond ; value = T7     } ;
      {color = Diamond ; value = T8     } ;
      {color = Diamond ; value = T9     } ;
      {color = Diamond ; value = T10    } ;
      {color = Diamond ; value = Jack   } ;
      {color = Diamond ; value = Queen  } ;
      {color = Diamond ; value = King   } ;
      {color = Diamond ; value = As     }
    ]

    let allClubs = [
      {color = Club ; value = T2        } ;
      {color = Club ; value = T3        } ;
      {color = Club ; value = T4        } ;
      {color = Club ; value = T5        } ;
      {color = Club ; value = T6        } ;
      {color = Club ; value = T7        } ;
      {color = Club ; value = T8        } ;
      {color = Club ; value = T9        } ;
      {color = Club ; value = T10       } ;
      {color = Club ; value = Jack      } ;
      {color = Club ; value = Queen     } ;
      {color = Club ; value = King      } ;
      {color = Club ; value = As        }
    ]

    let all = allSpades@allHearts@allDiamonds@allClubs

    let getValue t = t.value
    let getColor t = t.color

    let toString t = (Value.toString t.value) ^ (Color.toString t.color)
    let toStringVerbose t = "Card(" ^ (Value.toStringVerbose t.value) ^ ", " ^ (Color.toStringVerbose t.color) ^ ")"

    let compare t1 t2 =
      if (Value.toInt (t1.value) < Value.toInt (t2.value)) then ~-1
      else if (Value.toInt (t1.value) > Value.toInt (t2.value)) then 1
      else 0
    let max t1 t2 =
      if (Value.toInt t1.value >= Value.toInt t2.value) then t1
      else t2
    let min t1 t2 =
      if (Value.toInt t1.value > Value.toInt t2.value) then t2
      else t1
    let best l = match l with
      | [] -> invalid_arg "Error"
      | first::remaining  -> List.fold_left max first remaining

    let isOf t color =
      if (t.color = color) then true else false
    let isSpade t = match t.color with
      | Spade   -> true
      | _       -> false
    let isHeart t = match t.color with
      | Heart   -> true
      | _       -> false
    let isDiamond t = match t.color with
      | Diamond -> true
      | _       -> false
    let isClub t = match t.color with
      | Club    -> true
      | _       -> false
end

type t = Card.t list

let rand d:t =
  Random.self_init () ;
  let nd = List.map (fun c -> (Random.bits (), c)) d in
  let sond = List.sort compare nd in
  List.map snd sond

let newDeck () = 
  rand Card.all

let toStringList (t:t) =
  List.map Card.toString t
let toStringListVerbose (t:t) =
  List.map Card.toStringVerbose t

let sliceFirst l:t = match l with
  | first::remaining -> remaining
  | []               -> raise (Failure "Error : deck is empty")
let drawCard (t:t) = (List.hd t, sliceFirst t)