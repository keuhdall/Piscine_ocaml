module StringHash =
  struct
    type t = string

    let equal s1 s2 = if (s1 = s2) then true else false

    let hash s =
      let rec doHash i acc =
        let len = String.length s in
        if (i < len) then
          doHash (i + 1) (acc + (int_of_char s.[i] + 42))
        else acc
        in
        doHash 0 0
  end

module StringHashtbl = Hashtbl.Make (StringHash)

let () =
  let ht = StringHashtbl.create 5 in
  let values = [ "Hello"; "world"; "42"; "Ocaml"; "H" ] in
  let pairs = List.map (fun s -> (s, String.length s)) values in
  List.iter (fun (k,v) -> StringHashtbl.add ht k v) pairs;
  StringHashtbl.iter (fun k v -> Printf.printf "k = \"%s\", v = %d\n" k v) ht