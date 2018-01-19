class virtual molecule (name:string) (atoms:Atom.atom list) =
  object (this)
    method name = name
    method atoms = atoms
    method formula = (this#compute_formula atoms)

    method private compare_atoms (a: Atom.atom) (b: Atom.atom) =
      String.compare a#symbol b#symbol

      method private hill_notation atoms =
      let rec find_atom atoms nb (to_find: string) = match atoms with
        | [] -> ""
        | head::next::tail ->
          if (head#symbol = to_find) then
            begin
              if (head#symbol = next#symbol) then find_atom (next::tail) (nb + 1) to_find
              else if nb = 0 then to_find
              else to_find ^ (string_of_int (nb + 1))
            end
          else find_atom (next::tail) nb to_find
        | head::[] ->
            if (head#symbol = to_find) then
              begin
                if (nb = 0) then to_find
                else to_find ^ (string_of_int (nb + 1))
              end
            else ""
      in
      let rec tail_hill atoms nb find_h ret = match atoms with
      | [] -> ret
      | head::next::tail ->
              if (head#symbol = "C" || (find_h = false && head#symbol = "H")) then tail_hill (next::tail) nb find_h ret
              else if (head#equals next) then tail_hill (next::tail) (nb + 1) find_h ret
              else if (nb = 0) then tail_hill (next::tail) 0 find_h (ret ^ head#symbol)
              else tail_hill (next::tail) 0 find_h (ret ^ head#symbol ^ (string_of_int (nb + 1)))
      | head::[] ->
              if (head#symbol = "C" || (find_h = false && head#symbol = "H")) then ret
              else if (nb = 0) then ret ^ head#symbol
              else ret ^ head#symbol ^ (string_of_int (nb + 1))
      in
      let carbon = find_atom atoms 0 "C" in
      if carbon = "" then (tail_hill atoms 0 true "")
      else carbon ^ (find_atom atoms 0 "H") ^ (tail_hill atoms 0 false "")

    method private compute_formula (atoms: Atom.atom list) = match atoms with
      | [] -> ""
      | head::tail -> this#hill_notation (List.sort this#compare_atoms atoms)

    method to_string = "Molecule of " ^ this#name ^ "(" ^ this#formula ^ ")"

    method equals (that:molecule) = (this#name = that#name) && (this#formula = that#formula)
end

class water =
  object
    inherit molecule "Water" [new Atom.hydrogen ; new Atom.oxygen ; new Atom.hydrogen]
end

class carbon_dioxyde =
  object
    inherit molecule "Carbon Dioxyde" [new Atom.oxygen ; new Atom.oxygen ; new Atom.carbon]
end

class serotonin =
  object
    inherit molecule "Serotonin" [new Atom.oxygen ; new Atom.nitrogen ; new Atom.nitrogen ;
    new Atom.hydrogen ; new Atom.hydrogen ; new Atom.hydrogen ; new Atom.hydrogen ; new Atom.hydrogen ;
    new Atom.hydrogen ; new Atom.hydrogen ; new Atom.hydrogen ; new Atom.hydrogen ; new Atom.hydrogen ;
    new Atom.hydrogen ; new Atom.hydrogen ;
    new Atom.carbon ; new Atom.carbon ; new Atom.carbon ; new Atom.carbon ; new Atom.carbon ;
    new Atom.carbon ; new Atom.carbon ; new Atom.carbon ; new Atom.carbon ; new Atom.carbon]
end

class cafein =
object
  inherit molecule "Cafein" [new Atom.oxygen ; new Atom.oxygen ;
  new Atom.nitrogen ; new Atom.nitrogen ; new Atom.nitrogen ; new Atom.nitrogen ;
  new Atom.carbon ; new Atom.carbon ; new Atom.carbon ; new Atom.carbon ;
  new Atom.carbon ; new Atom.carbon ; new Atom.carbon ; new Atom.carbon ;
  new Atom.hydrogen ; new Atom.hydrogen ; new Atom.hydrogen ; new Atom.hydrogen ; new Atom.hydrogen ;
  new Atom.hydrogen ; new Atom.hydrogen ; new Atom.hydrogen ; new Atom.hydrogen ; new Atom.hydrogen]
end

class venlafaxine =
object
  inherit molecule "Venlafaxine" [new Atom.oxygen ; new Atom.oxygen ; new Atom.nitrogen ;
  new Atom.carbon ; new Atom.carbon ; new Atom.carbon ; new Atom.carbon ; new Atom.carbon ;
  new Atom.carbon ; new Atom.carbon ; new Atom.carbon ; new Atom.carbon ; new Atom.carbon ; new Atom.carbon ;
  new Atom.carbon ; new Atom.carbon ; new Atom.carbon ; new Atom.carbon ; new Atom.carbon ; new Atom.carbon ;
  new Atom.hydrogen ; new Atom.hydrogen ; new Atom.hydrogen ; new Atom.hydrogen ; new Atom.hydrogen ;
  new Atom.hydrogen ; new Atom.hydrogen ; new Atom.hydrogen ; new Atom.hydrogen ; new Atom.hydrogen ;
  new Atom.hydrogen ; new Atom.hydrogen ; new Atom.hydrogen ; new Atom.hydrogen ; new Atom.hydrogen ;
  new Atom.hydrogen ; new Atom.hydrogen ; new Atom.hydrogen ; new Atom.hydrogen ; new Atom.hydrogen ;
  new Atom.hydrogen ; new Atom.hydrogen ; new Atom.hydrogen ; new Atom.hydrogen ; new Atom.hydrogen ;
  new Atom.hydrogen ; new Atom.hydrogen]
end