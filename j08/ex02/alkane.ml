class virtual alkane (n:int) =
  object (this)
    inherit Molecule.molecule (match n with
                                | 1   ->  "Methane"
                                | 2   ->  "Ethane"
                                | 3   ->  "Propane"
                                | 4   ->  "Butane"
                                | 5   ->  "Pentane"
                                | 6   ->  "Hexane"
                                | 7   ->  "Heptane"
                                | 8   ->  "Octane"
                                | 9   ->  "Nonane"
                                | 10  ->  "Decane"
                                | 11  ->  "Undecane"
                                | 12  ->  "Dodecane"
                                | _   ->  "Go fuck yourself")
                                
                                (let rec getAtomListLoop n2 (l:Atom.atom list) =
                                  if (n2 < n) then
                                    getAtomListLoop (n2 + 1) (((new Atom.carbon)::(new Atom.hydrogen)::(new Atom.hydrogen)::[])@l)
                                  else
                                    ((new Atom.hydrogen)::(new Atom.hydrogen)::[])@l
                                in getAtomListLoop 0 [])
end

class methane =
object
  inherit alkane 1
end

class ethane =
object
  inherit alkane 2
end

class octane =
object
  inherit alkane 8
end