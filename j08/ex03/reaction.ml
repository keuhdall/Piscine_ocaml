class virtual reaction (l1:Molecule.molecule list) (l2:Molecule.molecule list) =
  object (this)
  method virtual get_start: (Molecule.molecule list * int)
  method virtual get_result: (Molecule.molecule list * int)
  method virtual balance : reaction
  method virtual is_balanced : bool
end