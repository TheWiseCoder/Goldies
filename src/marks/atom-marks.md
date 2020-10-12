Small utilities handling atoms. The available predicates are:

- `atom_sort(+Atom, -AtomSorted)` - lexically sort *Atom*, unifying *AtomSorted* with the result
- `atom_concat3(+Atom1, +Atom2, +Atom3, -Atom123)` - concatenate 3 atoms together
- `atom_concat4(+Atom1, +Atom2, +Atom3, +Atom4, -Atom1234)` - concatenate 4 atoms together
- `atom_concat5(+Atom1, +Atom2, +Atom3, +Atom4, +Atom5, -Atom12345)` - concatenate 5 atoms together
- `atom_concat6(+Atom1, +Atom2, +Atom3, +Atom4, +Atom5, +Atom6, -Atom123456)` - concatenate 6 atoms together
- `atom_concat_number(+Atom, +Number, -AtomNumber)` - concatenate an atom and a number to a new atom
- `atom_contained(+AtomContainer, +AtomContained)` - succeed if *AtomContainer* contains *AtomContained*
- `atom_number(?Atom, ?Number)` - convert between atom and number
- `atom_prefix(+AtomPrefixed, +AtomPrefix)` - succeed if *AtomPrefix* is a prefix of *AtomPrefixed*
- `atom_suffix(+AtomSuffixed, +AtomSuffix)` - succeed if *AtomSuffix* is a suffix of *AtomSuffixed*
- `atoms_chars(?Atoms, ?Chars)` - unify list of atoms with list of chars (see note in source code file)
- `atoms_codes(?Atoms, ?Codes)` - unify list of atoms with list of codes (see note in source code file)
- `atoms_numbers(?Atoms, ?Numbers)` -  unify list of atoms with list of numbers they represent
- `atoms_contained_all(+AtomsContainer, +AtomsList)` - succeed if *AtomContainer* contains all the atoms in *AtomsList*
- `atoms_contained_any(+AtomsContainer, +AtomsList)` - succeed if *AtomContainer* contains any of the atoms in *AtomsList*
- `chars_lines(?Chars, ?Lines)` - unify list of chars with lists of lines, each line itself a list of chars
- `codes_lines(?Chars, ?Codes)` - unify list of codes with lists of lines, each line itself a list of codes.

Additionally, you may peruse the documentation and code on the source code file.