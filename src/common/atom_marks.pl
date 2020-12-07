:- module(atom_marks,
    [
        atom_concat3/4,
        atom_concat4/5,
        atom_concat5/6,
        atom_concat6/7,
        atom_concat_number/3,
        atom_contained/2,
        atom_int/3,
        atom_number/2,
        atom_prefix/2,
        atom_sort/2,
        atom_suffix/2,
        atoms_chars/2,
        atoms_codes/2,
        atoms_numbers/2,
        atoms_contained_all/2,
        atoms_contained_any/2,
        chars_lines/2,
        codes_lines/2
    ]).

/** <module> Atom handling utilities

@author GT Nunes
@version 1.0
@copyright (c) 2020 GT Nunes
@license BSD-3-Clause License
*/

%-------------------------------------------------------------------------------------

:- if(current_prolog_flag(dialect, sicstus)).

:- use_module(library(lists),
    [
        maplist/2,
        reverse/2
    ]).

:- elif(current_prolog_flag(dialect, swi)).

:- use_module(library(lists),
    [
        reverse/2
    ]).

:- endif.
  
%! atom_contained(+AtomContainer:atom, +AtomContained:atom) is semidet.
%
%  True if AtomContainer contains AtomContained.
%
%  @param AtomContainer Container atom
%  @param AtomContained Contained atom

atom_contained(AtomContainer, AtomContained) :-
    % fail point
    sub_atom(AtomContainer, _, _, _, AtomContained).

%! atom_prefix(+AtomPrefixed:atom, +AtomPrefix:atom) is semidet.
%
%  True if AtomPrefix is a prefix of AtomPrefixed.
%
%  @param AtomPrefixed The atom under inspection
%  @param AtomPrefix   The candidate leading atom

atom_prefix(AtomPrefixed, AtomPrefix) :-
    % fail point
    sub_atom(AtomPrefixed, 0, _, _, AtomPrefix).

%! atom_suffix(+AtomSuffixed:atom, +AtomSuffix:atom) is semidet
%
%  True if AtomSuffix is a suffix of AtomSuffixed.
%
%  @param AtomSuffixed  the atom being inspected
%  @param AtomSuffix    the candidate leading atom

atom_suffix(AtomSuffixed, AtomSuffix) :-
    % fail point
    sub_atom(AtomSuffixed, _, _, 0, AtomSuffix).

%! atom_int(-Atom:atom, +Int:int, +Len:int) is det.
%
%  Unify Atom with a representation of Int of length Len,
%  left-truncating or left-padding the result with zeros, if necessary.
%
%  @param Atom The resulting integer representation
%  @param Int  The integer value to process
%  @param Len  The length of the result

atom_int(Atom, Int, Len) :-

    atom_number(Anum, Int),
    atom_concat('0000000000', Anum, Apad),
    sub_atom(Apad, _, Len, 0, Atom).

%! atom_number(+Atom:atom, -Number:number) is det.
%! atom_number(-Atom:atom, +Number:number) is det.
%
%  Convert between atom and number, unifying Atom or Number with the result.
%
%  @param Atom   The atom to convert
%  @param Number The number to convert

atom_number(Atom, Number) :-

    (var(Atom) ->
        number_chars(Number, Chars),
        atom_chars(Atom, Chars)
    ;
        atom_chars(Atom, Chars),
        number_chars(Number, Chars)
    ).

%! atom_sort(+Atom:atom, -AtomResult:atom) is det.
%
%  Lexically sort Atom, unifying AtomSorted with the result.
%
%  @param Atom       Atom to be sorted
%  @param AtomResult The sorted atom
atom_sort(Atom, AtomResult) :-

    atom_chars(Atom, Chars),
    sort(Chars, CharsSorted),
    atom_chars(AtomResult, CharsSorted).

%! atom_concat_number(+Atom:atom, +Number:atom, -AtomResult:atom) is det.
%
%  Unify AtomResult with the concatenation of Atom and Number.
%
%  @param Atom       Atom to be concatenated
%  @param Number     Number to be concatenated
%  @param AtomResult The result atom

atom_concat_number(Atom, Number, AtomResult) :-

    atom_number(Num, Number),
    atom_concat(Atom, Num, AtomResult).

%! atom_concat3(+Atom1:atom, +Atom2:atom, +Atom3:atom, -Atom123:atom) is det.
%
%  Concatenate 3 atoms together.
%
%  @param A1   The head atom
%  @param A2   The middle atom
%  @param A3   The tail atom
%  @param A123 The result atom

atom_concat3(A1, A2, A3, A123) :-

    atom_concat(A1, A2, A12),
    atom_concat(A12, A3, A123).

%! atom_concat4(+A1:atom, +A2:atom, +A3:atom, +A4:atom, -A1234:atom) is det.
%
%  Concatenate 4 atoms together.
%
%  @param A1    The head atom
%  @param A2    The 1st middle atom
%  @param A3    The 2nd middle atom
%  @param A4    The tail atom
%  @param A1234 The result atom

atom_concat4(A1, A2, A3, A4, A1234) :-

    atom_concat(A1, A2, A12),
    atom_concat(A3, A4, A34),
    atom_concat(A12, A34, A1234).

%! atom_concat5(+A1:atom, +A2:atom, +A3:atom, +A4:atom, +A5:atom, -A12345:atom) is det.
%
%  Concatenate 5 atoms together.
%
%  @param A1     The head atom
%  @param A2     The 1st middle atom
%  @param A3     The 2nd middle atom
%  @param A4     The 3rd middle atom
%  @param A5     The tail atom
%  @param A12345 The result atom

atom_concat5(A1, A2, A3, A4, A5, A12345) :-

    atom_concat(A1, A2, A12),
    atom_concat(A3, A4, A34),
    atom_concat(A34, A5, A345),
    atom_concat(A12, A345, A12345).

%! atom_concat6(+A1:atom, +A2:atom, +A3:atom, +A4:atom, +A5:atom, +A6:atom, -A123456:atom) is det.
%
%  Concatenate 6 atoms together.
%
%  @param A1      The head atom
%  @param A2      The 1st middle atom
%  @param A3      The 2nd middle atom
%  @param A4      The 3rd middle atom
%  @param A4      The 4th middle atom
%  @param A6      The tail atom
%  @param A123456 The result atom

atom_concat6(A1, A2, A3, A4, A5, A6, A123456) :-

    atom_concat(A1, A2, A12),
    atom_concat(A3, A4, A34),
    atom_concat(A5, A6, A56),
    atom_concat(A34, A56, A3456),
    atom_concat(A12, A3456, A123456).

%-------------------------------------------------------------------------------------

%! atoms_contained_all(+AtomContainer:atom, +AtomsList:list) is semidet.
%
%  True if AtomContainer contains all the atoms in AtomsList.
%
%  @param AtomContainer Container atom
%  @param AtomsList     List of contained atoms

atoms_contained_all(_AtomContainer, []).

atoms_contained_all(AtomContainer, AtomsList) :-
    % fail point
    maplist(atom_contained(AtomContainer), AtomsList).

%! atoms_contained_any(+AtomContainer:atom, +AtomsList:list) is semidet.
%
%  True if AtomContainer contains any of the atoms in AtomsList.
%
%  @param AtomContainer Container atom
%  @param AtomsList     List of contained atoms
  
atoms_contained_any(_AtomContainer, []) :-
    !, fail.

atoms_contained_any(AtomContainer, [Atom|AtomsList]) :-

    (sub_atom(AtomContainer, _, _, _, Atom) ->
        true
    ;
        !,
        % fail point
        atoms_contained_any(AtomContainer, AtomsList)
    ).

%-------------------------------------------------------------------------------------

%! atoms_chars(+Atoms:list, -Chars:list) is det.
%! atoms_chars(-Atoms:list, +Chars:list) is det.
%
%  Unify list of atoms with list of chars. Please, note:
%  ~~~
%  1. as one single atom might yield more than one char, applying one
%     operation followed by its reverse might not restore the original list;
%  2. the operation, as well as its reverse, might yield the same list,
%     as a list of chars is identical to a list of single-char atoms.
%  ~~~
%
%  @param Atoms List of atoms
%  @param Chars List of chars

atoms_chars(Atoms, Chars) :-

    % a list of single-char atoms is identical to a list of chars
    ( (var(Atoms) , Atoms = Chars)
    ; (atoms_chars_(Atoms, [], Chars)) ).

% (done)
atoms_chars_([], CharsFinal, CharsFinal).

% (iterate)
atoms_chars_([Atom|Atoms], CharsProgress, CharsFinal) :-

    % might be a multi-char atom
    atom_chars(Atom, Chars),
    append(CharsProgress, Chars, CharsRevised),
    atoms_chars_(Atoms, CharsRevised, CharsFinal).

%-------------------------------------------------------------------------------------

%! atoms_codes(+Atoms:list, -Codes:list) is det.
%! atoms_codes(-Atoms:list, +Codes:list) is det.
%
%  Unify list of atoms with list of codes.
%
%  Please, note:
%  ~~~
%  1. as one single atom might yield more than one code, applying one
%     operation followed by its reverse might not restore the original list;
%  2. the operation, as well as its reverse, can be applied to lists of chars,
%     as a list of chars is identical to a list of single-char atoms.
%  ~~~
%
%  @param Atoms List of atoms
%  @param Codes List of codes

atoms_codes(Atoms, Codes) :-

    ( (var(Atoms) , codes_atoms_(Codes, [], Atoms))
    ; (var(Codes) , atoms_codes_(Atoms, [], Codes)) ).

% (done)
atoms_codes_([], CodesFinal, CodesFinal).

% (iterate)
atoms_codes_([Atom|Atoms], CodesProgress, CodesFinal) :-

    % might be a multi-code atom
    atom_codes(Atom, Codes),
    append(CodesProgress, Codes, CodesRevised),
    atoms_codes_(Atoms, CodesRevised, CodesFinal).

% (done)
codes_atoms_([], AtomsProgress,  AtomsFinal) :-
    reverse(AtomsProgress, AtomsFinal).

% (iterate)
codes_atoms_([Code|Codes], AtomsProgress, AtomsFinal) :-

    % single-code atom
    atom_codes(Atom, [Code]),
    codes_atoms_(Codes, [Atom|AtomsProgress], AtomsFinal).

%-------------------------------------------------------------------------------------

%! atoms_numbers(+Atoms, +Numbers) is det.
%! atoms_numbers(-Atoms, +Numbers) is det.
%
%  Unify list of atoms with list of numbers they represent.
%  Note that the operation, as well as its reverse, can be applied to
%  lists of chars, as a list of chars is identical to a list of single-char atoms
%  (in that case, the range is [0,9])
%
%  @param Atoms   List of atoms
%  @param Numbers List of numbers

atoms_numbers(Atoms, Numbers) :-

    ( (var(Atoms) , numbers_atoms_(Numbers, [], Atoms))
    ; (var(Numbers) , atoms_numbers_(Atoms, [], Numbers)) ).

% (done)
atoms_numbers_([], NumbersProgress, NumbersFinal) :-
    reverse(NumbersProgress, NumbersFinal).

% (iterate)
atoms_numbers_([Atom|Atoms], NumbersProgress, NumbersFinal) :-

    atom_number(Atom, Number),
    atoms_numbers_(Atoms, [Number|NumbersProgress], NumbersFinal).

% (done)
numbers_atoms_([], AtomsProgress, AtomsFinal) :-
    reverse(AtomsProgress, AtomsFinal).

% (iterate)
numbers_atoms_([Number|Numbers], AtomsProgress, AtomsFinal) :-

    % single-code atom
    atom_codes(Atom, Number),
    numbers_atoms_(Numbers, [Atom|AtomsProgress], AtomsFinal).

%-------------------------------------------------------------------------------------

%! chars_lines(+Chars:list, -Lines:list) is det.
%! chars_lines(-Chars:list, +Lines:list) is det.
%
%  Unify list of chars with lists of lines, each line itself a list of chars.
%  Both UNIX (LF or \n) and Windows (CRLF or \r\n) style line separators
%  are considered. When building Chars from Lines, the OS-specific line
%  terminator is used.</br>
%  HAZARD: lone CRs will be suppressed, without originating new lines.
%
%  @param Chars List of chars
%  @param Lines List of lists of chars

chars_lines([], Lines) :- Lines = [].
chars_lines(Chars, []) :- Chars = [].

chars_lines(Chars, Lines) :-

    ( ( var(Chars)
      , current_prolog_flag(os_data,  os(Os, _, _))
      , ((Os = windows , Ls = ['\r','\n']) ; Ls = ['\n'])
      , lines_chars_(Lines, Ls, [], Chars) )
    ; (var(Lines) , chars_lines_(Chars, [[]], Lines)) ).

% (done)
chars_lines_([], LinesProgress, LinesFinal) :-
    reverse(LinesProgress, LinesFinal).

% suppress CR
chars_lines_(['\r'|Chars], LinesProgress, LinesFinal) :-
    chars_lines_(Chars, LinesProgress, LinesFinal).

% originate new empty line
chars_lines_(['\n'|Chars], LinesProgress, LinesFinal) :-
    chars_lines_(Chars, [[]|LinesProgress], LinesFinal).

% add char to current line, in the proper order
chars_lines_([Char|Chars], [Head|Tail], LinesFinal) :-

    append(Head, [Char], HeadNew),
    chars_lines_(Chars, [HeadNew|Tail], LinesFinal).

% (done)
lines_chars_([], Ls, CharsProgress, CharsFinal) :-

    [_|CharsRevised] = CharsProgress,
    ( (length(Ls, 2) , [_|CharsFinal] = CharsRevised)
      ; CharsFinal = CharsRevised ).

% iterate
lines_chars_([Line|Lines], Ls, CharsProgress, CharsFinal) :-

    append(Ls, Line, Chars1),
    append(CharsProgress, Chars1, Chars2),
    lines_chars_(Lines, Ls, Chars2, CharsFinal).

%-------------------------------------------------------------------------------------

%! codes_lines(+Codes:list, -Lines:list) is det.
%! codes_lines(-Codes:list, +Lines:list) is det.
%
%  Unify list of codes with lists of lines, each line itself a list of codes.
%  Both UNIX (LF or \n) and Windows (CRLF or \r\n) style line separators
%  are considered. When building Codes from Lines, the OS-specific line
%  terminator is used.</br>
%  HAZARD: lone CRs will be suppressed, without originating new lines.
%
%  @param Codes List of codes
%  @param Lines List of lists of codes

codes_lines([], Lines) :- Lines = [].
codes_lines(Codes, []) :- Codes = [].

codes_lines(Codes, Lines) :-

    ( ( var(Codes)
      , current_prolog_flag(os_data,  os(Os, _, _))
      , ((Os = windows , Ls = [13,10]) ; Ls = [10])
      , lines_codes_(Lines, Ls, [], Codes) )
    ; (var(Lines) , codes_lines_(Codes, [[]], Lines)) ).

% (done)
codes_lines_([], CodesProgress, CodesFinal) :-
    reverse(CodesProgress, CodesFinal).

% suppress CR
codes_lines_([13|Codes], LinesProgress, LinesFinal) :-
    codes_lines_(Codes, LinesProgress, LinesFinal).

% originate new empty line
codes_lines_([10|Codes], LinesProgress, LinesFinal) :-
    codes_lines_(Codes, [[]|LinesProgress], LinesFinal).

% add code to current line, in the proper order
codes_lines_([Code|Codes], [Head|Tail], LinesFinal) :-

    append(Head, [Code], HeadNew),
    codes_lines_(Codes, [HeadNew|Tail], LinesFinal).

% (done)
lines_codes_([], Ls, CodesProgress, CodesFinal) :-

    [_|CodesRevised] = CodesProgress,
    ( (length(Ls, 2) , [_|CodesFinal] = CodesRevised)
      ; CodesFinal = CodesRevised ).

% iterate
lines_codes_([Line|Lines], Ls, CodesProgress, CodesFinal) :-

    append(Ls, Line, Codes1),
    append(CodesProgress, Codes1, Codes2),
    lines_codes_(Lines, Ls, Codes2, CodesFinal).
