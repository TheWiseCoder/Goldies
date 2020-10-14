/*******************************************************************************
* FILENAME / MODULE : atom-marks.pl / atom_marks
*
* DESCRIPTION :
*       Atom handling utilities.
*
* PUBLIC PREDICATES :
*       atom_concat3(+Atom1, +Atom2, +Atom3, -Atom123)
*       atom_concat4(+Atom1, +Atom2, +Atom3, +Atom4, -Atom1234)
*       atom_concat5(+Atom1, +Atom2, +Atom3, +Atom4, +Atom5, -Atom12345)
*       atom_concat6(+Atom1, +Atom2, +Atom3,
*                    +Atom4, +Atom5, +Atom6, -Atom123456)
*       atom_concat_number(+Atom, +Number, -AtomNumber)
*       atom_contained(+AtomContainer, +AtomContained)
*       atom_int(?Atom, ?Int, +Len)
*       atom_number(?Atom, ?Number)
*       atom_prefix(+AtomPrefixed, +AtomPrefix)
        atom_sort(+Atom, -AtomSorted)
*       atom_suffix(+AtomSuffixed, +AtomSuffix)
*       atoms_chars(?Atoms, ?Chars)
*       atoms_codes(?Atoms, ?Codes)
*       atoms_numbers(?Atoms, ?Numbers)
*       atoms_contained_all(+AtomsContainer, +AtomContained)
*       atoms_contained_any(+AtomsContainer, +AtomContained)
*       chars_lines(?Chars, ?Lines)
*       codes_lines(?Chars, ?Codes)
*
* NOTES :
*       None yet.
*
*       Copyright TheWiseCoder 2020.  All rights reserved.
*
* REVISION HISTORY :
*
* DATE        AUTHOR            REVISION
* ----------  ----------------  ------------------------------------------------
* 2020-05-10  GT Nunes          Module creation
* 2020-08-09  GT Nunes          Added conditional compilation for SICStus/SWI
*
*******************************************************************************/

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

:- if(current_prolog_flag(dialect, sicstus)).   % SICStus ----------------------

:- use_module(library(lists),
    [
        maplist/2,
        reverse/2
    ]).

:- elif(current_prolog_flag(dialect, swi)).     % SWI-Prolog -------------------

:- use_module(library(lists),
    [
        reverse/2
    ]).

:- endif.                                       % ------------------------------
  
% succeed if AtomContainer contains AtomContained
% atom_contained(+AtomContainer, +AtomContained)
% AtomContainer         atom container
% AtomContained         contained atom
atom_contained(AtomContainer, AtomContained) :-
    % fail point
    sub_atom(AtomContainer, _, _, _, AtomContained).

% succeed if AtomPrefix is a prefix of AtomPrefixed
% atom_prefix(+AtomPrefixed, +AtomPrefix)
% AtomPrefixed  the atom being inspected
% AtomPrefix    the candidate leading atom
atom_prefix(AtomPrefixed, AtomPrefix) :-
    % fail point
    sub_atom(AtomPrefixed, 0, _, _, AtomPrefix).

% succeed if AtomSuffix is a suffix of AtomSuffixed
% atom_suffix(+AtomSuffixed, +AtomSuffix)
% AtomSuffixed  the atom being inspected
% AtomSuffix    the candidate leading atom
atom_suffix(AtomSuffixed, AtomSuffix) :-
    % fail point
    sub_atom(AtomSuffixed, _, _, 0, AtomSuffix).

% unify an integer with an atom of given length,
% left-truncating or left-padding the latter with zeros, if necessary
% atom_int(?Atom, ?Int, +Len)
atom_int(Atom, Int, Len) :-

    atom_number(Anum, Int),
    atom_concat('0000000000', Anum, Apad),
    sub_atom(Apad, _, Len, 0, Atom).

% convert between atom and number
% atom_number(?Atom, ?Number)
atom_number(Atom, Number) :-

    (var(Atom) ->
        number_chars(Number, Chars),
        atom_chars(Atom, Chars)
    ;
        atom_chars(Atom, Chars),
        number_chars(Number, Chars)
    ).

% lexically sort Atom, unifying AtomSorted with the result
% atom_sort(+Atom, -AtomSorted)
% Atom          atom to be sorted
% AtomSorted    the sorted atom
atom_sort(Atom, AtomSorted) :-

    atom_chars(Atom, Chars),
    sort(Chars, CharsSorted),
    atom_chars(AtomSorted, CharsSorted).

% concatenate an atom and a number to a new atom
atom_concat_number(Atom, Number, AtomNumber) :-

    atom_number(Num, Number),
    atom_concat(Atom, Num, AtomNumber).

% concatenate 3 atoms together
% atom_concat3(+Atom1, +Atom2, +Atom3, -Atom123)
% Atom1         the head atom
% Atom2         the middle atom
% Atom3         the tail atom
% Atom123       the final atom
atom_concat3(Atom1, Atom2, Atom3, Atom123) :-

    atom_concat(Atom1, Atom2, Atom12),
    atom_concat(Atom12, Atom3, Atom123).

% concatenate 4 atoms together
% atom_concat4(+Atom1, +Atom2, +Atom3, +Atom4, -Atom1234)
% Atom1         the head atom
% Atom2         the 1st middle atom
% Atom3         the 2nd middle atom
% Atom4         the tail atom
% Atom1234      the final atom
atom_concat4(Atom1, Atom2, Atom3, Atom4, Atom1234) :-

    atom_concat(Atom1, Atom2, Atom12),
    atom_concat(Atom3, Atom4, Atom34),
    atom_concat(Atom12, Atom34, Atom1234).

% concatenate 5 atoms together
% atom_concat5(+Atom1, +Atom2, +Atom3, +Atom4, +Atom5, -Atom12345)
% Atom1         the head atom
% Atom2         the 1st middle atom
% Atom3         the 2nd middle atom
% Atom4         the 3rd middle atom
% Atom5         the tail atom
% Atom12345     the final atom
atom_concat5(Atom1, Atom2, Atom3, Atom4, Atom5, Atom12345) :-

    atom_concat(Atom1, Atom2, Atom12),
    atom_concat(Atom3, Atom4, Atom34),
    atom_concat(Atom34, Atom5, Atom345),
    atom_concat(Atom12, Atom345, Atom12345).

% concatenate 6 atoms together
% atom_concat6(+Atom1, +Atom2, +Atom3, +Atom4, +Atom5, +Atom6, -Atom123456)
% Atom1         the head atom
% Atom2         the 1st middle atom
% Atom3         the 2nd middle atom
% Atom4         the 3rd middle atom
% Atom5         the 4th middle atom
% Atom6         the tail atom
% Atom123456    the final atom
atom_concat6(Atom1, Atom2, Atom3, Atom4, Atom5, Atom6, Atom123456) :-

    atom_concat(Atom1, Atom2, Atom12),
    atom_concat(Atom3, Atom4, Atom34),
    atom_concat(Atom5, Atom6, Atom56),
    atom_concat(Atom34, Atom56, Atom3456),
    atom_concat(Atom12, Atom3456, Atom123456).

%-------------------------------------------------------------------------------
  
% succeed if AtomContainer contains all the atoms in AtomsList
% atoms_contained_all(+AtomContainer, +AtomsList)
% AtomContainer     atom container
% AtomsList         list of contained atoms
  
atoms_contained_all(_AtomContainer, []).

atoms_contained_all(AtomContainer, AtomsList) :-
    % fail point
    maplist(atom_contained(AtomContainer), AtomsList).

%-------------------------------------------------------------------------------

% succeed if AtomContainer contains any of the atoms in AtomsList
% atoms_contained_any(+AtomContainer, +AtomsList)
% AtomContainer     atoms container
% AtomsList         list of contained atoms
  
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

%-------------------------------------------------------------------------------
% Unify list of atoms with list of chars. Note:

%   1. as one single atom might yield more than one char, applying one
%      operation followed by its reverse might not restore the original list
%   2. the operation, as well as its reverse, might yield the same list,
%      as a list of chars is identical to a list of single-char atoms

% atoms_chars(?Atoms, ?Chars)
% Atoms         list of atoms
% Chars         list of chars
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

%-------------------------------------------------------------------------------
% Unify list of atoms with list of codes. Note:

%   1. as one single atom might yield more than one code, applying one
%      operation followed by its reverse might not restore the original list
%   2. the operation, as well as its reverse, can be applied to lists of chars,
%      as a list of chars is identical to a list of single-char atoms

% atoms_codes(?Atoms, ?Codes)
% Atoms         list of atoms
% Chars         list of codes
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

%-------------------------------------------------------------------------------
% Unify list of atoms with list of numbers they represent. Note that the
% operation, as well as its reverse, can be applied to lists of chars,
% as a list of chars is identical to a list of single-char atoms
% (in that case the range is [0,9])

% atoms_numbers(?Atoms, ?Numbers)
% Atoms         list of atoms
% Numbers       list of numbers
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

%-------------------------------------------------------------------------------
% Unify list of chars with lists of lines, each line itself a list of chars.
% Both UNIX (LF or \n) and Windows (CRLF or \r\n) style line separators
% are considered. When building Chars from Lines, the OS-specific line
% terminator is used.
% HAZARD: lone CRs will be suppressed, without originating new lines

chars_lines([], Lines) :- Lines = [].
chars_lines(Chars, []) :- Chars = [].

% chars_lines(?Chars, ?Lines)
% Chars         list of chars
% Lines         list of lists of chars
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


%-------------------------------------------------------------------------------
% Unify list of codes with lists of lines, each line itself a list of codes.
% Both UNIX (LF or \n) and Windows (CRLF or \r\n) style line separators
% are considered. When building Codes from Lines, the OS-specific line
% terminator is used.
% HAZARD: lone CRs will be suppressed, without originating new lines

codes_lines([], Lines) :- Lines = [].
codes_lines(Codes, []) :- Codes = [].

% codes_lines(?Codes, ?Lines)
% Codes         list of codes
% Lines         list of lists of codes
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
