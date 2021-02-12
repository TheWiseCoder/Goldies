:- module(stream_atoms,
    [
        stream_atoms/2,
        stream_atoms/3
    ]).

/** <module> Read/write a list of atoms from/to a given stream

When reading from the stream, the invoker may provide a EOS (end-of-stream) marker
to limit the reading, or the EOF (end_of_file) atom to read to the end of the stream.
When writing to the stream, the invoker may provide a EOS to limit the writing,
or EOF to write all atoms in the list of atoms provided. A EOS marker may be any
atom suitable as an indication of the end of the stream operation.

@author GT Nunes
@version 1.2
@copyright (c) TheWiseCoder 2020-2021
@license BSD-3-Clause License
*/

%-------------------------------------------------------------------------------------

:- if(current_prolog_flag(dialect, sicstus)).

:- use_module(library(lists),
    [
        reverse/2
    ]).

:- use_module('../sicstus/port_layer',
    [
        read_line_to_codes/2
    ]).

:- elif(current_prolog_flag(dialect, swi)).

:- use_module(library(lists),
    [
        reverse/2
    ]).

:- use_module(library(readutil),
    [
        read_line_to_codes/2
    ]).
    
:- endif.

:- use_module('stream_chars',
    [
        stream_chars/3
    ]).

%-------------------------------------------------------------------------------------

%! stream_atoms(+Stream:ref, -Atoms:list) is det.
%! stream_atoms(+Stream:ref, +Atoms:list) is det.
%
%  Read/write atoms from/to stream.
%  If Atoms is not grounded, read from Stream up to the end of the stream.
%  If Atoms is grounded, write all atoms in Atoms to Stream.
%
%  @param Stream The input/output stream
%  @param Atoms  List of atoms read from, or to write to, the stream

stream_atoms(Stream, Atoms) :-

    (var(Atoms) ->
        read_line_to_codes(Stream, LineCodes),
        stream_read(Stream, end_of_file, LineCodes, [], Atoms)
    ;
        stream_write(Stream, end_of_file, Atoms)
    ).

%-------------------------------------------------------------------------------------

%! stream_atoms(+Stream:ref, +EOS:list, -Atoms:list) is det.
%! stream_atoms(+Stream:ref, +EOS:list, +Atoms:list) is det.
%
%  Read/write atoms from/to stream.
%  If Atoms is not grounded, read from Stream until an EOS atom is found.
%  For EOS = end_of_file, read to the end of the stream.
%  If Atoms is grounded, write atoms to Stream until an EOS atom is found.
%  For EOS = end_of_file, write all atoms in Atoms.
%
%  @param Stream The input/output stream
%  @param EOS    EOF or list of chars signifying end of stream
%  @param Atoms  List of atoms read from, or to write to, the stream

stream_atoms(Stream, EOS, Atoms) :-

    (var(Atoms) ->
        read_line_to_codes(Stream, LineCodes),
        stream_read(Stream, EOS, LineCodes, [], Atoms)
    ;
        stream_write(Stream, EOS, Atoms)
    ).

%-------------------------------------------------------------------------------------

%! stream_read(+Stream:ref, +EOS:list, +LineCodes:list, +AtomsProgress:list, -AtomsFinal:list) is det.
%
%  Read from Stream until an atom equal to EOS is found.
%  For EOS = end_of_file, read to the end of the stream.
%
%  @param Stream        The input stream
%  @param EOS           EOF or atom signifying end of stream
%  @param LineCodes     Line currently read from the stream, as a list of codes
%  @param AtomsProgress Working list of atoms read from the stream
%  @param AtomsFinal    Final list of atoms read from the stream

% (done)
stream_read(_Stream, _EOS, end_of_file, AtomsProgress, AtomsFinal) :-
    reverse(AtomsProgress, AtomsFinal).

% (iterate, adding atom to atoms list)
stream_read(Stream, EOS, LineCodes, AtomsProgress, AtomsFinal) :-

    % convert codes to atom in current line
    atom_codes(Atom, LineCodes),

    (Atom = EOS ->
        NewCodes = end_of_file,
        AtomsRevised = AtomsProgress
    ;
        AtomsRevised = [Atom|AtomsProgress],
        read_line_to_codes(Stream, NewCodes)
    ),

    % go for the next atom
    stream_read(Stream, EOS, NewCodes, AtomsRevised, AtomsFinal).

%-------------------------------------------------------------------------------------

%! stream_write(+Stream:ref, +EOS:list, +Atoms:list) is det.
%
%  Write Atoms to Stream until an atom equal to EOS is found.
%  For EOS = end_of_file, write all lines in Atoms to the stream.
%
%  @param Stream The output stream
%  @param EOS    EOF or atom signifying end of stream
%  @param Atoms  List of atoms to write to the stream

% (done)
stream_write(_Stream, _EOS, []).

% (start)
stream_write(Stream, EOS, [Atom|Atoms]) :-

    % has EOS been found ?
    (Atom = EOS ->
        % yes, so signal end of stream
        AtomsRevised = []
    ;
        % no, so write current atom, followed by a new line
        atom_chars(Atom, Chars),
        stream_chars(Stream, -1, Chars),
        put_char(Stream, '\n'),
        AtomsRevised = Atoms
    ),

    % go for the next atom
    stream_write(Stream, EOS, AtomsRevised).
