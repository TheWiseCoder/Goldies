:- module(stream_atoms,
    [
        stream_atoms/3
    ]).

/** <module> Read/write a list of atoms from/to a given stream

For reading from the stream, the invoker must provide a EOS (end-of-stream) marker,
or end_of_file to read to the end of the stream.
For writing to the stream, the invoker must provide a EOS, or  end_of_file to write
all atoms in the list of atoms given. 

@author GT Nunes
@version 1.1
@copyright (c) 2020 GT Nunes
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

%! stream_atoms(+Stream:ref, +EOS:list, -Atoms:list) is det.
%! stream_atoms(+Stream:ref, +EOS:list, +Atoms:list) is det.
%
%  If Atoms is grounded, write atoms to Stream until an EOS atom is found.
%  For EOS = end_of_file, write all atoms in Atoms
%  Otherwise, read from Stream until an EOS atom is found.
%  For EOS = end_of_file, read to the end of the stream.
%
%  @param Stream The input/output stream
%  @param EOS    Atom signifying end of stream (empty, if n/a)
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
%  @param EOS           List of codes signifying end of stream, if applicable
%  @param LineCodes     Line currently read from the stream
%  @param AtomsProgress Working list of atoms read from the stream
%  @param AtomsFinal    Final list of atoms read from the stream

% (done)
stream_read(_Stream, _EOS, end_of_file, AtomsProgress, AtomsFinal) :-
    reverse(AtomsProgress, AtomsFinal).

% (iterate, adding line to lines list)
stream_read(Stream, EOS, LineCodes, AtomsProgress, AtomsFinal) :-

    (LineCodes = EOS ->
        NewCodes = end_of_file,
        AtomsRevised = AtomsProgress
    ;
        atom_codes(Atom, LineCodes),
        AtomsRevised = [Atom|AtomsProgress],
        read_line_to_codes(Stream, NewCodes)
    ),

    % go for the next line
    stream_read(Stream, EOS, NewCodes, AtomsRevised, AtomsFinal).

%-------------------------------------------------------------------------------------

%! stream_write(+Stream:ref, +EOS:list, +Atoms:list) is det.
%
%  Write Atoms to Stream until an atom equal to EOS is found.
%  For EOS = end_of_file, write all lines in Atoms to the stream.
%
%  @param Stream The output stream
%  @param EOS    Atom signifying end of stream, if applicable
%  @param Atoms  List of atoms to write to the stream

% (done)
stream_write(_Stream, _EOS, end_of_file).

% (start)
stream_write(Stream, EOS, [Atom|Atoms]) :-

    % has EOS been found ?
    (Atom = EOS ->
        % yes, so signal end-of-stream
        AtomsAdjusted = end_of_file
    ;
        % no, so write current line, followed by a new line
        atom_chars(Atom, Chars),
        stream_chars(Stream, -1, Chars),
        put_char(Stream, '\n'),
        AtomsAdjusted = Atoms
    ),

    % go for the next line
    stream_write(Stream, EOS, AtomsAdjusted).
