:- module(stream_lines,
    [
        stream_lines/3
    ]).

/** <module> Read lines as char lists from a given stream

Read from the current stream position up to, and until,
one of two conditions is met:<br/>
1- the end of the stream is reached, or<br/>
2- the line read from the stream contains the provided EOS.

@author GT Nunes
@version 1.0
@copyright (c) 2020 GT Nunes
@license BSD-3-Clause License
*/

%-------------------------------------------------------------------------------------

:- if(current_prolog_flag(dialect, sicstus)).   % SICStus ----------------------------

:- use_module(library(lists),
    [
        reverse/2,
        sublist/3,
        sublist/4
    ]).

:- use_module('../sicstus/port_layer',
    [
        read_line_to_codes/2
    ]).

:- elif(current_prolog_flag(dialect, swi)).     % SWI-Prolog -------------------------

:- use_module(library(lists),
    [
        reverse/2
    ]).

:- use_module(library(readutil),
    [
        read_line_to_codes/2
    ]).

:- use_module('../swi/port_lists',
    [
        sublist/3,
        sublist/4
    ]).
    
:- endif.                                       % ------------------------------------

:- use_module('../common/atom_marks',
    [
        atoms_codes/2
    ]).

%-------------------------------------------------------------------------------------

%! stream_lines(+Stream:ref, +EOS:list, -Lines:list) is det.
%
%  @param Stream The input stream
%  @param EOS    List of codes signifying end of stream (empty, if n/a)
%  @param Lines  List of lines read from stream

stream_lines(Stream, EOS, Lines) :-

    read_line_to_codes(Stream, LineCodes),
    stream_lines_(Stream, EOS, LineCodes, [], Lines).

% (done)
stream_lines_(_Stream, _EOS, end_of_file, LinesProgress, LinesFinal) :-
    reverse(LinesProgress, LinesFinal).

% (iterate, add line to lines list)
stream_lines_(Stream, EOS, LineCodes, LinesProgress, LinesFinal) :-

    ( LineCodes = EOS ->
        stream_lines_(Stream, EOS, end_of_file,
                      LinesProgress, LinesFinal)
    ; (EOS \= [] , sublist(LineCodes, EOS, Pos)) ->
        (Pos = 0 ->
            stream_lines_(Stream, EOS, end_of_file,
                          LinesProgress, LinesFinal)
        ;
            sublist(LineCodes, LastCodes, 0, Pos),
            atoms_codes(LineChars, LastCodes),
            stream_lines_(Stream, EOS, end_of_file,
                          [LineChars|LinesProgress], LinesFinal)
        )
    ;
        atoms_codes(LineChars, LineCodes),
        read_line_to_codes(Stream, NewCodes),
        stream_lines_(Stream, EOS, NewCodes,
                      [LineChars|LinesProgress], LinesFinal)
    ).
