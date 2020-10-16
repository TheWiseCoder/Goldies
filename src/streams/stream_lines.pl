/*******************************************************************************
* FILENAME / MODULE : stream_lines.pl / stream_lines
*
* DESCRIPTION :
*       Read lines as char lists from a given stream, from the current
*       stream position up to, and until, one of two conditions is met:
*       1. the end of the stream is reached, or
*       2. the line read from the stream contains the provided EOS
*
* PUBLIC PREDICATES :
*       stream_lines(+Stream, +EOS, -Lines)
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
* 2020-07-20  GT Nunes          Module creation
* 2020-08-09  GT Nunes          Added conditional compilation for SICStus/SWI
*
*******************************************************************************/

:- module(stream_lines,
    [
        stream_lines/3
    ]).

:- if(current_prolog_flag(dialect, sicstus)).   % SICStus ----------------------

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

:- elif(current_prolog_flag(dialect, swi)).     % SWI-Prolog -------------------

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
    
:- endif.                                       % ------------------------------

:- use_module('../common/atom_marks',
    [
        atoms_codes/2
    ]).

%-------------------------------------------------------------------------------

% (start)
% stream_lines(+Stream, +EOS, -Lines)
% Stream        input stream
% EOS           list of codes signifying end of stream (empty, if n/a)
% Lines         list of lines read from stream
stream_lines(Stream, EOS, Lines) :-

    read_line_to_codes(Stream, LineCodes),
    stream_lines_(Stream, EOS, LineCodes, [], Lines).

% stream_lines_(+Stream, +Line, +LinesProgress, -LinesFinal)
% Stream        input stream handle
% EOS           list of codes signifying end of stream
% LineCodes     current line read
% LinesProgress working list of lines read
% LinesFinal    final list of lines read

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
