:- module(stream_ints,
    [
        stream_ints/2
    ]).

/** <module> Read/write a list of integers from/to a given stream

 
Integers are read from the stream one per line, to the end of the stream,
or written to the stream, one per line, to the end of the list.

@author GT Nunes
@version 1.3
@copyright (c) TheWiseCoder 2020-2021
@license BSD-3-Clause License
*/

%-------------------------------------------------------------------------------------

:- if(current_prolog_flag(dialect, sicstus)).

:- use_module('../sicstus/port_layer',
    [
        read_line_to_codes/2
    ]).

:- elif(current_prolog_flag(dialect, swi)).

:- use_module(library(readutil),
    [
        read_line_to_codes/2
    ]).

:- endif.

:- use_module(library(lists),
    [
        reverse/2
    ]).

:- use_module('stream_codes',
    [
        stream_codes/3
    ]).

%-------------------------------------------------------------------------------------

%! stream_ints(+Stream:ref, -Ints:list) is det.
%! stream_ints(+Stream:ref, +Ints:list) is det.
%
%  Read/write ints from/to stream.
%  If Ints is not grounded, read ints from Stream, up to the end of the stream.
%  Otherwise, write all ints in Ints to Stream.
%
%  @param Stream The input/output stream
%  @param Ints   List of ints read from, or to write to, the stream

stream_ints(Stream, Ints) :-

    (var(Ints) ->
        stream_read_1(Stream, [], Ints)
    ;
        stream_write(Stream, Ints)
    ).

%-------------------------------------------------------------------------------------

%! stream_read_1(+Stream:ref, -Ints:list) is det.
%
%  Read integers from Stream, one per line, to the end of the stream.
%
%  @param Stream       The input stream
%  @param IntsProgress Working list of ints read from the stream
%  @param IntsFinal    Final list ints read from the stream

% (iterate on file lines)
stream_read_1(Stream, IntsProgress, IntsFinal) :-

    read_line_to_codes(Stream, Line),
    stream_read_2(Stream, Line, IntsProgress, IntsFinal).

% (done)
stream_read_2(_Stream, end_of_file, IntsProgress, IntsFinal) :-
    reverse(IntsProgress, IntsFinal), !.

% (skip empty line)
stream_read_2(Stream, [], IntsProgress, IntsFinal) :-
    stream_read_1(Stream, IntsProgress, IntsFinal), !.

% (skip commented line - ascii value for '%' is 37)
stream_read_2(Stream, [37|_Line], IntsProgress, IntsFinal) :-
    stream_read_1(Stream, IntsProgress, IntsFinal), !.

% (keep the int)
stream_read_2(Stream, Line, IntsProgress, IntsFinal) :-

    number_codes(Int, Line),
    stream_read_1(Stream, [Int|IntsProgress], IntsFinal).

%-------------------------------------------------------------------------------------

%! stream_write(+Stream:ref, +Ints:list) is det.
%
%  Write all ints in Ints to Stream, one per line.
%
%  @param Stream The output stream
%  @param Ints   List of ints to write to the stream

% (done)
stream_write(_Stream, []) :- !.

% (start)
stream_write(Stream, [Int|Ints]) :-

    % write int to the stream
    number_codes(Int, Codes),
    stream_codes(Stream, -1, Codes),
    put_char(Stream, '\n'),

    % go for the next code
    stream_write(Stream, Ints).
