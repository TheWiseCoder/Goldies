:- module(stream_ints,
    [
        stream_ints/2
    ]).

/** <module> Read integers from a given stream

Read one integer per line, to the end of the stream.

@author GT Nunes
@version 1.0
@copyright (c) 2020 GT Nunes
@license BSD-3-Clause License
*/

%-------------------------------------------------------------------------------------

:- use_module(library(lists),
    [
        reverse/2
    ]).

%-------------------------------------------------------------------------------------

%! stream_ints(+Stream:ref, -Ints:list) is det.
%
%  Read integers from a Stream, one per line, to the end of the stream.
%
%  @param Stream Input stream
%  @param Ints   List of ints read from stream

stream_ints(Stream, Ints) :-
    stream_ints_1(Stream, [], Ints).

% (iterate on file lines)
stream_ints_1(Stream, IntsProgress, IntsFinal) :-

    read_line(Stream, Line),
    stream_ints_2(Stream, Line, IntsProgress, IntsFinal).

% (done)
stream_ints_2(_Stream, end_of_file, IntsProgress, IntsFinal) :-
    reverse(IntsProgress, IntsFinal).

% (skip empty line)
stream_ints_2(Stream, [], IntsProgress, IntsFinal) :-
    stream_ints_1(Stream, IntsProgress, IntsFinal).

% (skip commented line - ascii value for '%' is 37)
stream_ints_2(Stream, [37|_Line], IntsProgress, IntsFinal) :-
    stream_ints_1(Stream, IntsProgress, IntsFinal).

% (keep the int)
stream_ints_2(Stream, Line, IntsProgress, IntsFinal) :-

    number_codes(Int, Line),
    stream_ints_1(Stream, [Int|IntsProgress], IntsFinal).
