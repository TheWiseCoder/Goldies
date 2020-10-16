/*******************************************************************************
* FILENAME / MODULE : stream_ints.pl / stream_ints
*
* DESCRIPTION :
*       Read integers from a given stream, one per line.
*
* PUBLIC PREDICATES :
*       stream_ints(+Stream, -Ints)
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
* 2020-03-20  GT Nunes          Module creation
* 2020-04-10  GT Nunes          Added this header
* 2020-08-07  GTN Nunes         Changed input from file to stream
*
*******************************************************************************/

:- module(stream_ints,
    [
        stream_ints/2
    ]).

:- use_module(library(lists),
    [
        reverse/2
    ]).

%-------------------------------------------------------------------------------
% read integers from a given stream, one per line, up to the end of the stream

% (start)
% stream_ints(+Stream, -Ints)
% Stream        input file path
% Ints          list of ints read from file
stream_ints(Stream, Ints) :-
    stream_ints_1(Stream, [], Ints).

% (iterate on file lines)
% stream_ints_1(+Stream, +IntsProgress, -IntsFinal)
% Stream        input file handle
% IntsProgress  working list of ints read
% IntsFinal     final list of ints read
stream_ints_1(Stream, IntsProgress, IntsFinal) :-

    read_line(Stream, Line),
    stream_ints_2(Stream, Line, IntsProgress, IntsFinal).

% (add int to ints list)
% stream_ints_2(+Stream, +Line, +IntsProgress, -IntsFinal)
% Stream        input file handle
% Line          current line read
% IntsProgress  working list of ints read
% IntsFinal     final list of ints read

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
