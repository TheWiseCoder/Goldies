:- module(stream_chars,
    [
        stream_chars/2,
        stream_chars/3
    ]).

/** <module> Read/write a list of chars from/to a given stream
 
When reading from the stream, the invoker may provide the number of chars to read,
or `-1` to read to the end of the stream.
When writing to the stream, the invoker may provide the number of chars to write,
or `-1` to write all chars in the list of chars given.

@author GT Nunes
@version 1.3
@copyright (c) TheWiseCoder 2020-2021
@license BSD-3-Clause License
*/

%-------------------------------------------------------------------------------------

:- use_module(library(lists),
    [
        reverse/2
    ]).

%-------------------------------------------------------------------------------------

%! stream_chars(+Stream:ref, -Chars:list) is det.
%! stream_chars(+Stream:ref, +Chars:list) is det.
%
%  Read/write chars from/to stream.
%  If Chars is not grounded, read from Stream up to the end of the stream.
%  Otherwise, write all chars in Chars to Stream.
%
%  @param Stream The input/output stream
%  @param Chars  List of chars read from, or to write to, the stream

stream_chars(Stream, Chars) :-

    (var(Chars) ->
        stream_read(Stream, -1, Chars)
    ;
        stream_write(Stream, -1, Chars)
    ).

%-------------------------------------------------------------------------------------

%! stream_chars(+Stream:ref, +Count:int, -Chars:list) is det.
%! stream_chars(+Stream:ref, +Count:int, +Chars:list) is det.
%
%  Read/write chars from/to stream.
%  If Chars is not grounded, read up to Count chars from Stream.
%  For Count = -1, read to the end of the stream.
%  If Chars is grounded, write Count chars in Chars to Stream.
%  For Count = -1, write all chars in Chars.
%
%  @param Stream The input/output stream
%  @param Count  Number of bytes to read or write
%  @param Chars  List of chars read from, or to write to, the stream

stream_chars(Stream, Count, Chars) :-

    (var(Chars) ->
        stream_read(Stream, Count, Chars)
    ;
        stream_write(Stream, Count, Chars)
    ).

%-------------------------------------------------------------------------------------

%! stream_read(+Stream:ref, +Count:int, -Chars:list) is det.
%
%  Read up to Count chars from Stream.
%  For Count = -1, read to the end of the stream.
%
%  @param Stream The input stream
%  @param Count  Number of chars to read
%  @param Chars  List of chars read from the stream

% (done)
stream_read(_Stream, 0, Chars) :-
    Chars = [], !.

% (start)
stream_read(Stream, Count, Chars) :-

    get_char(Stream, Count, Char, CountNext),
    stream_read_(Stream, CountNext, Char, [], CharsFinal),
    reverse(CharsFinal, Chars).

% (done, number of chars obtained)
stream_read_(_Stream, 0, Char, CharsProgress, CharsFinal) :-
    CharsFinal = [Char|CharsProgress], !.

% (done, end of stream reached)
stream_read_(_Stream, _Count, end_of_file, CharsFinal, CharsFinal) :- !.

% (iterate)
stream_read_(Stream, Count, Char, CharsProgress, CharsFinal) :-

    get_char(Stream, Count, CharNext, CountNext),
    stream_read_(Stream, CountNext, CharNext,
                 [Char|CharsProgress], CharsFinal).

%-------------------------------------------------------------------------------------

%! get_char(+Stream:ref, +Count:int, -Char:atom, -CountNew:int) is det.
%
%  Read the next char from Stream.
%
%  @param Stream   The input stream
%  @param Count    Number of chars to read
%  @param Char     Char read from the stream
%  @param CountNew The remaining number of chars to read

:- if(current_prolog_flag(dialect, sicstus)).

get_char(Stream, Count, Char, CountNew) :-

    get_char(Stream, Char),
    (Char = '\n' ->
        % '\n' counts as 2 chars
        CountNew is Count - 2
    ;
        CountNew is Count - 1
    ).

:- elif(current_prolog_flag(dialect, swi)).

get_char(Stream, Count, Char, CountNew) :-

    get_char(Stream, Char),
    CountNew is Count - 1.

:- endif.

%-------------------------------------------------------------------------------------

%! stream_write(+Stream:ref, +Count:int, +Chars:list) is det.
%
%  Write up to Count bytes in Chars to Stream.
%  For Count = -1, write all chars in Chars to Stream.
%
%  @param Stream The output stream
%  @param Count  Number of chars to write
%  @param Chars  List of chars to write to the stream

% (done)
stream_write(_Stream, 0, _Chars) :- !.

% (done)
stream_write(_Stream, _Count, []) :- !.

% (start)
stream_write(Stream, Count, [Char|Chars]) :-

    % write char to the stream
    put_char(Stream, Char),

    % go for the next char
    CountNext is Count - 1,
    stream_write(Stream, CountNext, Chars).
