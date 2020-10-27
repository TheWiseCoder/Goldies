:- module(stream_chars,
    [
        stream_chars/3      % stream_chars(+Stream, +Count, -Chars)
    ]).

/** <module> Read a list of chars from a given stream

The invoker must provide the number of chars to read,
or `-1` to read to the end of the stream.

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

%! stream_chars(+Stream:ref, +Count:int, -Chars:list) is det.
%
%  Read up to Count chars from Stream. For Count = -1, read to the end of the stream.
%
%  @param Stream The input stream
%  @param Count  Number of chars to read
%  @param Chars  List of chars read from the stream

% (done)
stream_chars(_Stream, 0, Chars) :-
    Chars = [].

% (start)
stream_chars(Stream, Count, Chars) :-

    get_char(Stream, Count, Char, CountNext),
    stream_chars_(Stream, CountNext, Char, [], CharsFinal),
    reverse(CharsFinal, Chars).

% (done, number of chars obtained)
stream_chars_(_Stream, 0, Char, CharsProgress, CharsFinal) :-
    CharsFinal = [Char|CharsProgress].

% (done, end of stream reached)
stream_chars_(_Stream, _Count, end_of_file, CharsFinal, CharsFinal).

% (iterate)
stream_chars_(Stream, Count, Char, CharsProgress, CharsFinal) :-

    get_char(Stream, Count, CharNext, CountNext),
    stream_chars_(Stream, CountNext, CharNext,
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
