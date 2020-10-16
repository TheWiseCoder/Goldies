/*******************************************************************************
* FILENAME / MODULE : stream_chars.pl / stream_chars
*
* DESCRIPTION :
*       Read a list of chars from a given stream. The invoker must
*       provide the number of chars to read, or -1 to read up to the
*       end of the stream.
*
* PUBLIC PREDICATES :
*       stream_chars(+Stream, +Count, -Chars)
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
* 2020-07-12  GT Nunes          Module creation
*
*******************************************************************************/

:- module(stream_chars,
    [
        stream_chars/3
    ]).

:- use_module(library(lists),
    [
        reverse/2
    ]).

%-------------------------------------------------------------------------------
% read up to Count chars from Stream, or to the end of the stream
% for Count equals to -1

% stream_chars(+Stream, +Count, -Chars)
% Stream        the input stream
% Count         the number of chars to read
% Chars         the list of chars read from the stream

% (done)
stream_chars(_Stream, 0, Chars) :-
    Chars = [].

% (start)
stream_chars(Stream, Count, Chars) :-

    get_char(Stream, Count, Char, CountNext),
    stream_chars_(Stream, CountNext, Char, [], CharsFinal),
    reverse(CharsFinal, Chars).

% stream_chars_(+Stream, +CharsProgress, -CharsFinal)
% Stream        the input stream
% Count         the number of chars to read
% CharsProgress working list of chars read
% CharsFinal    final list of chars read

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

%-------------------------------------------------------------------------------

% read the next char from the stream
% get_char(+Stream, +Count, -Char, -CountNew)
% Stream        the input stream
% Count         the number of chars to read
% Char          the char read from the stream
% CountNew      the remaining number of chars to read

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
