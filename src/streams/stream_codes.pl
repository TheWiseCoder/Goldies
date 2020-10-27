:- module(stream_codes,
    [
        stream_codes/3      % stream_codes(+Stream, +Count, -Codes)
    ]).

/** <module> Read a list of codes from a given stream

The invoker must provide the number of codes to read,
or -1 to read to the end of the stream.

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

%! stream_codes(+Stream:ref, +Count:int, -Codes:list) is det.
%
%  Read up to Count codes from Stream. For Count = -1, read to the end of the stream.
%
%  @param Stream The input stream
%  @param Count  Number of chars to read
%  @param Codes  List of codes read from the stream

% (done)
stream_codes(_Stream, 0, Codes) :-
    Codes = [].

% (start)
stream_codes(Stream, Count, Codes) :-

    get_code(Stream, Count, Code, CountNew),
    stream_codes_(Stream, CountNew, Code, [], CodesFinal),
    reverse(CodesFinal, Codes).

% (done, number of codes obtained)
stream_codes_(_Stream, 0, Code, CodesProgress, CodesFinal) :-
    CodesFinal = [Code|CodesProgress].

% (done, end of stream reached)
stream_codes_(_Stream, _Count, -1, CodesFinal, CodesFinal).

% (iterate)
stream_codes_(Stream, Count, Code, CodesProgress, CodesFinal) :-

    get_code(Stream, Count, CodeNext, CountNext),
    stream_codes_(Stream, CountNext, CodeNext,
                  [Code|CodesProgress], CodesFinal).

%-------------------------------------------------------------------------------------

%! get_code(+Stream:ref, +Count:int, -Code:int, -CountNew:int) is det.
%
%  Read the next code from Stream.
%
%  @param Stream   The input stream
%  @param Count    Number of chars to read
%  @param Code     Code read from the stream
%  @param CountNew The remaining number of chars to read

:- if(current_prolog_flag(dialect, sicstus)).

get_code(Stream, Count, Code, CountNew) :-

    get_code(Stream, Code),
    (Code = 10 ->
        % '\n' counts as 2 chars
        CountNew is Count - 2
    ;
        CountNew is Count - 1
    ).

:- elif(current_prolog_flag(dialect, swi)).

get_code(Stream, Count, Code, CountNew) :-

    get_code(Stream, Code),
    CountNew is Count - 1.

:- endif.
