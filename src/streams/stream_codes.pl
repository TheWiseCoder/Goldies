:- module(stream_codes,
    [
        stream_codes/2,
        stream_codes/3
    ]).

/** <module> Read/write a list of codes from/to a given stream
 
When reading from the stream, the invoker may provide the number of codes to read,
or `-1` to read to the end of the stream.
When writing to the stream, the invoker may provide the number of codes to write,
or `-1` to write all codes in the list of codes given.

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

%! stream_codes(+Stream:ref, -Codes:list) is det.
%! stream_codes(+Stream:ref, +Codes:list) is det.
%
%  Read/write codes from/to stream.
%  If Codes is not grounded, read from Stream up to the end of the stream.
%  Otherwise, write all codes in Codes to Stream.
%
%  @param Stream The input/output stream
%  @param Codes  List of codes read from, or to write to, the stream

stream_codes(Stream, Codes) :-

    % fail point
    var(Codes),

    stream_read(Stream, -1, Codes),
    !.

stream_codes(Stream, Codes) :-

    % fail point
    nonvar(Codes),

    stream_write(Stream, -1, Codes),
    !.

%-------------------------------------------------------------------------------------

%! stream_codes(+Stream:ref, +Count:int, -Codes:list) is det.
%! stream_codes(+Stream:ref, +Count:int, +Codes:list) is det.
%
%  Read/write codes from/to stream.
%  If Codes is not grounded, read up to Count codes from Stream.
%  For Count = -1, read to the end of the stream.
%  If Codes is not grounded, write Count codes in Codes to Stream.
%  For Count = -1, write all codes in Codes to Stream.
%
%  @param Stream The input/output stream
%  @param Count  Number of bytes to read or write
%  @param Codes  List of codes read from, or to write to, the stream

stream_codes(Stream, Count, Codes) :-

    % fail point
    var(Codes),

    stream_read(Stream, Count, Codes),
    !.

stream_codes(Stream, Count, Codes) :-

    % fail point
    nonvar(Codes),

    stream_write(Stream, Count, Codes),
    !.

%-------------------------------------------------------------------------------------

%! stream_read(+Stream:ref, +Count:int, -Codes:list) is det.
%
%  Read up to Count codes from Stream.
%  For Count = -1, read to the end of the stream.
%
%  @param Stream The input stream
%  @param Count  Number of codes to read
%  @param Codes  List of codes read from the stream

% (done)
stream_read(_Stream, 0, []) :- !.

% (start)
stream_read(Stream, Count, Codes) :-

    get_code(Stream, Count, Code, CountNew),
    stream_read_(Stream, CountNew, Code, [], CodesFinal),
    reverse(CodesFinal, Codes).

% (done, number of codes obtained)
stream_read_(_Stream, 0, Code, CodesProgress, CodesFinal) :-
    CodesFinal = [Code|CodesProgress], !.

% (done, end of stream reached)
stream_read_(_Stream, _Count, -1, CodesFinal, CodesFinal) :- !.

% (iterate)
stream_read_(Stream, Count, Code, CodesProgress, CodesFinal) :-

    get_code(Stream, Count, CodeNext, CountNext),
    stream_read_(Stream, CountNext, CodeNext,
                 [Code|CodesProgress], CodesFinal).

%-------------------------------------------------------------------------------------

%! get_code(+Stream:ref, +Count:int, -Code:int, -CountNew:int) is det.
%
%  Read the next code from Stream.
%
%  @param Stream   The input stream
%  @param Count    Number of codes to read
%  @param Code     Code read from the stream
%  @param CountNew The remaining number of codes to read

:- if(current_prolog_flag(dialect, sicstus)).

get_code(Stream, Count, Code, CountNew) :-

    get_code(Stream, Code),
    (Code = 10 ->
        % '\n' counts as 2 codes
        CountNew is Count - 2
    ;
        CountNew is Count - 1
    ).

:- elif(current_prolog_flag(dialect, swi)).

get_code(Stream, Count, Code, CountNew) :-

    get_code(Stream, Code),
    CountNew is Count - 1.

:- endif.

%-------------------------------------------------------------------------------------

%! stream_write(+Stream:ref, +Count:int, +Codes:list) is det.
%
%  Write up to Count bytes in Codes to Stream.
%  For Count = -1, write all codes in Codes to Stream.
%
%  @param Stream The output stream
%  @param Count  Number of bytes to write
%  @param Codes  List of codes to write to the stream

% (done)
stream_write(_Stream, 0, _Codes) :- !.

% (done)
stream_write(_Stream, _Count, []) :- !.

% (start)
stream_write(Stream, Count, [Code|Codes]) :-

    % write code to the stream
    put_code(Stream, Code),

    % go for the next code
    CountNext is Count - 1,
    stream_write(Stream, CountNext, Codes).
