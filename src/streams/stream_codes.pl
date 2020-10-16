/*******************************************************************************
* FILENAME / MODULE : stream_codes.pl / stream_codes
*
* DESCRIPTION :
*       Read a list of codes from a given stream. The invoker must
*       provide the number of codes to read, or -1 to read up to the
*       end of the stream.
*
* PUBLIC PREDICATES :
*       stream_codes(+Stream, +Count, -Codes)
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

:- module(stream_codes,
    [
        stream_codes/3
    ]).

:- use_module(library(lists),
    [
        reverse/2
    ]).

%-------------------------------------------------------------------------------
% read up to Count codes from Stream, or to the end of the stream
% for Count equals to -1

% stream_codes(+Stream, +Count, -Codes)
% Stream        the input stream
% Count         the number of codes to read
% Codes        the codes read from the stream

% (done)
stream_codes(_Stream, 0, Codes) :-
    Codes = [].

% (start)
stream_codes(Stream, Count, Codes) :-

    get_code(Stream, Count, Code, CountNew),
    stream_codes_(Stream, CountNew, Code, [], CodesFinal),
    reverse(CodesFinal, Codes).

% stream_codes_(+Stream, +CodesProgress, -CodesFinal)
% Stream        the input stream
% Count         the number of codes to read
% CodesProgress working list of codes read
% CodesFinal    final list of codes read

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

%-------------------------------------------------------------------------------

% read the next code from the stream
% get_code(+Stream, +Count, -Code, -CountNew)
% Stream        the input stream
% Count         the number of codes to read
% Code          the code read from the stream
% CountNew      the remaining number of codes to read

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
