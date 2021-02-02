:- module(stream_lines,
    [
        stream_lines/2,
        stream_lines/3
    ]).

/** <module> Read/write lines as lists of chars from/to a given stream

When reading from the stream, the invoker may provide a EOS (end-of-stream) marker
to limit the reading, or the EOF (end_of_file) atom to read to the end of the stream.
When writing to the stream, the invoker may provide a EOS to limit the writing,
or EOF to write all lines in the list of lines provided. A EOS marker may be any
atom suitable as an indication of the end of the stream operation, presented as a
list of chars.

@author GT Nunes
@version 1.1
@copyright (c) 2020 GT Nunes
@license BSD-3-Clause License
*/

%-------------------------------------------------------------------------------------

:- if(current_prolog_flag(dialect, sicstus)).

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

:- elif(current_prolog_flag(dialect, swi)).

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
    
:- endif.

:- use_module('../common/atom_marks',
    [
        atoms_codes/2
    ]).

:- use_module('stream_chars',
    [
        stream_chars/3
    ]).

%-------------------------------------------------------------------------------------

%! stream_lines(+Stream:ref, -Lines:list) is det.
%! stream_lines(+Stream:ref, +Lines:list) is det.
%
%  If Lines is not grounded, read from Stream up to the end of the stream.
%  If Lines is grounded, write all lines in Lines to Stream.
%
%  @param Stream The input/output stream
%  @param Lines  List of lines read from, or to write to, the stream

stream_lines(Stream, Lines) :-

    (var(Lines) ->
        read_line_to_codes(Stream, LineCodes),
        stream_read(Stream, end_of_file, LineCodes, [], Lines)
    ;
        stream_write(Stream, end_of_file, Lines)
    ).

%-------------------------------------------------------------------------------------

%! stream_lines(+Stream:ref, +EOS:list, -Lines:list) is det.
%! stream_lines(+Stream:ref, +EOS:list, +Lines:list) is det.
%
%  If Lines is not grounded, read from Stream until a line containing EOS is found.
%  For EOS = end_of_file, read to the end of the stream.
%  If Lines is grounded, write lines to Stream until a line containing EOS is found.
%  For EOS = end_of_file, write all lines in Lines.
%
%  @param Stream The input/output stream
%  @param EOS    EOF or list of chars signifying end of stream
%  @param Lines  List of lines read from, or to write to, the stream

stream_lines(Stream, EOS, Lines) :-

    (var(Lines) ->
        read_line_to_codes(Stream, LineCodes),
        stream_read(Stream, EOS, LineCodes, [], Lines)
    ;
        stream_write(Stream, EOS, Lines)
    ).

%-------------------------------------------------------------------------------------

%! stream_read(+Stream:ref, +EOS:list, +LineCodes:list, +LinesProgress:list, -LinesFinal:list) is det.
%
%  Read from Stream until a line containing EOS is found.
%  For EOS = end_of_file, read to the end of the stream.
%
%  @param Stream        The input stream
%  @param EOS           EOF or List of chars signifying end of stream
%  @param LineCodes     Line newly read from the stream, as a list of codes
%  @param LinesProgress Working list of lines read from the stream
%  @param LinesFinal    Final list of lines read from the stream

% (done)
stream_read(_Stream, _EOS, end_of_file, LinesProgress, LinesFinal) :-
    reverse(LinesProgress, LinesFinal).

% (iterate, adding a new line to lines list)
stream_read(Stream, EOS, LineCodes, LinesProgress, LinesFinal) :-

    % convert codes to chars in current line
    atoms_codes(LineChars, LineCodes),

    % is the new line equal to EOS ?
    ( LineChars = EOS ->

        % yes, so conclude the operation
        NewCodes = end_of_file,
        LinesAdjusted = LinesProgress

    % has EOS been found within the new line ?
    ; (EOS \= [] , sublist(LineChars, EOS, Pos)) ->

        % yes, so process the line and conclude the operation
        NewCodes = end_of_file,
        (Pos = 0 ->
            LinesAdjusted = LinesProgress
        ;
            sublist(LineChars, LastChars, 0, Pos),
            LinesAdjusted = [LastChars|LinesProgress]
        )

    % process the new line
    ; otherwise ->

        LinesAdjusted = [LineChars|LinesProgress],
        read_line_to_codes(Stream, NewCodes)
    ),

    % go for the next line
    stream_read(Stream, EOS, NewCodes, LinesAdjusted, LinesFinal).

%-------------------------------------------------------------------------------------

%! stream_write(+Stream:ref, +EOS:list, +Lines:list) is det.
%
%  Write Lines to Stream until a line containing EOS is found.
%  For EOS = end_of_file, write all lines in Lines to the stream.
%
%  @param Stream The output stream
%  @param EOS    EOF or List of chars signifying end of stream
%  @param Lines  List of lines to write to the stream

% (done)
stream_write(_Stream, _EOS, []).

% (iterate)
stream_write(Stream, EOS, [Line|Lines]) :-

    % is the new line equal to EOS ?
    ( Line = EOS ->
        % yes, so signal end of the stream
        LinesAdjusted = []

    % has EOS been found within the new line ?
    ; (EOS \= [] , sublist(Line, EOS, Pos)) ->

        % yes, so process the line and conclude the operation
        LinesAdjusted = [],
        ( Pos = 0
        ;
          ( sublist(Line, LastLine, 0, Pos)
          ,  stream_chars(Stream, -1, LastLine)
          ,  put_char(Stream, '\n') )
        )

    % write current line, followed by a new line, and proceed
    ; otherwise ->
        stream_chars(Stream, -1, Line),
        put_char(Stream, '\n'),
        LinesAdjusted = Lines
    ),

    % go for the next line
    stream_write(Stream, EOS, LinesAdjusted).
