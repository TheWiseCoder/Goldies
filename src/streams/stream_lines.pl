:- module(stream_lines,
    [
        stream_lines/2,
        stream_lines/3
    ]).

/** <module> Read/write lines as lists of chars from/to a given stream

When reading from the stream, the invoker may provide a EOS (end-of-stream) marker,
or end_of_file to read to the end of the stream.
When writing to the stream, the invoker may provide a EOS, or  end_of_file to write
all lines in the list of lines given. 

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
/*
:- use_module('stream_chars',
    [
        stream_chars/3
    ]).
*/
%-------------------------------------------------------------------------------------

%! stream_lines(+Stream:ref, -Lines:list) is det.
%
%  @param Stream The input stream
%  @param Lines  List of lines read from stream

stream_lines(Stream, Lines) :-

    read_line_to_codes(Stream, LineCodes),
    stream_lines_(Stream, [], LineCodes, [], Lines).

%! stream_lines(+Stream:ref, +EOS:list, -Lines:list) is det.
%
%  @param Stream The input stream
%  @param EOS    List of codes signifying end of stream (empty, if n/a)
%  @param Lines  List of lines read from stream

stream_lines(Stream, EOS, Lines) :-

    read_line_to_codes(Stream, LineCodes),
    stream_lines_(Stream, EOS, LineCodes, [], Lines).

% (done)
stream_lines_(_Stream, _EOS, end_of_file, LinesProgress, LinesFinal) :-
    reverse(LinesProgress, LinesFinal).

% (iterate, add line to lines list)
stream_lines_(Stream, EOS, LineCodes, LinesProgress, LinesFinal) :-

    ( LineCodes = EOS ->
        stream_lines_(Stream, EOS, end_of_file,
                      LinesProgress, LinesFinal)
    ; (EOS \= [] , sublist(LineCodes, EOS, Pos)) ->
        (Pos = 0 ->
            stream_lines_(Stream, EOS, end_of_file,
                          LinesProgress, LinesFinal)
        ;
            sublist(LineCodes, LastCodes, 0, Pos),
            atoms_codes(LineChars, LastCodes),
            stream_lines_(Stream, EOS, end_of_file,
                          [LineChars|LinesProgress], LinesFinal)
        )
    ;
        atoms_codes(LineChars, LineCodes),
        read_line_to_codes(Stream, NewCodes),
        stream_lines_(Stream, EOS, NewCodes,
                      [LineChars|LinesProgress], LinesFinal)
    ).

/*
%-------------------------------------------------------------------------------------

%! stream_lines(+Stream:ref, -Lines:list) is det.
%! stream_lines(+Stream:ref, +Lines:list) is det.
%
%  If Lines is grounded, write all lines in Lines to Stream.
%  Otherwise, read from Stream up to the end of the stream.
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
%  If Lines is grounded, write lines to Stream until a line containing EOS is found.
%  For EOS = end_of_file, write all lines in Lines.
%  Otherwise, read from Stream until a line containing EOS is found.
%  For EOS = end_of_file, read to the end of the stream.
%
%  @param Stream The input/output stream
%  @param EOS    List of codes signifying end of stream, if applicable
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
%  @param EOS           List of codes signifying end of stream, if applicable
%  @param LineCodes     Line currently read from the stream
%  @param LinesProgress Working list of lines read from the stream
%  @param LinesFinal    Final list of lines read from the stream

% (done)
stream_read(_Stream, _EOS, end_of_file, LinesProgress, LinesFinal) :-
    reverse(LinesProgress, LinesFinal).

% (iterate, adding line to lines list)
stream_read(Stream, EOS, LineCodes, LinesProgress, LinesFinal) :-

    (LineCodes = EOS ->
        NewCodes = end_of_file,
        LinesRevised = LinesProgress
    ;
        atoms_codes(LineChars, LineCodes),
        LinesRevised = [LineChars|LinesProgress],
        read_line_to_codes(Stream, NewCodes)
    ),

    % go for the next line
    stream_read(Stream, EOS, NewCodes, LinesRevised, LinesFinal).

%-------------------------------------------------------------------------------------

%! stream_write(+Stream:ref, +EOS:list, +Lines:list) is det.
%
%  Write Lines to Stream until a line containing EOS is found.
%  For EOS = end_of_file, write all lines in Lines to the stream.
%
%  @param Stream The output stream
%  @param EOS    List of codes signifying end of stream, if applicable
%  @param Lines  List of lines to write to the stream

% (done)
stream_write(_Stream, _EOS, end_of_file).

% (start)
stream_write(Stream, EOS, [Line|Lines]) :-

    % has EOS been found ?
    (Line = EOS ->
        % yes, so signal end-of-stream
        LinesAdjusted = end_of_file
    ;
        % no, so write current line, followed by a new line
        stream_chars(Stream, -1, Line),
        put_char(Stream, '\n'),
        LinesAdjusted = Lines
    ),

    % go for the next line
    stream_write(Stream, EOS, LinesAdjusted).
*/
