/*******************************************************************************
* FILENAME / MODULE : stream_bytes.pl / stream_bytes
*
* DESCRIPTION :
*       Read a list of bytes from a given stream. The invoker must
*       provide the number of bytes to read, or -1 to read up to the
*       end of the stream.
*
* PUBLIC PREDICATES :
*       stream_bytes(+Stream, +Count, -Bytes)
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

:- module(stream_bytes,
    [
        stream_bytes/3
    ]).

:- use_module(library(lists),
    [
        reverse/2
    ]).

%-------------------------------------------------------------------------------
% read up to Count bytes from Stream, or to the end of the stream
% for Count equals to -1

% stream_bytes(+Stream, +Count, -Bytes)
% Stream    the input stream
% Count     the number of bytes to read
% Bytes     the list of bytes read from the stream

% (done)
stream_bytes(_Stream, 0, Bytes) :-
    Bytes = [].

% (start)
stream_bytes(Stream, Count, Bytes) :-

    get_byte(Stream, Count, Byte, CountNew),
    stream_bytes_(Stream, CountNew, Byte, [], BytesFinal),
    reverse(BytesFinal, Bytes).

% stream_bytes_(+Stream, +BytesProgress, -BytesFinal)
% Stream        the input stream
% Count         the number of bytes to read
% BytesProgress working list of bytes read
% BytesFinal    final list of bytes read

% (done, number of bytes obtained)
stream_bytes_(_Stream, 0, Byte, BytesProgress, BytesFinal) :-
    BytesFinal = [Byte|BytesProgress].

% (done, end of stream reached)
stream_bytes_(_Stream, _Count, -1, BytesFinal, BytesFinal).

% (iterate)
stream_bytes_(Stream, Count, Byte, BytesProgress, BytesFinal) :-

    get_byte(Stream, Count, ByteNext, CountNext),
    stream_bytes_(Stream, CountNext, ByteNext,
                  [Byte|BytesProgress], BytesFinal).

%-------------------------------------------------------------------------------

% read the next byte from the stream
% get_byte(+Stream, +Count, -Byte, -CountNew)
% Stream        the input stream
% Count         the number of bytes to read
% Byte          the byte read from the stream
% CountNew      the remaining number of bytes to read
get_byte(Stream, Count, Byte, CountNew) :-

    get_byte(Stream, Byte),
    CountNew is Count - 1.
