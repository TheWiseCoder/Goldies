:- module(stream_bytes,
    [
        stream_bytes/3
    ]).

/** <module> Read a list of bytes from a given stream
 
The invoker must provide the number of bytes to read,
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

%! stream_bytes(+Stream:ref, +Count:int, -Bytes:list) is det.
%
%  Read up to Count bytes from Stream. For Count = -1, read to the end of the stream.
%
%  @param Stream The input stream
%  @param Count  Number of bytes to read
%  @param Bytes  List of bytes read from the stream

% (done)
stream_bytes(_Stream, 0, Bytes) :-
    Bytes = [].

% (start)
stream_bytes(Stream, Count, Bytes) :-

    get_byte(Stream, Count, Byte, CountNew),
    stream_bytes_(Stream, CountNew, Byte, [], BytesFinal),
    reverse(BytesFinal, Bytes).

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

%-------------------------------------------------------------------------------------

%! get_byte(+Stream:ref, +Count:int, -Byte:int, -CountNew:int) is det.
%
%  Read the next byte from Stream.
%
%  @param Stream   The input stream
%  @param Count    Number of bytes to read
%  @param Byte     Byte read from the stream
%  @param CountNew The remaining number of bytes to read

get_byte(Stream, Count, Byte, CountNew) :-

    get_byte(Stream, Byte),
    CountNew is Count - 1.
