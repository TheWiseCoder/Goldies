:- module(stream_bytes,
    [
        stream_bytes/3
    ]).

/** <module> Read/write a list of bytes from/to a given stream
 
For reading from the stream, the invoker must provide the number of bytes to read,
or `-1` to read to the end of the stream.
For writing to the stream, the invoker must provide the number of bytes to write,
or `-1` to write all bytes in the list of bytes given.
In both situations, make sure to open the stream with 'type(binary)' in the
options list.

@author GT Nunes
@version 1.1
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
%! stream_bytes(+Stream:ref, +Count:int, +Bytes:list) is det.
%
%  If Bytes is grounded, write Count bytes in Bytes to Stream.
%  For Count = -1, write all bytes in Bytes.
%  Otherwise, Read up to Count bytes from Stream.
%  For Count = -1, read to the end of the stream.
%
%  @param Stream The input/output stream
%  @param Count  Number of bytes to read or write
%  @param Bytes  List of bytes read from, or write to, the stream

stream_bytes(Stream, Count, Bytes) :-

    (var(Bytes) ->
        stream_read(Stream, Count, Bytes)
    ;
        stream_write(Stream, Count, Bytes)
    ).

%-------------------------------------------------------------------------------------

%! stream_read(+Stream:ref, +Count:int, -Bytes:list) is det.
%
%  Read up to Count bytes from Stream.
%  For Count = -1, read to the end of the stream.
%
%  @param Stream The input stream
%  @param Count  Number of bytes to read
%  @param Bytes  List of bytes read from the stream

% (done)
stream_read(_Stream, 0, Bytes) :-
    Bytes = [].

% (start)
stream_read(Stream, Count, Bytes) :-

    get_byte(Stream, Count, Byte, CountNew),
    stream_read_(Stream, CountNew, Byte, [], BytesFinal),
    reverse(BytesFinal, Bytes).

% (done, number of bytes obtained)
stream_read_(_Stream, 0, Byte, BytesProgress, BytesFinal) :-
    BytesFinal = [Byte|BytesProgress].

% (done, end of stream reached)
stream_read_(_Stream, _Count, -1, BytesFinal, BytesFinal).

% (iterate)
stream_read_(Stream, Count, Byte, BytesProgress, BytesFinal) :-

    get_byte(Stream, Count, ByteNext, CountNext),
    stream_read_(Stream, CountNext, ByteNext,
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

%-------------------------------------------------------------------------------------

%! stream_write(+Stream:ref, +Count:int, +Bytes:list) is det.
%
%  Write up to Count bytes in Bytes to Stream.
%  For Count = -1, write all bytes in Bytes to Stream.
%
%  @param Stream The input stream
%  @param Count  Number of bytes to read
%  @param Bytes  List of bytes to write to the stream

% (done)
stream_write(_Stream, 0, _Bytes).

% (done)
stream_write(_Stream, _Count, []).

% (start)
stream_write(Stream, Count, [Byte|Bytes]) :-

    % write byte to the stream
    put_byte(Stream, Byte),

    % go for the next byte
    CountNext is Count - 1,
    stream_write(Stream, CountNext, Bytes).
