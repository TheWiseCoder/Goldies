These are a few utilities related to stream usage.  
  
**STREAM_BYTES**  
  
- `stream_bytes(+Stream, +Count, -Bytes)` - read up to *Count* bytes from *Stream*, or to the end of the stream for *Count* equals to -1  
  
**STREAM_CHARS**  
  
- `stream_chars(+Stream, +Count, -Bytes)` - read up to *Count* chars from *Stream*, or to the end of the stream for *Count* equals to -1  
  
**STREAM_CODES**  
  
- `stream_chars(+Stream, +Count, -Bytes)` - read up to *Count* codes from *Stream*, or to the end of the stream for *Count* equals to -1  
  
**STREAM_INTS**  
  
- `stream_ints(+Stream, -Ints)` - read integers from a *Stream*, one per line, up to the end of the stream
  
**STREAM_LINES**  
  
- `stream_lines(+Stream, +EOS, -Lines)` - read lines from *Stream* until *EOS* (a list of codes) is read, or, if it is empty, up to the end of the stream

