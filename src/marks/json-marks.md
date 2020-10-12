Small utilities for handling JSON-encoded data. The representation of JSON values in the SICStus and SWI-Prolog platforms is as follows:

**Number**  
a JSON number is represented as the corresponding Prolog number, or as a floating point number when the JSON number has an exponent or a fractional part, otherwise as an integer.  
**String**  
a JSON string is represented as the corresponding Prolog atom (escaped surrogate pairs are combined into the corresponding Unicode code point).  
**Array**  
a JSON array is represented as a list of the corresponding Prolog terms.  
**Object**  
a JSON object is a sequence of name:value pairs, where each nsme is a JSON string and each value is an arbitrary JSON value. It is represented as a term json(Members) with Members being a list of Name=Value pairs, where Name is a representation of the JSON string name and Value is a representaton of the JSON value.  
**null**, **true**, **false**  
these special JSON literals are, by default, translated to the Prolog terms @(null), @(true), and @(false), respectively.  

The available predicates are:

- `json_add(?JsonTerm, +Item(s), -JsonResult)` - add item(s) to a JSON term
- `json_atom(?JsonTerm, ?JsonAtom)` - unify a JSON term with an atom standing for the corresponding JSON string representation
- `json_chars(?JsonTerm, ?JsonChars)` - unify a JSON term with a list of chars standing for the corresponding JSON string representation
- `json_codes(?JsonTerm, ?JsonCodes)` - unify a JSON term with a list of char codes standing for the corresponding JSON string representation
- `json_member(+JsonTerm, +Name, -JsonValue)` - unify *JsonValue* with the corresponding value for *Name* in *JsonTerm*
- `json_members(+JsonTerm, +Names, -JsonValues)` - unify *JsonValues* with the corresponding values for *Names* in *JsonTerm*
- `json_merge(+JsonTerms, -JsonResult)` -  merge a list of JSON terms into a single JSON term
- `json_input(+Stream, -Term)` - read the next value from *Stream* into a JSON term
- `json_output(+Stream, +Term)` - write a JSON term to Stream

The following predicates replicate the features lists, with *Options* related to read/write operations:
- `json_atom(?JsonTerm, ?JsonAtom, +Options)`
- `json_chars(?JsonTerm, ?JsonChars, +Options)`
- `json_codes(?JsonTerm, ?JsonCodes, +Options)`
- `json_input(+Stream, -Term, +Options)`
- `json_output(+Stream, +Term, +Options)`

Additionally, you may peruse the documentation and code on the source code file.