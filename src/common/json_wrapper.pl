:- module(json_wrapper,
    [
        json_add/3,
        json_atom/2,
        json_atom/3,
        json_chars/2,
        json_chars/3,
        json_codes/2,
        json_codes/3,
        json_input/2,
        json_input/3,
        json_member/3,
        json_members/3,
        json_merge/2,
        json_merge/3,
        json_output/2,
        json_output/3
    ]).

/** <module> Utilities for handling JSON-encoded data

The representations of JSON values in the SICStus and SWI-Prolog platforms
are as follows:
~~~
*|Number|* - A JSON number is represented as the corresponding Prolog number;
             as a floating point number when the JSON number has an exponent
             or a fractional part, otherwise as an integer.

*|String|* - A JSON string is represented as the corresponding Prolog atom
             (escaped surrogate pairs are combined into the corresponding
              Unicode code point).

*|Array|*  - A JSON array is represented as a list of the corresponding Prolog terms.

*|Object|* - A JSON object is a sequence of name:value pairs, where each name
             is a JSON string and each value is an arbitrary JSON value. It is
             represented as a term `json(Members)` with `Members` being a list of
             `Name=Value` pairs, where `Name` is a representation of the JSON
             string name and `Value` is a representation of the JSON value.

*|null|*   - Translated to the Prolog term `@(null)`.

*|true|*   - Translated to the Prolog term `@(true)`.

*|false|*  - Translated to the Prolog term `@(false)`.
~~~

@author GT Nunes
@version 1.2
@copyright (c) TheWiseCoder 2020-2021
@license BSD-3-Clause License
*/

%-------------------------------------------------------------------------------------

:- if(current_prolog_flag(dialect, sicstus)).

:- use_module(library(json),
    [
        json_read/2,
        json_read/3,
        json_write/2,
        json_write/3
    ]).

:- use_module(library(lists),
    [
        is_list/1,
        reverse/2
    ]).

:- elif(current_prolog_flag(dialect, swi)).

:- use_module(library(lists),
    [
        reverse/2
    ]).

:- use_module(library(http/json),
    [
        json_read/2,
        json_read/3,
        json_write/2,
        json_write/3
    ]).

:- endif.

:- use_module(library(codesio),
    [
        open_codes_stream/2,
        with_output_to_codes/4
    ]).

:- use_module('atom_marks',
    [
        atoms_codes/2
    ]).

%-------------------------------------------------------------------------------------

%! json_input(+Stream:ref, -Term:json) is det.
%
%  Read next value from Stream into Term.
%
%  @param Stream  The stream to read from
%  @param Term    The JSON term to unify with

json_input(Stream, Term) :-
    json_read(Stream, Term).

%! json_input(+Stream:ref, -Term:json, +Options:list) is det.
%
%  Read next value from Stream into Term, according to Options.
%
%  @param Stream  The stream to read from
%  @param Term    The JSON term to unify with
%  @param Options List of options guiding the input process

json_input(Stream, Term, Options) :-
    json_read(Stream, Term, Options).

%-------------------------------------------------------------------------------------

%! json_output(+Stream:ref, +Term:json) is det.
%
%  Write Term to Stream.
%
%  @param Stream The stream to write to
%  @param Term   The JSON term to write to Stream

json_output(Stream, Term) :-
    json_write(Stream, Term).

%! json_output(+Stream:ref, +Term:json, +Options:list) is det.
%
%  Write Term to Stream, according to Options.
%
%  @param Stream  The stream to write to
%  @param Term    The JSON term to write to Stream
%  @param Options List of options guiding the output process

json_output(Stream, Term, Options) :-
    json_write(Stream, Term, Options).

%-------------------------------------------------------------------------------------

%! json_add(+JsonTerm:term, +Items:list, -JsonResult:json) is det.
%
%  Add Items to JsonTerm, and unify JsonResult with the result.
%  Items can be a scalar value.
%
%  @param JSonTerm   The JSON term
%  @param Items      The item or list of items to add to
%  @param JsonResult The resulting JSON term

json_add(JsonTerm, Item, JsonResult) :-

    (is_list(Item) ->
        json_add_(Item, JsonTerm, JsonResult)
    ;
        json(Items) = JsonTerm,
        JsonResult = json([Item|Items])
    ).

% (done)
json_add_([], JsonFinal, JsonFinal).

% (iterate)
json_add_([Item|Items], JsonTerm, JsonFinal) :-

    json(CurrItems) = JsonTerm,
    JsonRevised = json([Item|CurrItems]),
    json_add_(Items, JsonRevised, JsonFinal).

%-------------------------------------------------------------------------------------

%! json_merge(+JsonTerm1:json, +JsonTerm2:json, -JsonResult:json) is det.
%
%  Merge two JSON terms into a single JSON term.
%
%  @param JsonTerm1  The 1st JSON term to merge
%  @param JsonTerm2  The 2nd JSON term to merge
%  @param JsonResult The resulting JSON term

json_merge(JsonTerm1, JsonTerm2, JsonResult) :-
    json_merge_([JsonTerm1,JsonTerm2], json([]), JsonResult).

%! json_merge(+JsonTerms:list, -JsonResult:json) is det.
%
%  Merge a list of JSON terms into a single JSON term.
%
%  @param JsonTerms  The JSON terme to merge
%  @param JsonResult The resulting JSON term

json_merge(JsonTerms, JsonResult) :-
    json_merge_(JsonTerms, json([]), JsonResult).

% (done)
json_merge_([], JsonFinal, JsonFinal).

% (iterate)
json_merge_([JsonTerm|JsonTerms], JsonProgress, JsonFinal) :-

    json(ItemsNew) = JsonTerm,
    json(ItemsCurr) = JsonProgress,
    append(ItemsCurr, ItemsNew, ItemsRevised),
    JsonRevised = json(ItemsRevised),
    json_merge_(JsonTerms, JsonRevised, JsonFinal).  

%-------------------------------------------------------------------------------------

%! json_member(+JsonTerm:json, +Name:atom, -Value:data) is det.
%
%  Unify Value with the corresponding value for Name in JsonTerm.
%
%  @param JsonTerm The JSON term
%  @param Name     The name of the target member
%  @param Value    The value associated with the targer member

json_member(JsonTerm, Name, JsonValue) :-

    json(JsonMembers) = JsonTerm,
    !,
    % fail point
    memberchk(Name=Value, JsonMembers),
    JsonValue = Value.

%-------------------------------------------------------------------------------------

%! json_members(+JsonTerm:json, +Names:list, -Values:list) is det.
%
%  Unify Values with the corresponding values for Names in JsonTerm.
%
%  @param JsonTerm The JSON term
%  @param Names    The names of the target members
%  @param Values   The values associated with the targer members

json_members(JsonTerm, Names, JsonValues) :-
    % fail point
    json_members_(JsonTerm, Names, [], JsonValues).

% (done)
json_members_(_JsonObj, [], JsonProgress, JsonValues) :-
    reverse(JsonProgress, JsonValues).

% (iterate)
json_members_(JsonTerm, [Name|Names], JsonProgress, JsonValues) :-

    % fail points
    json_member(JsonTerm, Name, Value),
    !,
    json_members_(JsonTerm, Names, [Value|JsonProgress], JsonValues).

%-------------------------------------------------------------------------------------

%! json_atom(+JsonTerm:json, -JsonAtom:atom) is det.
%! json_atom(-JsonTerm:json, +JsonAtom:atom) is det.
%
%  Unify a JSON term with an atom standing for the corresponding JSON
%  string representation. See json_codes/2 and json_codes/3 below.
%
%  @param JsonTerm  The JSON term
%  @param JsonAtom  The atom holding the JSON string representation

json_atom(JsonTerm, JsonAtom) :-

    (var(JsonAtom) ->
        json_codes(JsonTerm, JsonCodes),
        atom_codes(JsonAtom, JsonCodes)
    ;
        atom_codes(JsonAtom, JsonCodes),
        json_codes(JsonTerm, JsonCodes)
    ).

%! json_atom(+JsonTerm:json, -JsonAtom:atom, +Options:list) is det.
%! json_atom(-JsonTerm:json, +JsonAtom:atom, +Options:list) is det.
%
%  Unify a JSON term with an atom standing for the corresponding JSON
%  string representation, according to Options.
%  See json_codes/2 and json_codes/3 below.
%
%  @param JsonTerm  The JSON term
%  @param JsonAtom  The atom holding the JSON string representation
%  @param Options   List of options for json_read/json_write

json_atom(JsonTerm, JsonAtom, Options) :-

    (var(JsonAtom) ->
        json_codes(JsonTerm, JsonCodes, Options),
        atom_codes(JsonAtom, JsonCodes)
    ;
        atom_codes(JsonAtom, JsonCodes),
        json_codes(JsonTerm, JsonCodes, Options)
    ).

%-------------------------------------------------------------------------------------

%! json_chars(+JsonTerm:json, -JsonChars:list) is det.
%! json_chars(-JsonTerm:json, +JsonChars:list) is det.
%
%  Unify JsonTerm with a list of chars standing for the corresponding
%  JSON string representation. See json_codes/2 and json_codes/3 below.
%
%  @param JsonTerm  The JSON term
%  @param JsonChars List of chars holding the JSON string representation

json_chars(JsonTerm, JsonChars) :-

    (var(JsonChars) ->
        json_codes(JsonTerm, JsonCodes),
        atoms_codes(JsonChars, JsonCodes)
    ;
        atoms_codes(JsonChars, JsonCodes),
        json_codes(JsonTerm, JsonCodes)
    ).

%! json_chars(+JsonTerm:json, -JsonChars:list, +Options:list) is det.
%! json_chars(-JsonTerm:json, +JsonChars:list, +Options:list) is det.
%
%  Unify JsonTerm with a list of chars standing for the corresponding
%  JSON string representation, according to Options.
%  See json_codes/2 and json_codes/3 below.
%
%  @param JsonTerm  The JSON term
%  @param JsonChars The chars holding the JSON string representation
%  @param Options   List of options for json_read/json_write

json_chars(JsonTerm, JsonChars, Options) :-

    (var(JsonChars) ->
        json_codes(JsonTerm, JsonCodes, Options),
        atoms_codes(JsonChars, JsonCodes)
    ;
        atoms_codes(JsonChars, JsonCodes),
        json_codes(JsonTerm, JsonCodes, Options)
    ).

%-------------------------------------------------------------------------------------

%! json_codes(+JsonTerm:json, -JsonCodes:list) is det.
%! json_codes(-JsonTerm:json, +JsonCodes:list) is det.
%
%  Unify a JSON term with a list of char codes standing for the corresponding
%  JSON string representation.
%  This is acomplished either by writing the term as JSON, using json_write/2 or
%  json_write/3, or by reading the JSON codes, using json_read/2 or json_read/3.
%  Examples:
%
%  *|1a.|*
%  ~~~
%    Atom = '{"x":1,"y":2}',
%    atom_codes(Atom, Codes),
%    json_codes(Term, Codes)
%  yields
%    Term = json([x=1,y=2])  (SICStus)
%    Term = json([x=1, y=2]) (SWI-Prolog)
%  ~~~
%  *|1b.|*
%  ~~~
%    Term = json([x=1,y=2]),
%    json_codes(Term, Codes),
%    atom_codes(Atom, Codes)
%  yields
%    Atom = '{\n  "x":1,\n  "y":2\n}' (SICStus)
%    Atom = '{"x":1, "y":2}'          (SWI-Prolog)
%  ~~~
%  *|2a.|*
%  ~~~
%    Atom = '{"C":["a","b","c"]}',
%    atom_codes(Atom, Codes),
%    json_codes(Term, Codes)
%  yields
%    Term = json(['C'=[a,b,c]])   (SICStus)
%    Term = json(['C'=[a, b, c]]) (SWI-Prolog)
%  ~~~
%  *|2b.|*
%  ~~~
%    List = [a,b,c],
%    Term = json(['C'=List),
%    json_codes(Term, Codes),
%    atom_codes(Atom, Codes)
%  yields
%    Atom = '{\n  "C":["a", "b", "c"]\n}' (SICStus)
%    Atom = '{"C": ["a", "b", "c"]}'      (SWI-Prolog)
%  ~~~
%
%  @param JsonTerm  The JSON term
%  @param JsonCodes List of char codes holding the JSON string representation

json_codes(JsonTerm, JsonCodes) :-

    (var(JsonCodes) ->
        with_output_to_codes(json_write(Stream, JsonTerm),
                             Stream, JsonCodes, [])
    ;
        open_codes_stream(JsonCodes, Stream),
        call_cleanup(json_read(Stream, JsonTerm), close(Stream, [force(true)]))
    ).

%! json_codes(+JsonTerm:json, -JsonCodes:list, +Options:list) is det.
%! json_codes(-JsonTerm:json, +JsonCodes:list, +Options:list) is det.
%
%  Unify JsonTerm with a list of char codes standing for the corresponding
%  JSON string representation, according to Options. See json_codes/2 above.
%
%  @param JsonTerm  The JSON term
%  @param JsonCodes List of char codes holding the JSON string representation
%  @param Options   List of options for json_read/json_write

json_codes(Term, JsonCodes, Options) :-

    (var(JsonCodes) ->
        with_output_to_codes(json_write(Stream, Term, Options),
                             Stream, JsonCodes, [])
    ;
        open_codes_stream(JsonCodes, Stream),
        call_cleanup(json_read(Stream, Term, Options),
                     close(Stream, [force(true)]))
    ).
