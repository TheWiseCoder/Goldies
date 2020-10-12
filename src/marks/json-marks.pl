/*******************************************************************************
* FILENAME / MODULE : json-marks.pl / json_marks
*
* DESCRIPTION :
*       Utilities for handling JSON-encoded data. The representation of JSON
*       values in the SICStus and SWI-Prolog platforms is as follows:
*
*       Number  a JSON number is represented as the corresponding Prolog number;
*               as a floating point number when the JSON number has an exponent
*               or a fractional part, otherwise as an integer.
*       String  a JSON string is represented as the corresponding Prolog atom
*               (escaped surrogate pairs are combined into the corresponding
*                Unicode code point).
*       Array   a JSON array is represented as a list of the corresponding
*               Prolog terms.
*       Object  a JSON object is a sequence of name:value pairs, where each
*               nsme is a JSON string and each value is an arbitrary JSON value.
*               It is represented as a term json(Members) with Members being a
*               list of Name=Value pairs, where Name is a representation of the
*               JSON string name and Value is a representaton of the JSON value.
*       null
*       true
*       false   these special JSON literals are, by default, translated to the
*               Prolog terms @(null), @(true), and @(false), respectively.
*
* PUBLIC PREDICATES :
*       json_add(?JsonTerm, +Item(s), -JsonResult)
*       json_atom(?JsonTerm, ?JsonAtom)
*       json_atom(?JsonTerm, ?JsonAtom, +Options)
*       json_chars(?JsonTerm, ?JsonChars)
*       json_chars(?JsonTerm, ?JsonChars, +Options)
*       json_codes(?JsonTerm, ?JsonCodes)
*       json_codes(?JsonTerm, ?JsonCodes, +Options)
*       json_input(+Stream, -Term)
*       json_input(+Stream, -Term, +Options)
*       json_member(+JsonTerm, +Name, -JsonValue)
*       json_members(+JsonTerm, +Names, -JsonValues)
*       json_merge(+JsonTerms, -JsonResult)
*       json_output(+Stream, +Term)
*       json_output(+Stream, +Term, +Options)
*
* NOTES :
*       None yet.
*
*       Copyright TheWiseProgrammer 2020.  All rights reserved.
*
* REVISION HISTORY :
*
* DATE        AUTHOR            REVISION
* ----------  ----------------  ------------------------------------------------
* 2020-05-05  GT Nunes          Module creation
* 2020-05-09  GT Nunes          Added JSON to atom/codes and back
*
*******************************************************************************/

:- module(json_marks,
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
        json_output/2,
        json_output/3
    ]).

:- if(current_prolog_flag(dialect, sicstus)).   % SICStus ----------------------

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

:- elif(current_prolog_flag(dialect, swi)).     % SWI-Prolog -------------------

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

:- endif.                                       % ------------------------------   

:- use_module(library(codesio),
    [
        open_codes_stream/2,
        with_output_to_codes/4
    ]).

:- use_module('atom-marks',
    [
        atoms_codes/2
    ]).

%-------------------------------------------------------------------------------
% JSON stream input (read next value from Stream into a JSON term)

% json_input(+Stream, ?Term, +Options) - SICStus
% json_input(+Stream, -Term, +Options) - SWI-Prolog
% Stream        the stream to read from
% JSonTerm      the JSON term to unify with
% Options       the Options to guide the reading

json_input(Stream, Term) :-
    json_read(Stream, Term).

json_input(Stream, Term, Options) :-
    json_read(Stream, Term, Options).

%-------------------------------------------------------------------------------
% JSON stream output (write a JSON term to Stream)

% json_output(+Stream, +Term, +Options)
% Stream        the stream to write to
% Term          the JSON term to write to Stream
% Options       the Options to guide the writing

json_output(Stream, Term) :-
    json_write(Stream, Term).

json_output(Stream, Term, Options) :-
    json_write(Stream, Term, Options).

%-------------------------------------------------------------------------------
% add Item(s) to a JSON term

% json_add(+JsonTerm, +Item(s), -JsonResult)
% JSonTerm      the JSON term
% Item/Items    the item or list of items to add to
% JsonResult    the resulting JSON term
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

%-------------------------------------------------------------------------------
% merge a list of JSON terms into a single JSON term

% json_merge(+JsonTerms, -JsonResult)
% JsonTerms     the JSON terme to merge
% JsonResult    the resulting JSON term
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
    

%-------------------------------------------------------------------------------
% unify JsonValue with the corresponding value for Name in JsonTerm

% json_member(+JsonTerm, +Name, -JsonValue)
% JsonTerm      the JSON term
% Name          the name of the target member
%JsonValue      the value associated with the targer member
json_member(JsonTerm, Name, JsonValue) :-

    json(JsonMembers) = JsonTerm,
    !,
    % fail point
    memberchk(Name=Value, JsonMembers),
    JsonValue = Value.

%-------------------------------------------------------------------------------
% unify JsonValues with the corresponding values for Names in JsonTerm

% json_members(+JsonTerm, +Names, -JsonValues)
% JsonTerm      the JSON term
% Names         the names of the target members
%JsonValues     the values associated with the targer members

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

%-------------------------------------------------------------------------------
% unify a JSON term with an atom standing for the corresponding JSON
%   string representation - see json_codes/2-json_codes/3 below

% json_atom(?JsonTerm, ?JsonAtom, +Options)
% JsonTerm      the JSON term
% JsonAtom      the atom holding the JSON string representation
% Options       json_read/json_write options

json_atom(JsonTerm, JsonAtom) :-

    (var(JsonAtom) ->
        json_codes(JsonTerm, JsonCodes),
        atom_codes(JsonAtom, JsonCodes)
    ;
        atom_codes(JsonAtom, JsonCodes),
        json_codes(JsonTerm, JsonCodes)
    ).

json_atom(JsonTerm, JsonAtom, Options) :-

    (var(JsonAtom) ->
        json_codes(JsonTerm, JsonCodes, Options),
        atom_codes(JsonAtom, JsonCodes)
    ;
        atom_codes(JsonAtom, JsonCodes),
        json_codes(JsonTerm, JsonCodes, Options)
    ).

%-------------------------------------------------------------------------------
% unify a JSON term with a list of chars standing for the corresponding
%   JSON string representation (see json_codes/2-json_codes/3 below)

% json_chars(?JsonTerm, ?JsonChars, +Options)
% JsonTerm      the JSON term
% JsonChars     the chars holding the JSON string representation
% Options       json_read/json_write options

json_chars(JsonTerm, JsonChars) :-

    (var(JsonChars) ->
        json_codes(JsonTerm, JsonCodes),
        atoms_codes(JsonChars, JsonCodes)
    ;
        atoms_codes(JsonChars, JsonCodes),
        json_codes(JsonTerm, JsonCodes)
    ).

json_chars(JsonTerm, JsonChars, Options) :-

    (var(JsonChars) ->
        json_codes(JsonTerm, JsonCodes, Options),
        atoms_codes(JsonChars, JsonCodes)
    ;
        atoms_codes(JsonChars, JsonCodes),
        json_codes(JsonChars, JsonCodes, Options)
    ).

%-------------------------------------------------------------------------------
% Unify a JSON term with a list of char codes standing for the corresponding
% JSON string representation - this is acomplished either by writing the term
% as JSON, using json_write/2 or json_write/3, or by reading the JSON codes,
% using json_read/2 or json_read/3. Examples:
%
%   1a. Atom = '{"x":1,"y":2}',
%       atom_codes(Atom, Codes),
%       json_codes(Term, Codes)
%       yields
%         Term = json([x=1,y=2])  (SICStus)
%         Term = json([x=1, y=2]) (SWI-Prolog)
%
%   1b. Term = json([x=1,y=2]),
%       json_codes(Term, Codes),
%       atom_codes(Atom, Codes)
%       yields
%         Atom = '{\n  "x":1,\n  "y":2\n}' (SICStus)
%         Atom = '{"x":1, "y":2}'          (SWI-Prolog)
%
%   2a. Atom = '{"C":["a","b","c"]}',
%       atom_codes(Atom, Codes),
%       json_codes(Term, Codes)
%       yields
%         Term = json(['C'=[a,b,c]])   (SICStus)
%         Term = json(['C'=[a, b, c]]) (SWI-Prolog)
%
%   2b. List = [a,b,c],
%       Term = json(['C'=List),
%       json_codes(Term, Codes),
%       atom_codes(Atom, Codes)
%       yields
%         Atom = '{\n  "C":["a", "b", "c"]\n}' (SICStus)
%         Atom = '{"C": ["a", "b", "c" ]}'     (SWI-Prolog)

% json_codes(?JsonTerm, ?JsonCodes, +Options)
% JsonTerm      the JSON term
% JsonCodes     the char codes holding the JSON string representation
% Options       json_read/json_write options

json_codes(JsonTerm, JsonCodes) :-

    (var(JsonCodes) ->
        with_output_to_codes(json_write(Stream, JsonTerm),
                             Stream, JsonCodes, [])
    ;
        open_codes_stream(JsonCodes, Stream),
        call_cleanup(json_read(Stream, JsonTerm), close(Stream, [force(true)]))
    ).

json_codes(Term, JsonCodes, Options) :-

    (var(JsonCodes) ->
        with_output_to_codes(json_write(Stream, Term, Options),
                             Stream, JsonCodes, [])
    ;
        open_codes_stream(JsonCodes, Stream),
        call_cleanup(json_read(Stream, Term, Options),
                     close(Stream, [force(true)]))
    ).
