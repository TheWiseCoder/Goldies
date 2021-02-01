/*******************************************************************************
* FILENAME / MODULE : port_layer.pl / port_layer
*
* DESCRIPTION :
*       These are predicates providing a portability layer for the SICStus
*       Prolog platform, by implementing some unique, or convenient,
*       SWI-Prolog features.
*
* PUBLIC PREDICATES :
*       file_base_name(+FilePath, -FileName)
*       file_directory_name(+FilePath, -DirectoryName)
*       flag(+Key, -Old, +New)
*       getenv(+Name, -Value)
*       get_flag(+Key, -Value)
*       guid(-Guid)
*       guids(+N, -Guids)
*       pairs_keys_values(?Pairs, ?Keys, ?Values)
*       randseq(+K, +N, -Set)
*       randset(+K, +N, -Set)
*       read_line_to_codes(+Stream, -Codes)
*       setenv(+Name, +Value)
*       set_flag(+Key, +Value)
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
* 2020-08-08  GT Nunes          Module creation
*
*******************************************************************************/

:- module(port_layer,
    [
        file_base_name/2,
        file_directory_name/2,
        flag/3,
        getenv/2,
        get_flag/2,
        guid/1,
        guids/2,
        pairs_keys_values/3,
        randseq/3,
        randset/3,
        read_line_to_codes/2,
        setenv/2,
        set_flag/2
    ]).

:- use_module(library(codesio),
    [
        format_to_codes/3
    ]).

:- use_module(library(lists),
    [
        keys_and_values/3
    ]).

:- use_module(library(random),
    [
        random/3,
        random_numlist/4,
        random_permutation/2
    ]).

:- use_module(library(system),
    [
        environ/2
    ]).

:- use_module('../common/repeat_goal',
    [
        repeat_goal/3
    ]).

:- use_module('simple_counter',
    [
        counter_create/2,
        counter_dec/2,
        counter_destroy/1
    ]).

%-------------------------------------------------------------------------------------

% Extract the file name part from a path. Unify FileName with the empty atom ('')
% if FilePath is '', or if ends with '/'. Otherwise, unify FileName with the
% expected value. Note that this behaviour differs from SWI-Prolog's
% file_base_name/2.
%
% file_base_name(+FilePath, -FileName)
file_base_name(FilePath, FileName) :-

    atom_length(FilePath, Len),
    counter_create(FilePath, Len),

    repeat,
        counter_dec(FilePath, Before),
        (Before = -1 ; sub_atom(FilePath, Before, 1, _, '/')),

    !,
    Pos is Before + 1,
    ( Pos = 0 ->
        FileName = FilePath
    ; Pos = Len ->
        FileName = ''
    ; otherwise ->
        sub_atom(FilePath, Pos, _, 0, FileName)
    ),
    counter_destroy(FilePath).

% Extract the directory name part from a path. Unify DirectoryName with '.'
% if FilePath is '' or '/', or if it does not contain '/'. Otherwise,
% unify DirectoryName with the expected value. DirectoryName will not have a
% trailing '/'. Note that this behaviour differs from SWI-Prolog's
% file_directory_name/2.
%
% file_directory_name(+FilePath, -DirectoryName)
file_directory_name(FilePath, DirectoryName) :-

    atom_length(FilePath, Len),
    counter_create(FilePath, Len),

    repeat,
        counter_dec(FilePath, Before),
        (Before = -1 ; sub_atom(FilePath, Before, 1, _, '/')),

    !,
    Pos is Before + 1,
    (Pos < 2 ->
        DirectoryName = '.'
    ;
        sub_atom(FilePath, 0, Before, _, DirectoryName)
    ),
    counter_destroy(FilePath).

% unify a list of key-value pairs with separate lists of keys and values
% pairs_keys_values(?Pairs, ?Keys, ?Values)
pairs_keys_values(Pairs, Keys, Values) :-
    keys_and_values(Pairs, Keys, Values).

% unify Codes with the line codes read from Stream
% read_line_to_codes(+Stream, -Codes)
% Stream    the input stream
% Codes     the line codes read
read_line_to_codes(Stream, Codes) :-
    read_line(Stream, Codes).

%-------------------------------------------------------------------------------------
% Flag operations in SWI-Prolog style.

% True when Old is the current value of the flag Key, and the flag has been
% set to New. New can be an arithmetic expression. For compatibility with the
% SWI-Prolog platform, the flag is initialized to 0 on first access. Note:
%   1. To retrieve the flag's value without changing it, use
%      flag(Key, Val, Val)
%   2. This predicate can be used to create a shared global counter, as
%      illustrated in this example:
%      next_id(Id) :- flag(my_id, Id, Id + 1).
% flag(+Key, -Old, +New)
% Key   atom identifying the flag
% Old   the flag's old value
% New   the flag's new value
flag(Key, Old, New) :-

    (bb_get(Key, Old) ; Old = 0),
    (Val is New ; Val = New),
    bb_put(Key, Val),
    !.

% Unify Value with the current value of the flag. As per SWI-Prolog,
% if the flag does not exist, a new flag with value '0' (zero) is created.
% get_flag(+Key, -Value)
% Key       atom identifying the flag
% Value     the flag's current value
get_flag(Key, Value) :-

    ( bb_get(Key, Value)
    ; (Value = 0 , bb_put(Key, Value)) ).

% Create the flag Key, if it does not exist, and set its value to Value.
% set_flag(+Key, +Value)
% Key       atom identifying the flag
% Value     the flag's new value
set_flag(Key, Value) :-
    bb_put(Key, Value).

%-------------------------------------------------------------------------------------
% generate a list of approximately K unique random integers in the range 1..N

% Set is an unsorted list of approximately K unique random integers in 1..N
% randseq(+K, +N, -Set)
% K     the number of random integers to generate (integer)
% N     the ceiling of the interval (integer)
% Set   the unsorted list
randseq(K, N, Set) :-

    % fail point
    randset(K, N, Ints),
    random_permutation(Ints, Set).

% Set is a sorted list of approximately K unique random integers in 1..N
% randset(+K, +N, -Set)
% K     the number of random integers to generate (integer)
% N     the ceiling of the interval (integer)
% Set   the sorted list
randset(K, N, Set) :-

    % fail point
    K =< N,
    P is float(K) / float(N),
    random_numlist(P, 1, N, Set).

%-------------------------------------------------------------------------------------
% GUID generation

% generate N GUIDs (UUID Version 4)
% guids(+N, -Guids)
% N         number of GUIDs to generate
% Guids     the list of new GUIDs
guids(N, Guids) :-
    % generate the GUIDs
    repeat_goal(guid, N, Guids).

% generate a GUID (UUID Version 4)
% guid(-Guid)
% Guid      the new GUID
guid(Guid) :-

    % 8 random integers in the 1..65535 range are needed, formatted as
    % AAAABBBB-CCCC-4DDD-NEEE-FFFFGGGGHHHH
    %   - A..H : hex digits in range 0..f
    %   - N    : hex digits in range 8..b

    % 1st block
    random(1, 0x10000, R1a),
    random(1, 0x10000, R1b),

    % 2nd block
    random(1, 0x10000, R2),

    % 3rd block - most-significant digit must be 0x4
    random(1, 0x01000, R3a),
    R3 is 0x4000 + R3a,

    % 4th block - most-significant digit must be in range 0x8..0xb
    random(1, 0x03000, R4a),
    R4 is 0x8000 + R4a,

    % 5th block
    random(1, 0x10000, R5a),
    random(1, 0x10000, R5b),
    random(1, 0x10000, R5c),

    % assemble the GUID
    guid_(R1a, A1a),
    guid_(R1b, A1b),
    guid_(R2, A2),
    guid_(R3, A3),
    guid_(R4, A4),
    guid_(R5a, A5a),
    guid_(R5b, A5b),
    guid_(R5c, A5c),
    format_to_codes('~a~a-~a-~a-~a-~a~a~a',
                    [A1a,A1b,A2,A3,A4,A5a,A5b,A5c], Codes),
    atom_codes(Guid, Codes).

% unify hex with Val represented as a 4-digit hexadecimal
guid_(Val, Hex) :-

    format_to_codes('000~16r',[Val], Codes),
    atom_codes(A, Codes),
    sub_atom(A, _, 4, 0, Hex).

%-------------------------------------------------------------------------------------

% unify Value with the value of the environment variable Name
% getenv(+Name, -Value)
% Name      name of the environment variable
% Value     corresponding value
getenv(Name, Value) :-
    environ(Name, Value).

% set the environment variable Name to Value
% setenv(+Name, +Value)
% Name      name of the environment variable
% Value     value to set
setenv(Name, Value) :-
    environ(Name, Value).
