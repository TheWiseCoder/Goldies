/*******************************************************************************
* FILENAME / MODULE : port_lists.pl / port_lists
*
* DESCRIPTION :
*       These are predicates providing a portability layer on SICStus-style
*       lists handling for the SWI-Prolog platform.
*
* PUBLIC PREDICATES :
*       keyclumps(+Pairs, -Clumps)
*       sublist(+Whole, ?Part, ?Before)
*       sublist(+Whole, ?Part, ?Before, ?Length)
*       sublist(+Whole, ?Part, ?Before, ?Length, ?After)
*       subseq0(+List, +Subseq)
*       subseq1(+List, +Subseq)
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

:- module(port_lists,
    [
        keyclumps/2,
        sublist/3,
        sublist/4,
        sublist/5,
        subseq0/2,
        subseq1/2
    ]).

%-------------------------------------------------------------------------------
% Implementation of SICStus' subseq/0, subseq/1

% true when Subseq is a subsequence of List
% subseq0(+List, +Subseq)
% List      the reference list
% Subseq    list for subsequence assertion

subseq0(List, List).

subseq0(List, Subseq) :-
    subseq1(List, Subseq).

% true when Subseq is a proper subsequence of List
% subseq0(+List, +Subseq)
% List      the reference list
% Subseq    list for proper subsequence assertion

subseq1([_Head|Tail], Subseq) :-
    subseq0(Tail, Subseq).

subseq1([Head|Tail], [Head|Subseq]) :-
    subseq1(Tail, Subseq).

%-------------------------------------------------------------------------------
% implementation of SICStus' sublist/3, sublist/4, sublist/5
%
% These relationships hold:
%   Whole = Alpha || Part || Omega
%   length(Alpha, Before)
%   length(Part, Length)
%   length(Omega, After)

% sublist(+Whole, ?Part, ?Before)
% Whole     the source list
% Part      the result list
% Before    length in Whole before Part
sublist(Whole, Part, Before) :-
    sublist(Whole, Part, Before, _, _).

% sublist(+Whole, ?Part, ?Before, ?Length)
% Whole     the source list
% Part      the result list
% Before    length in Whole before Part
% Length    length of Part
sublist(Whole, Part, Before, Length) :-
    sublist(Whole, Part, Before, Length, _).

% sublist(+Whole, ?Part, ?Before, ?Length, ?After)
% Whole     the source list
% Part      the result list
% Before    length in Whole before Part
% Length    length of Part
% After     length in Whole after Part

sublist(Whole, Part, Before, Length, After) :-

    integer(Before),
    ( integer(Length) -> true
    ;
      var(Length) , proper_length(Part, Length)
    ),

    !,
    Before >= 0,
    Length >= 0,
    append_make(Before, Suffix, Whole),
    append_make(Length, Part, Rest, Suffix),
    length(Rest, After).

sublist(Whole, Part, Before, Length, After) :-

    proper_length(Whole, LengthOfWhole),    
    ( integer(Before) -> true
    ; var(Before)
    ),

    ( integer(After) -> true
    ; var(After)
    ),

    ( integer(Length) -> true
    ; nonvar(Length) -> fail
    ; proper_length(Part, Length) -> true
    ; true
    ),

    !,
    append_length(Suffix, Whole, Before),   
    ( var(Length) -> true 
    ; Length =< LengthOfWhole-Before
    ),

    ( var(After) -> true
    ; After =< LengthOfWhole-Before
    ),

    append_length(Part, _, Suffix, Length),
    After is LengthOfWhole - Before - Length.
    
append_length(Prefix, Suffix, List, Length) :-
    ( var(Length) ->
      append_find(Prefix, Suffix, List, 0, Length)
    ; integer(Length) ->
      Length >= 0,
      append_make(Length, Prefix, Suffix, List)
    ; must_be(Length, integer,
      append_length(Prefix, Suffix, List, Length), 4)
    ).

append_find([], List, List, N, N).

append_find([Head|Prefix], Suffix, [Head|List], N0, N) :-
    N1 is N0 + 1,
    append_find(Prefix, Suffix, List, N1, N).

append_make(0, Prefix, Suffix, List) :- !,
    Prefix = [], Suffix = List.

append_make(N, [Head|Prefix], Suffix, [Head|List]) :-

    M is N - 1,
    append_make(M, Prefix, Suffix, List).
   
append_length(Suffix, List, Length) :-

    ( var(Length) ->
      append_find(Suffix, List, 0, Length)
    ; integer(Length) ->
      Length >= 0,
      append_make(Length, Suffix, List)
    ; fail
    ).

append_find(List, List, N, N).

append_find(Suffix, [_|List], N0, N) :-

    N1 is N0 + 1,
    append_find(Suffix, List, N1, N).

append_make(0, Suffix, List) :- !,
    Suffix = List.

append_make(N, Suffix, [_|List]) :-

    M is N - 1,
    append_make(M, Suffix, List).

%-------------------------------------------------------------------------------
% Unify Clumps with a list of lists of key-value pairs obtained from Pairs,
% each list sharing the same key. For example:
%   keyclumps([a-2,a-3,b-1,b-2,c-4], Clumps) yields
%   C = [[a-2,a-3],[b-1,b-2],[c-4]]

% keyclumps(+Pairs, -Clumps)
% Pairs     the list key-value pairs
% Clumps    the list of clumped lists
keyclumps(Pairs, Clumps):-

    group_pairs_by_key(Pairs, Groups),
    keyclumps_1(Groups, [], Clumps).

keyclumps_1([], ClumpsProgress, ClumpsFinal) :-
	sort(ClumpsProgress, ClumpsFinal).

keyclumps_1([Key-Values|Groups], ClumpsProgress, ClumpsFinal) :-

    keyclumps_2(Key, Values, [], ClumpFinal),
    keyclumps_1(Groups, [ClumpFinal|ClumpsProgress], ClumpsFinal).

keyclumps_2(_Key, [], ClumpProgress, ClumpFinal) :-
	sort(ClumpProgress, ClumpFinal).

keyclumps_2(Key, [Value|Values], ClumpProgress, ClumpFinal) :-
	keyclumps_2(Key, Values, [Key-Value|ClumpProgress], ClumpFinal).
