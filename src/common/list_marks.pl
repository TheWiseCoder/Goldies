/*******************************************************************************
* FILENAME / MODULE : list_marks.pl / list_marks
*
* DESCRIPTION :
*       Miscellaneous small list-related utilities.
*
* PUBLIC PREDICATES :
*       append3(+List1, +List2, +List3, -List123)
*       append4(+List1, +List2, +List3, +List4, -List1234)
*       append5(+List1, +List2, +List3, +List4, +List5, -List12345)
*       convlist_first(:Goal, +List, -Element)
*       list_common(+List1, +List2, -ListCommon)
*       list_compacts(+List, -ListsCompact)
*       list_count_same(+List, +Pos, -Count)
*       list_fill(+Count, +Item, -List)
*       list_minus_list(+ListSearch, +ListElements, -ListResult)
*       list_replace0(+Pos0, +List, +Element, -ListResult)
*       list_replace1(+Pos1, +List, +Element, -ListResult)
*       list_same(+List)
*       list_same(+List, ?Before, ?Length, ?After)
*       list_same0(+List, +Pos0, +Count)
*       list_same1(+List, +Pos1, +Count)
*       list_split(+List, +Sep, -Lists)
*       list_values(+Count, +Start, +Offset, -Values)
*       lists_common(+List1, +List2)
*       lists_consolidate(+ListsRefs, +ListElems, -ListsResult)
*       lists_find(+Lists, +Element, -Pos1)
*       lists_flatten(+Lists, -List)
*       lists_start_with(+Lists, +Sublist, -List)
*
* NOTES :
*       None yet.
*
*       Copyright Instituto Modal 2020.  All rights reserved.
*
* REVISION HISTORY :
*
* DATE        AUTHOR            REVISION
* ----------  ----------------  ------------------------------------------------
* 2020-03-20  GT Nunes          Module creation
* 2020-04-10  GT Nunes          Added this header
* 2020-08-09  GT Nunes          Added conditional compilation for SICStus/SWI
*
*******************************************************************************/

:- module(list_marks,
    [
        append3/4,
        append4/5,
        append5/6,
        convlist_first/3,
        list_common/3,
        list_compacts/2,
        list_count_same/3,
        list_fill/3,
        list_minus_list/3,
        list_replace0/4,
        list_replace1/4,
        list_same/1,
        list_same/4,
        list_same0/3,
        list_same1/3,
        list_split/3,
        list_values/4,
        lists_common/2,
        lists_consolidate/3,
        lists_flatten/2,
        lists_find/3,
        lists_start_with/3
    ]).

:- meta_predicate convlist_first(2, +, -).

:- if(current_prolog_flag(dialect, sicstus)).   % SICStus ----------------------

:- use_module(library(between),
    [
        numlist/3
    ]).

:- use_module(library(lists),
    [
        delete/3,
        is_list/1,
        nth0/3,
        nth0/4,
        nth1/3,
        nth1/4,
        reverse/2,
        sublist/3,
        sublist/5
    ]).

:- elif(current_prolog_flag(dialect, swi)).     % SWI-Prolog -------------------

:- use_module(library(lists),
    [
        delete/3,
        nth0/3,
        nth0/4,
        nth1/3,
        nth1/4,
        numlist/3,
        reverse/2
    ]).

:- use_module('../swi/port_lists',
    [
        sublist/3,
        sublist/5
    ]).

:- endif.                                       % ------------------------------

%-------------------------------------------------------------------------------
% join together extra number of lists by appension

% append3(+List1, +List2, +List3, -List123)
% List1         the head list
% List2         the middle list
% List3         the tail list
% List123       the final list
append3(List1, List2, List3, List123) :-

    append(List2, List3, List23),
    append(List1, List23, List123).

% append4(+List1, +List2, +List3, +List4-List1234)
% List1         the head list
% List2         the 1st middle list
% List3         the 2nd middle list
% List4         the tail list
% List1234      the final list
append4(List1, List2, List3, List4, List1234) :-

    append(List3, List4, List34),
    append(List1, List2, List12),
    append(List12, List34, List1234).

% append5(+List1, +List2, +List3, +List4-List1234)
% List1         the head list
% List2         the 1st middle list
% List3         the 2nd middle list
% List4         the 3nd middle list
% List5         the tail list
% List12345     the final list
append5(List1, List2, List3, List4, List5, List12345) :-

    append(List3, List4, List34),
    append(List1, List2, List12),
    append(List12, List34, List1234),
    append(List1234, List5, List12345).

%-------------------------------------------------------------------------------
% Similar to convlist/3, but only the first element for which
% call(Goal, Element, _) succeeds is returned. Fail if no element succeeds.

% unify Element with the first element in List that satisfies Goal
% convlist_first(:Goal, +List, -Element)
% Goal      predicate to be invoked with the elements of List
% List      list of parameters for Goal invocation
% Element   the first element in List with which a Goal invocation succeeds

convlist_first(_Goal, [], _Element) :-
    !, fail.

convlist_first(Goal, [Head|List], Element) :-

    ( call(Goal, Head, Element) ;
      convlist_first(Goal, List, Element) ).

%-------------------------------------------------------------------------------
% find the 1-based position of the first list
% containing a given element, in a list of lists

% lists_find(+Lists, +Element, -Pos1)
% Lists         list of lists under inspection
% Element       element being sought
% Pos1          1-based position of the list containing the element
lists_find(Lists, Element, Pos1) :-

    length(Lists, Count),
    !,
    % fail point
    lists_find_(Count, Lists, Element, Pos1).

% (failure)
lists_find_(0, _Lists, _Element, _Pos1) :-
    !, fail.

% (iterate)
lists_find_(Count, Lists, Element, Pos1) :-

    nth1(Count, Lists, List),
    (memberchk(Element, List) ->
        Pos1 = Count
    ;
        CountNext is Count - 1,
        !,
        % fail point
        lists_find_(CountNext, Lists, Element, Pos1)
    ).

%-------------------------------------------------------------------------------
% find the first list starting with the given sublist, in a list of lists

% lists_start_with(+Lists, +Sublist, -List)
% Lists         list of lists under inspection
% Elements      element being sought
% List          the list starting with the given elements

% (failure)
lists_start_with([], _Sublist, _List) :-
    !, fail.

% (iterate)
lists_start_with([Head|Lists], Sublist, List) :-

    (sublist(Head, Sublist, 0) ->
        List = Head
    ;
        !,
        % fail point
        lists_start_with(Lists, Sublist, List)
    ).

%-------------------------------------------------------------------------------
% consolidate a list of lists ListsRefs with a list ListElems -
%   if a list ListElem2 within ListsRef contains the first element of
%   ListElem, the elements of the ListElem are appended to its head,
%   otherwise ListElem is appended to the head of ListsRef. Examples:
%
%     1. lists_consolidate([[1,d,b],[4,c,f]], [c,j,k], ListsResult)
%        yields  ListsResult = [[1,d,b],[c,j,k,4,c,f]]
%
%     2. lists_consolidate([[1,d,b],[4,c,f]], [d,j,k], ListsResult)
%        yields  ListsResult = [[d,j,k,1,d,b],[4,c,f]]
%
%     3. lists_consolidate([[1,d,b],[4,c,f]], [j,k,l], ListsResult)
%        yields ListsResult = [[j,k,l],[1,d,b],[4,c,f]]

% lists_consolidate(+ListsRefs, +ListElems, -ListsResult)
% ListsRefs     the reference list of lists
% ListElems     the list to be consolidated into the list of lists
% ListsResult      the resulting list of lists
lists_consolidate(ListsRefs, ListElems, ListsResult) :-

    [Elem|_] = ListElems,

    % is there a list containg Elem ?
    (lists_find(ListsRefs, Elem, Pos) ->

        % yes, ListElems2 is a list containing the element Elem
        % (ListsRems has the other lists)
        nth1(Pos, ListsRefs, ListElems2, ListsRems)
    ;
        % no, use empty list ListElemse2 inst\read
        Pos = 1,
        ListElems2 = [],
        ListsRems = ListsRefs
    ),

    % join the elements of ListElems and ListElems2 in ListElems3
    append(ListElems, ListElems2, ListElems3),

    % insert the new list at the appropriate place in the list of lists
    nth1(Pos, ListsResult, ListElems3, ListsRems).

%-------------------------------------------------------------------------------
% remove all occurences of a given element in a specified list

% remove from ListSearch all occurrences of elements in ListElements
% delete(+ListSearch, +ListElements, -ListResult)
% ListSearch        list to be inspected for elements removal
% ListElements      element to be removed
% ListResult        the final purged list

% (done)
list_minus_list(ListResult, [], ListResult).

% (iterate)
list_minus_list(ListSearch, [E|ListElements], ListResult) :-

    delete(ListSearch, E, ListPurged),
    list_minus_list(ListPurged, ListElements, ListResult).

%-------------------------------------------------------------------------------
% replace element in a list

% replace the element at 0-based Pos with Element
% list_replace0(+Pos0, +List, +Element, -ListResult)
% Pos0          the 0-based target position
% List          the source list
% Element       the replacing element
% ListResult    the new list with Element at position Pos
list_replace0(Pos0, List, Element, ListResult) :-

    nth0(Pos0, List, _, ListTemp),
    nth0(Pos0, ListResult, Element, ListTemp).


% replace the element at 1-based Pos with Element
% list_replace0(+Pos1, +List, +Element, -ListResult)
% Pos1          the 1-based target position
% List          the source list
% Element       the replacing element
% ListResult    the new list with Element at position Pos
list_replace1(Pos1, List, Element, ListResult) :-

    nth1(Pos1, List, _, ListTemp),
    nth1(Pos1, ListResult, Element, ListTemp).

%-------------------------------------------------------------------------------
% unify Values with a list with Count calculated values as elements

% list_values(+Count, +Start, +Offset, -Values)
% Count         number of elements
% Start         the start value
% Offset        the offset
% Values        list with the values produced
list_values(Count, Start, Offset, Values) :-
    list_values_(Count, Start, Offset, [], Values).

% list_values_(+Count, +Value, +Offset, +Values)
% Count              number of elements
% Start             the value to add to list
% Offset            the offset
% ValuesProgress    working list with the values produced
% ValuesFinal       final list with the values produced

% (done)
list_values_(0, _Value, _Offset, ValuesProgress, ValuesFinal) :-
    reverse(ValuesProgress, ValuesFinal).

% (iterate)
list_values_(Count, Value, Offset, ValuesProgress, ValuesFinal) :-

    CountNext is Count - 1,
    ValueNext is Value + Offset,
    list_values_(CountNext, ValueNext, Offset,
                 [Value|ValuesProgress], ValuesFinal).

%-------------------------------------------------------------------------------
% Recursively flatten a list of lists. The original order of the elements
% is kept, and repeating values are not removed. Note that append/2 flattens
% only the first level within the list of lists.

% lists_flatten(+Lists, -List)
% Lists     list of lists to flatten
% List      flattened list

lists_flatten([], List) :-
    List = [].

lists_flatten(Lists, List) :-
    lists_flatten_(Lists, [], List).

% (done)
lists_flatten_([], ListFinal, ListFinal).

% (iterate)
lists_flatten_([Head|Lists], ListProgress, ListFinal) :-

    % is the head element itself a list ?
    (is_list(Head) ->
        % yes, so flatten it first
        lists_flatten_(Head, [], List),
        append(ListProgress, List, ListRevised)
    ;
        % no, so add it to the flattened result
        append(ListProgress, [Head], ListRevised)
    ),

    % go for the next element
    lists_flatten_(Lists, ListRevised, ListFinal). 

%-------------------------------------------------------------------------------
% unify List with a list containing Count instances of Item
% (note that Count = 0 is acceptable)

% list_fill(+Count, +Item, -List)
% Count     then number of instances
% Item      the item to append to list
% List      the final list

list_fill(Count, Item, List) :-
    list_fill_(Count, Item, [], List).

% (done)
list_fill_(0, _Item, ListFinal, ListFinal).

% (iterate)
list_fill_(Count, Item, ListProgress, ListFinal) :-

    % fail point
    Count > 0,
    CountNext is Count - 1,
    list_fill_(CountNext, Item, [Item|ListProgress], ListFinal).

%-------------------------------------------------------------------------------
% unify ListCommon with the elements common to List1 and List2,
% in the order they are found in List1

% list_common(+List1, List2, -ListCommon)
list_common(List1, List2, ListCommon) :-
    list_common_(List1, List2, [], ListCommon).

% (done)
list_common_([], _List2, CommonsProgress, CommonsFinal) :-
    reverse(CommonsProgress, CommonsFinal).

% (iterate
list_common_([Elem|List1], List2, CommonsProgress, CommonsFinal) :-

    % is Elem in List2 ?
    (memberchk(Elem, List2) ->
        % yes, so use it
        CommonsRevised = [Elem|CommonsProgress]
    ;
        % no, so disregard it
        CommonsRevised = CommonsProgress
    ),

    % go for the next element
    list_common_(List1, List2, CommonsRevised, CommonsFinal).

%-------------------------------------------------------------------------------
% Unify a list of integers with a list of lists, each one of them containing
% a range of values found in the original list. The original list must have
% unique element and be ascendingly sorted, otherwise the compactification
% will yield unpredictable results. Examples:
%
%   1. list_compacts([1,3,4,6,7,8,10], ListsCompact)
%      yields ListsCompact = [[1,1],[3,4],[6,8],[10,10]]
%
%   2. list_compacts(List, [[1,1],[3,4],[6,8],[10,10]])
%      yields List = [1,3,4,6,7,8,10]

% list_compacts(+List, -ListsCompact)
% List          the input list
% ListsCompact  the compactified resulting list

% (done)
list_compacts([], ListsCompact) :-
    ListsCompact = [].

% (done)
list_compacts(List, []) :-
    List = [].

% (start)
list_compacts(List, ListsCompact) :-

    (var(List) ->
        compacts_list_(ListsCompact, [], List)
    ;
        [Prev|Tail] = List,
        list_compacts_(Tail, [[Prev,Prev]], ListsCompact)
    ).

% (done)
list_compacts_([], [[First,Last]|ListsProgress], ListsFinal) :-
    reverse([[Last,First]|ListsProgress], ListsFinal).

% (iterate)
list_compacts_([Elem|List], [[First,Last]|ListsProgress], ListsFinal) :-

    % is the current element in proper sequence ?
    (Elem =:= First + 1 ->
        % yes, so go for the next element
        list_compacts_(List, [[Elem,Last]|ListsProgress], ListsFinal)
    ;
        % no, so register the interval and proceed
        list_compacts_(List, [[Elem,Elem],[Last,First]|ListsProgress],
                       ListsFinal)
    ).

% (done)
compacts_list_([], ListFinal, ListFinal).

% (iterate)
compacts_list_([[First,Last]|ListsCompact], ListProgress, ListFinal) :-

    numlist(First, Last, List),
    append(ListProgress, List, ListRevised),
    compacts_list_(ListsCompact, ListRevised, ListFinal).

%-------------------------------------------------------------------------------
% split a list into various lists, based on a given separator
% (if no separator exists, a list of lists with one element is returned)

% list_split(+List, +Sep, -Lists)
% List          the list under inspection
% Sep           the separator
% Lists         the resulting list of lists
list_split(List, Sep, Lists) :-
    list_split_(List, Sep, [], Lists).

list_split_(List, Sep, ListsProgress, ListsFinal) :-

    (nth0(Pos, List, Sep) ->
        sublist(List, ListPrev, 0, Pos, _),
        PosSeq is Pos + 1,
        sublist(List, ListPost, PosSeq, _, 0),
        list_split_(ListPost, Sep, [ListPrev|ListsProgress], ListsFinal)
    ;
        reverse([List|ListsProgress], ListsFinal)
    ).

%-------------------------------------------------------------------------------
% count occurrences of the same element contiguously
% from a given 0-based position within a list

% list_count_same(+List, +Pos, -Count)
% List          the list under inspection
% Pos           the 0-based position of the element to count
% Count         the number of cosecutive occurrences of the element
list_count_same(List, Pos, Count) :-

    length(List, Len),
    !,
    % fail points
    Pos>= 0,
    Pos < Len,
    nth0(Pos,List, Elem),
    list_count_same_(Pos, Len, Elem, List, 0, Count).

% list_count_same_(+Pos, +Len, +Elem, +List, +CountProgress, -CountFinal)
% Pos           the position of the element to count
% Len           the length of the list
% Elem          the element being counted
% List          the list under inspection
% CountProgress the working occurrences count
% CountFinal    the final occurrences count

% (done)
list_count_same_(Len, Len, _Elem, _List, CountFinal, CountFinal).

% (iterate)
list_count_same_(Pos, Len, Elem, List, CountProgress, CountFinal) :-

    (nth0(Pos, List, Elem) ->
        % acknowledge consecutive occurrence and proceed
        Count is CountProgress + 1,
        PosNext is Pos + 1,
        list_count_same_(PosNext, Len, Elem, List, Count, CountFinal)
    ;
        % done (no more consecutive occurrences)
        CountFinal = CountProgress
    ).

%-------------------------------------------------------------------------------
% assert whether all elements in a list or sublist are the same
% (note that empty lists will fail)

% list_same(+List)
% List  list being inspected
list_same(List) :-

    length(List, Len),
    !,
    % fail point
    (Len = 1 ; list_same_(List)).

% (done)
list_same_([_|[]]).

% (iterate)
list_same_(List) :-

    [H,H|ListNew] = List,
    !,
    % fail point
    list_same_([H|ListNew]).

% list_same(+List, ?Before, ?Length, ?After)
% List          list being inspected
% Before        number of items before the sublist
% Length        length of sublist to inspect
% After         number of items after the sublist
list_same(List, Before, Length, After) :-

    sublist(List, Sublist, Before, Length, After),
    !,
    % fail point
    list_same(Sublist).

% list_same0(+List, +Pos0, +Count)
% List          list being inspected
% Pos0          0-based starting position
% Count         number of items to compare
list_same0(List, Pos0, Count) :-

    sublist(List, Sublist, Pos0, Count, _),
    !,
    % fail point
    list_same(Sublist).

% list_same1(+List, +Pos1, +Count)
% List          list being inspected
% Pos1          1-based starting position
% Count         number of items to compare
list_same1(List, Pos1, Count) :-

    Pos is Pos1 - 1,
    sublist(List, Sublist, Pos, Count, _),
    !,
    % fail point
    list_same(Sublist).

%-------------------------------------------------------------------------------
% assert whether the given lists have at least one element in common

% lists_common(+List1, +List2)
% List1     first list to compare
% List2     second list ot compare

lists_common([], _List2) :- !, fail.
lists_common(_List1, []) :- !, fail.

lists_common([Elem|List1], List2) :-

    % is element from ListA also in ListB
    (memberchk(Elem, List2) ->
        % yes, so success
        true
    ;
        % no, so try again with the next element
        !,
        % fail point
        lists_common(List1, List2)
    ).
