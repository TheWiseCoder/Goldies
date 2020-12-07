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
        lists_find/3,
        lists_flatten/2,
        lists_start_with/3
    ]).

/** <module>  Miscellaneous small list-related utilities

@author GT Nunes
@version 1.0
@copyright (c) 2020 GT Nunes
@license BSD-3-Clause License
*/

%-------------------------------------------------------------------------------------

:- meta_predicate convlist_first(2, +, -).

:- if(current_prolog_flag(dialect, sicstus)).

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

:- elif(current_prolog_flag(dialect, swi)).

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

:- endif.

%-------------------------------------------------------------------------------------
% join together extra number of lists by appension

%! append3(+L1:list, +L2:list, +L3:list, -L123:list) is det.
%
%  Append 3 lists together.
%
%  @param L1   The head list
%  @param L2   The middle list
%  @param L3   The tail list
%  @param L123 The result list

append3(L1, L2, L3, L123) :-

    append(L2, L3, L23),
    append(L1, L23, L123).

%! append4(+L1:list, +L2:list, +L3:list, +L4:list, -L1234:list) is det.
%
%  Append 4 lists together.
%
%  @param L1    The head list
%  @param L2    1st middle list
%  @param L3    2nd middle list
%  @param L4    The tail list
%  @param L1234 The result list

append4(L1, L2, L3, L4, L1234) :-

    append(L3, L4, L34),
    append(L1, L2, L12),
    append(L12, L34, L1234).

%! append5(+L1:list, +L2:list, +L3:list, +L4:list, L5:list, -L12345:list) is det.
%
%  Append 5 lists together.
%
%  @param L1     The head list
%  @param L2     1st middle list
%  @param L3     2nd middle list
%  @param L4     3nd middle list
%  @param L5     The tail list
%  @param L12345 The final list

append5(L1, L2, L3, L4, L5, L12345) :-

    append(L3, L4, L34),
    append(L1, L2, L12),
    append(L12, L34, L1234),
    append(L1234, L5, L12345).

%-------------------------------------------------------------------------------------

%! convlist_first(:Goal:pred, +List:list, -Element:data) is semidet.
%
%  Unify Element with the first element in List that satisfies Goal.
%  Similar to convlist/3, but only the first element for which
%  `call(Goal, Element, _)` succeeds is returned. Fail if no element succeeds.
%
%  @param Goal    Predicate to be invoked with the elements of List
%  @param List    List of parameters for Goal invocation
%  @param Element The first element in List with which a Goal invocation succeeds

convlist_first(_Goal, [], _Element) :-
    !, fail.

convlist_first(Goal, [Head|List], Element) :-

    ( call(Goal, Head, Element) ;
      convlist_first(Goal, List, Element) ).

%-------------------------------------------------------------------------------------

%! lists_find(+Lists:list, +Element:data, -Pos1:int) is semidet.
%
% Unify Pos1 with the 1-based position of the first list in Lists containing Element.
%
%  @param Lists   List of lists under inspection
%  @param Element Element being sought
%  @param Pos1    1-based position of the list containing the element

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

%-------------------------------------------------------------------------------------

%! lists_start_with(+Lists:list, +Sublist:list, -List:list) is semidet.
%
%  Unify List with the first list in Lists starting with Sublist.
%
%  @param Lists   List of lists under inspection
%  @param Sublist Sublist being sought
%  @param List    The list starting with the given elements

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

%-------------------------------------------------------------------------------------

%! lists_consolidate(+ListsRefs:list, +ListElems:list, -ListsResult:list) is det.
%
%  Consolidate ListsResults with a list of lists based on the contents of
%  ListsRefs and ListElems. If a list within ListsRef contains the first element
%  of ListElems, the elements of that list ListElem are appended to its head,
%  otherwise that list  is appended to the head of ListsRef. Examples:
%
%  *|1.|*
%  ~~~
%    lists_consolidate([[1,d,b],[4,c,f]], [c,j,k], ListsResult)
%  yields
%    ListsResult = [[1,d,b],[c,j,k,4,c,f]]
%  ~~~
%  *|2.|*
%  ~~~
%    lists_consolidate([[1,d,b],[4,c,f]], [d,j,k], ListsResult)
%  yields
%    ListsResult = [[d,j,k,1,d,b],[4,c,f]]
%  ~~~
%  *|3.|*
%  ~~~
%    lists_consolidate([[1,d,b],[4,c,f]], [j,k,l], ListsResult)
%  yields
%    ListsResult = [[j,k,l],[1,d,b],[4,c,f]]
%  ~~~
%
%  @param ListsRefs   The reference list of lists
%  @param  ListElems  The list to be consolidated into the list of lists
%  @param ListsResult The resulting list of lists

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

%-------------------------------------------------------------------------------------

%! list_minus_list(+ListRef:list, +ListElems:list, -ListResult:list) is det.
%
%  Remove from ListRef all occurrences of elements in ListElems.
%
%  @param ListRef    List to be inspected for elements removal
%  @param ListElems  List of elements to be removed
%  @param ListResult The resulting purged list

% (done)
list_minus_list(ListResult, [], ListResult).

% (iterate)
list_minus_list(ListRef, [E|ListElems], ListResult) :-

    delete(ListRef, E, ListPurged),
    list_minus_list(ListPurged, ListElems, ListResult).

%-------------------------------------------------------------------------------------

%! list_replace0(+Pos0:int, +List:list, +Element:data, -ListResult:list) is det.
%
%  Replace the element at 0-based Pos0 in List with Element.
%
%  @param Pos0       the 0-based target position
%  @param List       the source list
%  @param Element    The replacing element
%  @param ListResult The resulting list with Element at position Pos0

list_replace0(Pos0, List, Element, ListResult) :-

    nth0(Pos0, List, _, ListTemp),
    nth0(Pos0, ListResult, Element, ListTemp).

%! list_replace1(+Pos0:int, +List:list, +Element:data, -ListResult:list) is det.
%
%  Replace the element at 1-based Pos1 in List with Element.
%
%  @param Pos1       the 1-based target position
%  @param List       the source list
%  @param Element    The replacing element
%  @param ListResult The resulting list with Element at position Pos1

list_replace1(Pos1, List, Element, ListResult) :-

    nth1(Pos1, List, _, ListTemp),
    nth1(Pos1, ListResult, Element, ListTemp).

%-------------------------------------------------------------------------------------

%! list_values(+Count:int, +Start:number, +Offset:number, -Values:list) is det.
%
%  Unify Values with a list with Count calculated values as elements
%
%  @param Count  Number of elements
%  @param Start  The start value
%  @param Offset The offset
%  @param Values List with the values produced

list_values(Count, Start, Offset, Values) :-
    list_values_(Count, Start, Offset, [], Values).

% (done)
list_values_(0, _Value, _Offset, ValuesProgress, ValuesFinal) :-
    reverse(ValuesProgress, ValuesFinal).

% (iterate)
list_values_(Count, Value, Offset, ValuesProgress, ValuesFinal) :-

    CountNext is Count - 1,
    ValueNext is Value + Offset,
    list_values_(CountNext, ValueNext, Offset,
                 [Value|ValuesProgress], ValuesFinal).

%-------------------------------------------------------------------------------------

%! lists_flatten(+Lists:list, -List:list) is det.
%
%  Recursively flatten a list of lists.
%
%  The original order of the elements is kept, and repeating values are not removed.
%  Note that append/2 flattens only the first level within the list of lists.
%
%  @param Lists List of lists to flatten
%  @param List  Flattened list

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

%-------------------------------------------------------------------------------------

%! list_fill(+Count:int, +Item:data, -List:list) is det.
%
%  Unify List with a list containing Count instances of Item.
%  (Note that Count = 0 is acceptable).
%
%  @param Count The number of instances
%  @param Item  The item to append to list
%  @param List  The resulting list

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

%-------------------------------------------------------------------------------------

%! list_common(+List1:list, List2:list, -ListCommon:list) is det.
%
%  Unify ListCommon with the elements common to List1 and List2,
%  in the order they are found in List1.
%
%  @param List1      The first list
%  @param List2      The second list
%  @param ListCommon The resulting list

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

%-------------------------------------------------------------------------------------

%! list_compacts(+List:list, -ListsCompact:list) is det.
%
%  Unify a list of integers with a list of lists, each one of them containing
%  a range of values found in the original list.
%
%  The original list must have unique elements and be ascendingly sorted,
%  otherwise the compactification will yield unpredictable results. Examples:
%
%  *|1.|*
%  ~~~
%    list_compacts([1,3,4,6,7,8,10], ListsCompact)
%  yields
%    ListsCompact = [[1,1],[3,4],[6,8],[10,10]]
%  ~~~
%  *|2.|*
%  ~~~
%    list_compacts(List, [[1,1],[3,4],[6,8],[10,10]])
%  yields
%    List = [1,3,4,6,7,8,10]
%  ~~~
%
%  @param List         The input list
%  @param ListsCompact The resulting compactified list

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

%-------------------------------------------------------------------------------------

%! list_split(+List:list, +Sep:atom, -Lists:list) is det.
%
%  Split a list into various lists, based on a given separator.
%  If no separator exists, a list of lists with one element is returned.
%
%  @param List  The list under inspection
%  @param Sep   The separator
%  @param Lists The resulting list of lists

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

%-------------------------------------------------------------------------------------

%! list_count_same(+List:list, +Pos:int, -Count:int) is det.
%
%  Count occurrences of the same element contiguously from a given
%  0-based position within a list.
%
%  @param List  The list under inspection
%  @param Pos   The 0-based position of the element to count
%  @param Count Tthe number of cosecutive occurrences of the element

list_count_same(List, Pos, Count) :-

    length(List, Len),
    !,
    % fail points
    Pos>= 0,
    Pos < Len,
    nth0(Pos,List, Elem),
    list_count_same_(Pos, Len, Elem, List, 0, Count).

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

%-------------------------------------------------------------------------------------

%! list_same(+List:list) is semidet.
%
%  Assert whether all elements in List are the same. Note that empty lists will fail.
%
%  @param List List being inspected

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

%! list_same(+List:list, ?Before:int, ?Length:int, ?After:int) is semidet.
%
%  Assert whether all elements in specified sublist of List are the same.
%  Note that empty lists will fail.

%  @param List   List being inspected
%  @param Before Number of items before the sublist
%  @param Length Length of sublist to inspect
%  @param After  Number of items after the sublist

list_same(List, Before, Length, After) :-

    sublist(List, Sublist, Before, Length, After),
    !,
    % fail point
    list_same(Sublist).

%! list_same0(+List:list, +Pos0:int, +Count:int) is semidet.
%
%  Assert whether all elements in specified sublist of List are the same.
%  Note that empty lists will fail.
%
%  @param List  List being inspected
%  @param Pos0  0-based starting position
%  @param Count Number of items to compare

list_same0(List, Pos0, Count) :-

    sublist(List, Sublist, Pos0, Count, _),
    !,
    % fail point
    list_same(Sublist).


%! list_same1(+List:list, +Pos1:int, +Count:int) is semidet.
%
%  Assert whether all elements in specified sublist of List are the same.
%  Note that empty lists will fail.
%
%  @param List  List being inspected
%  @param Pos1  1-based starting position
%  @param Count Number of items to compare

list_same1(List, Pos1, Count) :-

    Pos is Pos1 - 1,
    sublist(List, Sublist, Pos, Count, _),
    !,
    % fail point
    list_same(Sublist).

%-------------------------------------------------------------------------------------

%! lists_common(+List1:list, +List2:list) is semidet.
%
%  Assert whether the given lists have at least one element in common.
%
%  @param List1     first list to compare
%  @param List2     second list ot compare

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
