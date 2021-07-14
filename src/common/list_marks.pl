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
        list_pad_head/4,
        list_pad_tail/4,
        list_pairs/3,
        list_prune_on_length/4,
        list_replace/4,
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
        lists_start_with/3,
        sublist_between/5
    ]).

/** <module>  Miscellaneous small list-related utilities

@author GT Nunes
@version 1.3
@copyright (c) TheWiseCoder 2020-2021
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
        sublist/4,
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
        sublist/4,
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
      convlist_first(Goal, List, Element) ),
    !.

%-------------------------------------------------------------------------------------

%! sublist_between(+List:list, +ListFrom:list, +ListTo:list, -Sublist:list, ListAdjusted:list) is semidet.
%
%  Unify Sublist with the contents of List found between ListFrom and ListTo,
%  exclusive. Subsequently, unify ListAdjusted with the remaining contents of List,
%  after Sublist, ListFrom and ListTo are extracted.
%  Fail if no such boundaries exist.
%
%  @param List         The input list
%  @param ListFrom     The begin boundary
%  @param ListTo       The end boundary
%  @param Sublist      The content between the boundaries
%  @param ListAdjusted The adjusted list (input list minus boundaries and content)

sublist_between(List, ListFrom, ListTo, Sublist, ListAdjusted) :-

    % the following are the target structures:
    %
    % <                List                   >
    % <List1><ListFrom><        List2         >
    %        ^         ^
    %        |         |
    %   FromBefore  FromAfter
    %
    % <        List2         >
    % <Sublist><ListTo><List3>
    %          ^       ^
    %          |       |
    %      ToBefore ToAfter
    %
    % <ListAdjusted>
    % <List1><List3>

    % fail point (locate ListFrom withing List)
    sublist(List, ListFrom, FromBefore),

    % extract the previous and remaining
    sublist(List, List1, 0, FromBefore),
    length(ListFrom, FromLength),
    FromAfter is FromBefore + FromLength,
    sublist(List, List2, FromAfter, _, 0),

    !,
    % fail point (locate ListTo within List2)
    sublist(List2, ListTo, ToBefore),

    % obtain Sublist
    length(ListTo, ToLength),
    sublist(List2, Sublist, 0, ToBefore),
    ToAfter is ToBefore + ToLength,
    sublist(List2, List3, ToAfter, _, 0),

    % obtain ListAdjusted
    append(List1, List3, ListAdjusted),
    !.

%-------------------------------------------------------------------------------------

%! list_minus_list(+ListRef:list, +ListElems:list, -ListResult:list) is det.
%
%  Remove from ListRef all occurrences of elements in ListElems.
%
%  @param ListRef    List to be inspected for elements removal
%  @param ListElems  List of elements to be removed
%  @param ListResult The resulting purged list

% (done)
list_minus_list(ListResult, [], ListResult) :- !.

% (iterate)
list_minus_list(ListRef, [E|ListElems], ListResult) :-

    delete(ListRef, E, ListPurged),
    list_minus_list(ListPurged, ListElems, ListResult).

%-------------------------------------------------------------------------------------

%! list_replace(+Old:data, +List:list, +New:data, -ListResult:list) is det.
%
%  Bind ListResult with the result of replacing all occurrences of Old in List
%  with New.
%
%  @param Old        the element to be replaced
%  @param List       the source list
%  @param New        The replacing element
%  @param ListResult The resulting list

list_replace(Old, List, New, ListResult) :-
    list_replace_(List, Old, New, [], ListResult).

% (done)
list_replace_([], _, _, ListWorking, ListFinal) :-

    reverse(ListWorking, ListFinal),
    !.

list_replace_([HeadOld|TailOld], Old, New, ListWorking, ListFinal) :-

    (Old = HeadOld ->
        HeadNew = New
    ;
        HeadNew = HeadOld
    ),

    % go for the next element
    list_replace_(TailOld, Old, New, [HeadNew|ListWorking], ListFinal).

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

%! list_replace1(+Pos1:int, +List:list, +Element:data, -ListResult:list) is det.
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
%  Unify Values with a list with Count calculated values as elements.
%  For Count <= 0, unify Values with an empty list. Note that this is useful
%  for lists of float values. For lists of integers, use numlist/3 instead.
%
%  @param Count  Number of elements
%  @param Start  The start value
%  @param Offset The offset
%  @param Values List with the values produced

list_values(Count, Start, Offset, Values) :-

    (Count =< 0 ->
        Values = []
    ;
        list_values_(Count, Start, Offset, [], Values)
    ).

% (done)
list_values_(0, _Value, _Offset, ValuesProgress, ValuesFinal) :-
    reverse(ValuesProgress, ValuesFinal), !.

% (iterate)
list_values_(Count, Value, Offset, ValuesProgress, ValuesFinal) :-

    CountNext is Count - 1,
    ValueNext is Value + Offset,
    list_values_(CountNext, ValueNext, Offset,
                 [Value|ValuesProgress], ValuesFinal).

%-------------------------------------------------------------------------------------

%! list_fill(+Count:int, +Item:data, -List:list) is semidet.
%
%  Unify List with a list containing Count instances of Item.
%  Unify List with [] if Count = 0 (Item is disregarded.
%
%  @param Count The number of instances
%  @param Item  The item to append to list
%  @param List  The resulting list

list_fill(Count, Item, List) :-

    % fail point
    Count >= 0,
    list_fill_(Count, Item, [], List).

% (done)
list_fill_(0, _Item, ListFinal, ListFinal) :- !.

% (iterate)
list_fill_(Count, Item, ListProgress, ListFinal) :-

    CountNext is Count - 1,
    list_fill_(CountNext, Item, [Item|ListProgress], ListFinal).

%-------------------------------------------------------------------------------------

%! list_pad_head(+ListIn:list, +Length:int, +Item:data, -ListOut) is semidet.
%
%  Unify ListOut with a list containing ListIn, head-padded with instances of Item
%  to length Length. if Length is equal to the length of ListIn, disregard Item and
%  unity ListOut with ListIn. Fail if Length is less than the length of ListIn.
%
%  @param ListIn  The input list
%  @param Length  The length of the output list
%  @param Item    The item to append to list
%  @param ListOut The output list

list_pad_head(ListIn, Length, Item, ListOut) :-

    length(ListIn, LengthIn),
    Count is Length - LengthIn,

    % fail point
    list_fill(Count, Item, Filler),
    append(Filler, ListIn, ListOut).

%! list_pad_tail(+ListIn:list, +Length:int, +Item:data, -ListOut) is semidet.
%
%  Unify ListOut with a list containing ListIn, tail-padded with instances of Item
%  to length Length. if Length is equal to the length of ListIn, disregard Item and
%  unity ListOut with ListIn. Fail if Length is less than the length of ListIn.
%
%  @param ListIn  The input list
%  @param Length  The length of the output list
%  @param Item    The item to append to list
%  @param ListOut The output list

list_pad_tail(ListIn, Length, Item, ListOut) :-

    length(ListIn, LengthIn),
    Count is Length - LengthIn,

    % fail point
    list_fill(Count, Item, Filler),
    append(ListIn, Filler, ListOut).

%-------------------------------------------------------------------------------------

%! list_prune_on_length(+ListIn:list, +MinLength:int, +MaxLength:int, -ListOut:list) is det.
%
%  Unify ListOut with a list containing the elements in the list of lists ListIn
%  with length between MinLength and MaxLength, inclusive. If MaxLength is 0,
%  the upper limit on the length is disregarded. Unify ListOut with [] if no
%  element in ListIn has length in the desired range.
%
%  @param ListIn    The input list of lists
%  @param MinLength Min acceptable length
%  @param MaxLength Max acceptable length
%  @param ListOut   The output list

list_prune_on_length(ListIn, MinLength, MaxLength, ListOut) :-
    list_prune_on_length_(ListIn, MinLength, MaxLength, [], ListOut).

% (done)
list_prune_on_length_([], _MinLength, _MaxLength, ListProgress, ListFinal) :-
    reverse(ListProgress, ListFinal), !.

% (iterate)
list_prune_on_length_([Elem|List], MinLength, MaxLength, ListProgress, ListFinal) :-

    length(Elem, Len),
    ((Len >= MinLength , (MaxLength = 0 ; Len =< MaxLength)) ->
        ListRevised = [Elem|ListProgress]
    ;
        ListRevised = ListProgress
    ),
    !,

    % go for the next element
    list_prune_on_length_(List, MinLength, MaxLength, ListRevised, ListFinal).

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
    reverse(CommonsProgress, CommonsFinal), !.

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
%  ~~~
%  1. list_compacts([1,3,4,6,7,8,10], ListsCompact)
%     yields
%     ListsCompact = [[1,1],[3,4],[6,8],[10,10]]
%
%  2. list_compacts(List, [[1,1],[3,4],[6,8],[10,10]])
%     yields
%     List = [1,3,4,6,7,8,10]
%  ~~~
%
%  Note that, due to the standard predicate loop scheme used, invoking this predicate
%  with lists with a very large number of elements might cause a stack trace overflow.
%  This holds true for SWI-Prolog, and very likely, to Sicstus Prolog as well.
%  In fact, on an Intel-based 64-bit desktop loaded with 16GB of RAM, SWI-Prolog
%  will break on predicate loops with over 200000 recursive invocations.
%  With a startup parameter such as "--stack-limit=32g", SWI-Prolog will break
%  execution at or near the 5-million recursive invocations mark. Thus, care
%  should be exercised when invoking this predicate on large lists.
%
%  @param List         The input list
%  @param ListsCompact The resulting compactified list

% (done)
list_compacts([], []) :- !.

list_compacts(List, ListsCompact) :-

    % fail point
    var(List),

    compacts_list_(ListsCompact, [], List),
    !.

list_compacts(List, ListsCompact) :-

    % fail point
    var(ListsCompact),

    [Prev|Tail] = List,
    list_compacts_(Tail, [[Prev,Prev]], ListsCompact),
    !.

% (done)
list_compacts_([], [[First,Last]|ListsProgress], ListsFinal) :-
    reverse([[Last,First]|ListsProgress], ListsFinal), !.

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
compacts_list_([], ListFinal, ListFinal) :- !.

% (iterate)
compacts_list_([[First,Last]|ListsCompact], ListProgress, ListFinal) :-

    numlist(First, Last, List),
    append(ListProgress, List, ListRevised),
    compacts_list_(ListsCompact, ListRevised, ListFinal).

%-------------------------------------------------------------------------------------

%! list_pairs(+Pairs:list, -Elements1st:list, -Elements2nd:lists) is det.
%! list_pairs(-Pairs:list, +Elements1st:list, +Elements2nd:lists) is det.
%
%  Unify Pairs with a list of lists comprising elements from the lists Elements1st
%  and Elements2nd, or conversely. Fail if Pairs is not a list of lists of pairs.
%  Pairs will have the cardinality of the list with the smaller number of elements
%  between Elements1st and Elements2nd.
%  ~~~
%  1) Pairs grounded:
%     Pairs:       [[a,1],[[23,4,5],2],[b,[4,5]],...]
%     yields
%     Elements1st: [a,[23,4,5],b,...]
%     Elements2nd: [[1,2,[4,5],...]
%
%  2) Elements1st and Elements2nd grounded:
%     Elements1st: [78,[5,8,a],x,...]
%     Elements2nd: [[91],y,[c,d],...]
%     yield
%     Pairs:       [[78,[91]],[[5,8,a],y],[x,[c,d]],...]
%  ~~~
%
%  @param Pairs       List of lists containing pairs of elements
%  @param Elements1st List of first elements in pairs
%  @param Elements2nd List of second elements in pairs

list_pairs(Pairs, Elements1st, Elements2nd) :-

    % fail point
    var(Pairs),

    lists_to_pairs(Elements1st, Elements2nd, [], Pairs),
    !.

list_pairs(Pairs, Elements1st, Elements2nd) :-

    % fail point
    nonvar(Pairs),

    pairs_to_lists(Pairs, [], Elements1st, [], Elements2nd),
    !.

%(done)
lists_to_pairs([], _Elements2nd, PairsFinal, PairsFinal) :- !.

%(done)
lists_to_pairs(_Elements1st, [], PairsFinal, PairsFinal) :- !.

% (iterate)
lists_to_pairs([Element1st|Elements1st],
               [Element2nd|Elements2nd], PairsProgress, PairsFinal) :-
    lists_to_pairs(Elements1st, Elements2nd,
                   [[Element1st,Element2nd]|PairsProgress], PairsFinal).

% (done)
pairs_to_lists([], Final1st, Final1st, Final2nd, Final2nd) :- !.

% (iterate)
pairs_to_lists([[Element1st,Element2nd]|Pairs],
              Progress1st, Final1st, Progress2nd, Final2nd) :-
    pairs_to_lists(Pairs, [Element1st|Progress1st], Final1st,
                   [Element2nd|Progress2nd], Final2nd).

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
        sublist(List, ListPrev, 0, Pos),
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

    % fail points
    Pos>= 0,
    Pos < Len,
    nth0(Pos,List, Elem),
    list_count_same_(Pos, Len, Elem, List, 0, Count).

% (done)
list_count_same_(Len, Len, _Elem, _List, CountFinal, CountFinal) :- !.

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
 
    % fail point
    (Len = 1 ; list_same_(List)),
    !.

% (done)
list_same_([_|[]]) :- !.

% (iterate)
list_same_(List) :-

    [H,H|ListNew] = List,
 
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

    sublist(List, Sublist, Pos0, Count),
 
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
    sublist(List, Sublist, Pos, Count),
 
    % fail point
    list_same(Sublist).

%-------------------------------------------------------------------------------------

%! lists_common(+List1:list, +List2:list) is semidet.
%
%  Assert whether the given lists have at least one element in common.
%
%  @param List1 First list to compare
%  @param List2 Second list ot compare

lists_common([], _List2) :- !, fail.
lists_common(_List1, []) :- !, fail.

lists_common([Elem|List1], List2) :-

    % is element from ListA also in ListB
    ( memberchk(Elem, List2)
    ; lists_common(List1, List2)
    ),
    !.

%-------------------------------------------------------------------------------------

%! lists_find(+Lists:list, +Element:data, -Pos1:int) is semidet.
%
%  Unify Pos1 with the 1-based position of the first list in Lists containing Element.
%
%  @param Lists   List of lists under inspection
%  @param Element Element being sought
%  @param Pos1    1-based position of the list containing the element

lists_find(Lists, Element, Pos1) :-

    length(Lists, Count),

    % fail point
    lists_find_(Count, Lists, Element, Pos1).

% (failure)
lists_find_(0, _Lists, _Element, _Pos1) :- !, fail.

% (iterate)
lists_find_(Count, Lists, Element, Pos1) :-

    nth1(Count, Lists, List),
    (memberchk(Element, List) ->
        Pos1 = Count
    ;
        CountNext is Count - 1,

        % fail point
        lists_find_(CountNext, Lists, Element, Pos1)
    ).

%-------------------------------------------------------------------------------------

%! lists_start_with(+Lists:list, +Sublist:list, -List:list) is semidet.
%
%  Unify List with the first list in Lists starting with Sublist.
%  Fail if so such list exists.
%
%  @param Lists   List of lists under inspection
%  @param Sublist Sublist being sought
%  @param List    The list starting with the given elements

% (failure)
lists_start_with([], _Sublist, _List) :- !, fail.

% (iterate)
lists_start_with([Head|Lists], Sublist, List) :-

    (sublist(Head, Sublist, 0) ->
        List = Head
    ;
        % fail point
        lists_start_with(Lists, Sublist, List)
    ).

%-------------------------------------------------------------------------------------

%! lists_consolidate(+ListsRefs:list, +ListElems:list, -ListsResult:list) is det.
%
%  Unify ListsResult with a list of lists based on the contents of ListsRefs
%  and ListElems. </br>
%  For the first list ListIn within ListsRef containing the first element of
%  ListElems, the elements of ListElems are added to the head of ListIn.
%  If no such list exists, ListElems is added to the tail of ListsRefs.
%  Unify ListsResult with ListsRefs, if ListElems is empty. Unify ListsResult
%  with a list containing ListElems, if ListsRefs is empty.
%  Examples:
%  ~~~
%  1. lists_consolidate([[1,d,b],[4,c,f]], [c,j,k], ListsResult)
%     yields
%     ListsResult = [[1,d,b],[c,j,k,4,c,f]]
%
%  2. lists_consolidate([[1,d,b],[4,c,f]], [d,j,k], ListsResult)
%     yields
%    ListsResult = [[d,j,k,1,d,b],[4,c,f]]
%
%  3.lists_consolidate([[1,d,b],[4,c,f]], [j,k,l], ListsResult)
%    yields
%    ListsResult = [[j,k,l],[1,d,b],[4,c,f]]
%  ~~~
%
%  @param ListsRefs   The reference list of lists
%  @param  ListElems  The list to be consolidated into the list of lists
%  @param ListsResult The resulting list of lists

lists_consolidate(ListsRefs, [], ListsResult) :-
    ListsResult = ListsRefs, !.

lists_consolidate(ListsRefs, ListElems, ListsResult) :-

    [Elem|_] = ListElems,

    % is there a list containg Elem ?
    (lists_find(ListsRefs, Elem, Pos) ->

        % yes, ListElems2 is a list containing the element Elem
        % (ListsRems has the other lists)
        nth1(Pos, ListsRefs, ListElems2, ListsRems)
    ;
        % no, use empty list ListElems2 instead
        Pos = 1,
        ListElems2 = [],
        ListsRems = ListsRefs
    ),

    % join the elements of ListElems and ListElems2 in ListElems3
    append(ListElems, ListElems2, ListElems3),

    % insert the new list at the appropriate place in the list of lists
    nth1(Pos, ListsResult, ListElems3, ListsRems).

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

lists_flatten([], []) :- !.

lists_flatten(Lists, List) :-
    lists_flatten_(Lists, [], List).

% (done)
lists_flatten_([], ListFinal, ListFinal) :- !.

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
