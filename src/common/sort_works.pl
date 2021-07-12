:- module(sort_works,
    [
        mergesort/3,
        quicksort/3
    ]).

/** <module> Sorting with comparators

Implementations of two classical sorting algorithms, with the flexibility of taking
a `comparator` as input. This is a predicate able to perform a comparison between
any two elements of the unsorted input list as parameters, and return the atoms
'=', '<', or '>', to indicate whether the first parameter is positionally
identical to, comes before, or comes after, the second parameter, respectively.
The choice of these atoms allows the use of the `compare/3` predicate as the
comparator for any of these two sorts.

@author GT Nunes
@version 1.3.3
@copyright (c) TheWiseCoder 2020-2021
@license BSD-3-Clause License
*/

%-------------------------------------------------------------------------------------

:- meta_predicate merge(3, +, +, -),
                  mergesort(3, +, -),
                  quicksort(3, +, -),
                  quicksort_partition(3, +, +, +, +).

%! mergesort(:Comparator:pred, +Unsorted:list, -Sorted:list) is det.
%
%  An implementation of the classic `mergesort` sorting algorithm on generic lists.
%  Mergesort is an efficient, general-purpose, and comparison-based sorting algorithm.
%  This is a stable sort implementation, meaning that the relative order of equal
%  sort items is preserved. For a description of the mergesort algorithm, see
%  https://en.wikipedia.org/wiki/Merge_sort .<br/>
%  Sort the contents of Unsorted according to the given comparison predicate,
%  and unify the result with Sorted. The comparison predicate must accept two
%  parameters, `ValueX` and `ValueY`, which might be any two elements in List,
%  and must have the following behavior:
%  ~~~
%  <Comparator>(-Result:atom, +ValueX, +ValueY) is det
%  where Result is unified with
%    a) = ('equal' sign)        - ValueX is positionally identical to ValueY
%    b) < ('less than' sign)    - ValueX comes before ValueY
%    c) > ('greater than' sign) - ValueX comes after ValueY
%  ~~~
%  The criteria that will determine the results of the comparisons are entirely up
%  to `Comparator`, and as such it must be able to handle the values it will receive.
%  In most cases, the built-in predicate `compare/3` may be readily used as the
%  comparator for the sort. Nothing is done if List has less than 2 elements.
%
%  @param Comparator Predicate to perform comparisons between any two elements in List
%  @param Unsorted   The list to be sorted
%  @param Sorted     The resulting sorted list

mergesort(_Comparator, [], Sorted) :- Sorted = [], !.

mergesort(_Comparator, [Value|[]], Sorted) :- Sorted = [Value], !.

mergesort(Comparator, Unsorted, Sorted) :-
    
    split(Unsorted, Odds, Evens),
    mergesort(Comparator, Odds, SortedLeft),
    mergesort(Comparator, Evens, SortedRight),
    merge(Comparator, SortedLeft, SortedRight, Sorted).

%! merge(+Comparator:pred, +Sorted1:list, +Sorted2:list, -Merged:list) is det.
%
%  Unify Merged with the result of merging the sorted lists Sorted1 and Sorted2.
%  Sorted1 and Sorted2 are sorted as per Comparator.
%
%  @param Comparator Predicate to perform comparisons between any two list elements
%  @param Sorted1    The first sorted list
%  @param Sorted2    The second sorted list
%  @param Merged     The merged list

merge(_Comparator, [], Merged, Merged).

merge(_Comparator, Merged, [], Merged).

merge(Comparator, [Elem1|Sorted1], [Elem2|Sorted2], [Elem1|Merged] ) :-

    call(Comparator, Cmp, Elem1, Elem2),
    \+ Cmp = '>',
    merge(Comparator, Sorted1, [Elem2|Sorted2], Merged).

merge(Comparator, [Elem1|Sorted1], [Elem2|Sorted2], [Elem2|Merged] ) :-

    call(Comparator, Cmp, Elem1, Elem2),
    Cmp = '>',
    merge(Comparator, [Elem1|Sorted1], Sorted2, Merged).

%! split(+List:list, -Odds:list, -Evens:list) is det
%
%  Alternate the odd- and even-position elements of List into Odds and Evens,
%  respectively. Positions are 1-based.
%
%  @param List  The source list
%  @param Odds  The odd-position elements
%  @param Evens The even-position elements

split([], [], []).

split([List], [List], []).

split([First,Second|List], [First|Odds], [Second|Evens]) :-
    split(List, Odds, Evens).

%-------------------------------------------------------------------------------------

%! quicksort(:Comparator:pred, +Unsorted:list, -Sorted:list) is det.
%
%  An implementation of the classic `Quicksort` sorting algorithm on generic lists.
%  `Quicksort` works by selecting a 'pivot' element from the list to be sorted and
%  partitioning the other elements into two sublists, according to whether they are
%  less than or greater than the pivot.The sublists are then sorted recursively.
%  This is not a stable sort implementation, meaning that the relative order of
%  equal sort items is not preserved. For a description of the quicksort algorithm,
%  see https://en.wikipedia.org/wiki/Quicksort .<br/>
%  Sort the contents of Unsorted according to the given comparison predicate,
%  and unify the result with Sorted. The comparison predicate must accept two
%  parameters, `ValueX` and `ValueY`, which might be any two elements in List,
%  and must have the following behavior:
%  ~~~
%  <Comparator>(-Result:atom, +ValueX, +ValueY) is det
%  where Result is unified with
%    a) = ('equal' sign)        - ValueX is positionally identical to ValueY
%    b) < ('less than' sign)    - ValueX comes before ValueY
%    c) > ('greater than' sign) - ValueX comes after ValueY
%  ~~~
%  The criteria that will determine the results of the comparisons are entirely up
%  to `Comparator`, and as such it must be able to handle the values it will receive.
%  In most cases, the built-in predicate `compare/3` may be readily used as the
%  comparator for the sort. Nothing is done if List has less than 2 elements.
%
%  @param Comparator Predicate to perform comparisons between any two elements in List
%  @param Unsorted   The list to be sorted
%  @param Sorted     The resulting sorted list

quicksort(_Comparator, [], Sorted) :- Sorted = [], !.

quicksort(_Comparator, [Value|[]], Sorted) :- Sorted = [Value], !.

quicksort(Comparator, [Value|Unsorted], Sorted) :-

    quicksort_partition(Comparator, Unsorted, Value, ListLeft, ListRight),
    quicksort(Comparator, ListLeft, SortedLeft),
    quicksort(Comparator, ListRight, SortedRight),
    quicksort_append(SortedLeft, [Value|SortedRight], Sorted).

quicksort_partition(_Comparator, [], _ValueY, [], []).

quicksort_partition(Comparator, [ValueX|Unsorted],
                    ValueY, [ValueX|ListLeft], ListRight) :-

    call(Comparator, Cmp, ValueX, ValueY),
    % fail point
    \+ Cmp = '>',
    quicksort_partition(Comparator, Unsorted, ValueY, ListLeft, ListRight).

quicksort_partition(Comparator, [ValueX|Unsorted],
                    ValueY, ListLeft, [ValueX|ListRight]) :-

    call(Comparator, Cmp, ValueX, ValueY),
    % fail point
    Cmp = '>',
    quicksort_partition(Comparator, Unsorted, ValueY, ListLeft, ListRight).

quicksort_append([], List, List).

quicksort_append([Value|ListX], ListY, [Value|ListZ]) :-
    quicksort_append(ListX, ListY, ListZ).
