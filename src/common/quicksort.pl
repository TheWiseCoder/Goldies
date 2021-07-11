:- module(quicksort,
    [
        quicksort/3
    ]).

/** <module> Binary search on sorted lists

An implementation of the classic `Quicksort` sorting algorithm on generic lists.
`Quicksort` works by selecting a 'pivot' element from the list to be sorted and
partitioning the other elements into two sublists, according to whether they are
less than or greater than the pivot.The sublists are then sorted recursively.
For a description of the quicksort algorithm, see
https://en.wikipedia.org/wiki/Quicksort .<br/>
This implementation takes as input a `comparator`. This is a predicate able to
perform a comparison between any two elements of the input list as parameters,
and return a negative number, zero, or a positive number, to indicate whether the
first parameter is smaller than, equal to, or greater than the second parameter,
respectively.<br/>
Finally, this is not a stable sort implementation, meaning that the relative
order of equal sort items is not preserved.

@author GT Nunes
@version 1.3
@copyright (c) TheWiseCoder 2020-2021
@license BSD-3-Clause License
*/

%-------------------------------------------------------------------------------------

:- meta_predicate quicksort(+, 3, -),
                  quicksort_partition(3, +, +, +, +).

%! quicksort(+Unsorted:list, :Comparator:pred, -Sorted:list) is det.
%
%  Sort the contents of Unsorted according to the given comparison predicate,
% and unify the result with Sorted. <br/>
%  The comparison predicate must accept two parameters, `ValueX` and `ValueY`,
%  which might be any two elements in List, and have the following behavior:
%  ~~~
%  <Comparator>(-Result:atom, +ValueX, +ValueY) is det
%  where Result is unified with
%    a) = ('equal' sign)        - ValueX is equal to ValueY
%    b) < ('less than' sign)    - ValueX is less than ValueY
%    c) > ('greater than' sign) - ValueX is greater than ValueY
%  ~~~
%  The criteria that will determine the results of the comparisons are entirely up
%  to `Comparator`, and as such it must be able to handle the values it will receive.
%  In most cases, the built-in predicate `compare/3` may be readily used as the
%  comparator for the sort. Nothing is done if List has less than 2 elements.
%
%  @param Unsorted   The list to be sorted
%  @param Comparator Predicate to perform comparisons between any two elements in List
%  @param Sorted     The resulting sorted list

quicksort([], _Comparator, Sorted) :- Sorted = [], !.

quicksort([E|[]], _Comparator, Sorted) :- Sorted = [E], !.
    
quicksort([Value|Unsorted], Comparator, Sorted) :-

    quicksort_partition(Comparator, Unsorted, Value, ListLeft, ListRight),
    quicksort(ListLeft, Comparator, SortedLeft),
    quicksort(ListRight, Comparator, SortedRight),
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
