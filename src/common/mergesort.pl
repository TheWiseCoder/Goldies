:- module(mergesort,
    [
        split/3,
        mergesort/3
    ]).

/** <module> Binary search on sorted lists

An implementation of the classic `mergesort` sorting algorithm on generic lists.
Mergesort is an efficient, general-purpose, and comparison-based sorting algorithm.
For a description of the merge sort algorithm, see
https://en.wikipedia.org/wiki/Merge_sort .<br/>
This implementation takes as input a `comparator`. This is a predicate able to
perform a comparison between any two elements of the input list as parameters,
and return a negative number, zero, or a positive number, to indicate whether the
first parameter is smaller than, equal to, or greater than the second parameter,
respectively.<br/>
This implementation was adapted from Markus Triska's implementation, found at
https://www.metalevel.at/misc/sorting.pl . This is a stable sort implementation,
meaning that the relative order of equal sort items is not preserved.

@author GT Nunes
@version 1.0
@copyright (c) TheWiseCoder 2020-2021
@license BSD-3-Clause License
*/

%-------------------------------------------------------------------------------------

:- meta_predicate mergesort(+, 3, -),
                  merge(3, +, +, -).

%! mergesort(+List:list, :Comparator:pred, -SortedList:list) is det.
%
%  Sort the contents of List according to the given comparison predicate,
% and unify the result with SortedList. <br/>
%  The comparison predicate must accept two parameters, `ValueX` and `ValueY`,
%  which might be any two elements in List, and have the following behavior:
%  ~~~
%  <Comparator>(-Result:atom, +ValueX, +ValueY) is det
%  where Result is unified with
%    a) = (equal sign)        - ValueX is equal to ValueY
%    b) < (less than sign)    - ValueX is less than ValueY
%    c) > (greater than sign) - ValueX is greater than ValueY
%  ~~~
%  The criteria that will determine the results of the comparisons are entirely up
%  to Comparator, and as such it must be able to handle the values it will receive.
%  In most cases, the built-in predicate `compare/3` may be readily used as the
%  comparator for the sort. Nothing is done if List has less than 2 elements.
%
%  @param List       The list to be sorted
%  @param Comparator Predicate to perform comparisons between any two elements in List
%  @param SortedList The resulting sorted list

mergesort([], _Comparator, SortedList) :- SortedList = [], !.

mergesort([Value|[]], _Comparator, SortedList) :- SortedList = [Value], !.

mergesort(Unsorted, Comparator, Sorted) :-
    
    split(Unsorted, Odds, Evens),
    mergesort(Odds, Comparator, SortedLeft),
    mergesort(Evens, Comparator, SortedRight),
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

%! split(+List:list, -ListOdd:list, -ListEven:list) is det
%
%  Alternate the odd- and even-position elements of List into ListOdd and ListEven,
%  respectively. Positions are 1-based.
%
%  @param List  The source list
%  @param Odds  The odd-position elements
%  @param Evens The even-position elements

split([], [], []).

split([List], [List], []).

split([First,Second|List], [First|Odds], [Second|Evens]) :-
    split(List, Odds, Evens).
