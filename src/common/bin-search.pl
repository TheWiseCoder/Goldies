/*******************************************************************************
* FILENAME / MODULE : bin-search.pl / bin_search
*
* DESCRIPTION :
*       This is an implementation of the classic binary search on sorted lists
*       (see https://en.wikipedia.org/wiki/Binary_search_algorithm). To
*       summmarize the algorithm:
*
*       1- take a list L of n elements with values A0, A1,...,An ordered such
*          that A0 <= A1 <= ... <= An, and a target value T;
*       2- set the indices l to 0 and r to n-1;
*       3- if l > r, terminate the search as unsuccessful;
*       4- set the index m to the position of the middle element, so that
*          it is the floor (greatest integer less than or equal to) of
*          (l + r) / 2;
*       5- if Am < T, set l to m + 1 an go to step 3;
*       6- if Am > T, set r to m - 1 an go to step 3;
*       7- now, Am = T, the search is done, and m is the desired index.
*
* PUBLIC PREDICATES :
*       bin_search(+Target, +Values, +Index)
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
* 2020-03-20  GT Nunes          Module creation
* 2020-04-10  GT Nunes          Added this header
* 2020-06-04  GT Nunes          Guarded fail points with cuts
*
*******************************************************************************/

:- module(bin_search,
    [
        bin_search/3
    ]).

:- use_module(library(lists),
    [
        nth0/3
    ]).

%-------------------------------------------------------------------------------
% Binary search on sorted lists. Unify Index with the 0-based position
% of Target within Values, fail otherwise.

% bin_search(+Target, +Values, -Index)
% Target    the value to search for
% Values    the sorted list to be searched
% Index     the 0-based index of Target within Values, on success
bin_search(Target, Values, Index) :-

    length(Values, Len),
    Last is Len - 1,
    First = 0,

    !,
    % fail point
    Last >= First,
    Mid is div((First + Last), 2),
    nth0(Mid, Values, Value),

    !,
    % fail point
    bin_search_(Value, Target, First, Last, Mid, Values, Index).

% bin_search_(+Value, +Target, +PosFirst, +PosLast, +PosMid, +Values)
% Value     the current working value
% Target    the value to search for
% PosFirst  the first position in the partition
% PosLast   the last position in the partition
% PosMid    the middle position in the partition
% Values    the sorted list being searched
% Index     the 0-based index of Target, on success

% (success)
bin_search_(Value, Value, _PosFirst, _PosLast, PosMid, _Values, Index) :-
    Index = PosMid.

% (iterate)
bin_search_(Value, Target, PosFirst, PosLast, PosMid, Values, Index) :-

    (Value < Target ->
        First is PosMid + 1,
        Last is PosLast
    ;
        First is PosFirst,
        Last is PosMid - 1
    ),

    !,
    % fail point
    Last >= First,
    Mid is div(First + Last, 2),
    nth0(Mid, Values, ValueNext),
    !,
    % fail point
    bin_search_(ValueNext, Target, First, Last, Mid, Values, Index).
