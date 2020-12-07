:- module(bin_search,
    [
        bin_search/3
    ]).

/** <module> Binary search on sorted lists

This is an implementation of the classic binary search on sorted lists
(see https://en.wikipedia.org/wiki/Binary_search_algorithm ).
To summmarize the algorithm:
~~~
  1- take a list L of n elements with values A0, A1,...,An ordered such
     that A0 <= A1 <= ... <= An, and a target value T;
     
  2- set the indices l to 0 and r to n-1;
  
  3- if l > r, terminate the search as unsuccessful;
  
  4- set the index m to the position of the middle element, so that it is
     the floor (greatest integer less than or equal to) of (l + r) / 2;
     
  5- if Am < T, set l to m + 1 an go to step 3;
  
  6- if Am > T, set r to m - 1 an go to step 3;
  
  7- now, Am = T, the search is done, and m is the desired index.
~~~

@author GT Nunes
@version 1.0
@copyright (c) 2020 GT Nunes
@license BSD-3-Clause License
*/

%-------------------------------------------------------------------------------------

:- use_module(library(lists),
    [
        nth0/3
    ]).

%-------------------------------------------------------------------------------------

%! bin_search(+Target:data, +Values:list, -Index:int) is semidet.
%
%  Binary search on sorted lists.
%
%  Unify Index with the 0-based position of Target within Values, fail otherwise.
%
%  @param Target The value to search for
%  @param Values The sorted list to be searched
%  @param Index  The 0-based index of Target within Values, on success

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

%! bin_search_(+Value:data, +Target:data, +PosFirst:int, +PosLast:int, +PosMid:int, +Values:list) is semidet.
%
%  @param Value    The current working value
%  @param Target   The value to search for
%  @param PosFirst The first position in the partition
%  @param PosLast  The last position in the partition
%  @param PosMid   The middle position in the partition
%  @param Values   The sorted list being searched
%  @param Index    The 0-based index of Target, on success

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
