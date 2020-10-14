bELOW ARE short descriptions of the features available. Please, refer to the README in the corresponding folder.  

**BIN-SEARCH**  

An implementation of the classic binary search on sorted lists (see https://en.wikipedia.org/wiki/Binary_search_algorithm).  
To summmarize the algorithm:  

- 1- take a list L of n elements with values A0, A1,...,An ordered such that A0 <= A1 <= ... <= An, and a target value T;  
- 2- set the indices l to 0 and r to n-1;  
- 3- if l > r, terminate the search as unsuccessful;  
- 4- set the index m to the position of the middle element, so that it is the floor (greatest integer less than or equal to) of (l + r) / 2;  
- 5- if Am < T, set l to m + 1 an go to step 3;  
- 6- if Am > T, set r to m - 1 an go to step 3;  
- 7- now, Am = T, the search is done, and m is the desired index.  

Usage:  
`bin_search(+SortedList, +Value, -Index)`  
Unify *Index* with the 0-based position of *Value* within *SortedList*, fail otherwise; unpredictable if *SortedList* is not ascendingly sorted.  

**REPEAT-GOAL**  

Invoke a goal a given number of times, with the specified number of parameters, returning a list of results.

Usage:  
`repeat__goal(:Goal, +N, Params, -Results)`  
*Params* are up to 6 parameters <+Pi,...,+Pj>  
*Results* is a list with the invocation results in the proper order  

**BDB**  

A simple, minimalistic approach to implementing persistence for Prolog data, by means of the Berkeley DB utility package.  

**COUNTER**  

An implementation of a global, stack-independent, simple counter, intended for use with integer values.  

**CSV**  

An attempt to define a standard interface for '.csv' file operations, to be used in different Prolog environments.  

**MARKS**  

Collections of small utilities, dealing with various subjects. 

