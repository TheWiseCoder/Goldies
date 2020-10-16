These are predicates providing a portability layer on SICStus-style lists handling for the SWI-Prolog platform.  

- `subseq0(+List, +Subseq)` - *true* when *Subseq* is a subsequence of *List*  
- `subseq1(+List, +Subseq)` - *true* when *Subseq* is a proper subsequence of *List*  
- `keyclumps(+Pairs, -Clumps)` - 
  
These are implementations of SICStus' `sublist/3`, `sublist/4`, `sublist/5`: 
  
- `sublist(+Whole, ?Part, ?Before)`
- `sublist(+Whole, ?Part, ?Before, ?Length)` 
- `sublist(+Whole, ?Part, ?Before, ?Length, ?After)`  

These relationships hold:  
* Whole = Alpha || Part || Omega  
* length(Alpha, Before)  
* length(Part, Length)  
* length(Omega, After)  

