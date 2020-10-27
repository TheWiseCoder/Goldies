:- module(repeat_goal,
    [
        repeat_goal/3, % repeat_goal(:Goal, +N, -Result)
        repeat_goal/4, % repeat_goal(:Goal, +N, +P, -Result)
        repeat_goal/5, % repeat_goal(:Goal, +N, +P1, +P2, -Result)
        repeat_goal/6, % repeat_goal(:Goal, +N, +P1, +P2, +P3, -Result)
        repeat_goal/7, % repeat_goal(:Goal, +N, +P1, +P2, +P3, +P4, -Result)
        repeat_goal/8, % repeat_goal(:Goal, +N, +P1, +P2, +P3, +P4, +P5, -Result)
        repeat_goal/9  % repeat_goal(:Goal, +N, +P1, +P2, +P3, +P4, +P5, +P6, -Result)
    ]).

/** <module>  Repeat invocation of goal

Repeat invocation of goal a given number of times, with varying number of parameters.

@author GT Nunes
@version 1.0
@copyright (c) 2020 GT Nunes
@license BSD-3-Clause License
*/

%-------------------------------------------------------------------------------------

:- meta_predicate
    repeat_goal(1, +, -),
    repeat_goal(2, +, +, -),
    repeat_goal(3, +, +, +, -),
    repeat_goal(4, +, +, +, +, -),
    repeat_goal(5, +, +, +, +, +, -),
    repeat_goal(6, +, +, +, +, +, +, -),
    repeat_goal(7, +, +, +, +, +, +, +, -).

:- use_module(library(lists),
    [
        reverse/2
    ]).

%-------------------------------------------------------------------------------------

%! repeat_goal(:Goal:pred, +N:int, -Result:list) is det.
%
%  Unify Result with a list of results in the proper order, from invoking
%  Goal N times.
%
%  @param Goal    The predicate to invoke
%  @param N       Number of invocations
%  @param Result List of results from the invocations

repeat_goal(Goal, N, Result) :-

    % fail point
    N >= 0,
    repeat_goal_(Goal, N, [], Result).

% (done)
repeat_goal_(_Goal, 0, ResultProgress, ResultFinal) :-
    reverse(ResultProgress, ResultFinal).

% (iterate)
repeat_goal_(Goal, N, ResultProgress, ResultFinal) :-

    call(Goal, Result),
    Next is N - 1,
    repeat_goal_(Goal, Next, [Result|ResultProgress], ResultFinal).

%-------------------------------------------------------------------------------------

%! repeat_goal(:Goal:pred, +P:data, +N:int, -Result:list) is det.
%
%  Unify Result with a list of results in the proper order, from invoking
%  Goal N times.
%
%  @param Goal    The predicate to invoke
%  @param N       Number of invocations
%  @param P       Parameter for the invocation
%  @param Result List of results from the invocations

repeat_goal(Goal, N, P, Result) :-

    % fail point
    N >= 0,
    repeat_goal_(Goal, N, P, [], Result).

% (done)
repeat_goal_(_Goal, 0, _P, ResultProgress, ResultFinal) :-
    reverse(ResultProgress, ResultFinal).

% (iterate)
repeat_goal_(Goal, N, P, ResultProgress, ResultFinal) :-

    call(Goal, P, Result),
    Next is N - 1,
    repeat_goal_(Goal, Next, P, [Result|ResultProgress], ResultFinal).

%-------------------------------------------------------------------------------------

%! repeat_goal(:Goal:pred, +P1:data, +P2:data, +N:int, -Result:list) is det.
%
%  Unify Result with a list of results in the proper order, from invoking
%  Goal N times.
%
%  @param Goal    The predicate to invoke
%  @param N       Number of invocations
%  @param P1      1st parameter for the invocation
%  @param P2      2nd parameter for the invocation
%  @param Result List of results from the invocations

repeat_goal(Goal, N, P1, P2, Result) :-

    % fail point
    N >= 0,
    repeat_goal_(Goal, P1, P2, N, [], Result).

% (done)
repeat_goal_(_Goal, 0, _P1, _P2, ResultProgress, ResultFinal) :-
    reverse(ResultProgress, ResultFinal).

% (iterate)
repeat_goal_(Goal, N, P1, P2, ResultProgress, ResultFinal) :-

    call(Goal, P1, P2, Result),
    Next is N - 1,
    repeat_goal_(Goal, Next, P1, P2, [Result|ResultProgress], ResultFinal).

%-------------------------------------------------------------------------------------

%! repeat_goal(:Goal:pred, +P1:data, +P2:data, +P3:data, +N:int, -Result:list) is det.
%
%  Unify Result with a list of results in the proper order, from invoking
%  Goal N times.
%
%  @param Goal    The predicate to invoke
%  @param N       Number of invocations
%  @param P1      1st parameter for the invocation
%  @param P2      2nd parameter for the invocation
%  @param P3      3rd parameter for the invocation
%  @param Result List of results from the invocations

repeat_goal(Goal, N, P1, P2, P3, Result) :-

    % fail point
    N >= 0,
    repeat_goal_(Goal, N, P1, P2, P3, [], Result).

% (done)
repeat_goal_(_Goal, 0, _P1, _P2, _P3, ResultProgress, ResultFinal) :-
    reverse(ResultProgress, ResultFinal).

% (iterate)
repeat_goal_(Goal, N, P1, P2, P3, ResultProgress, ResultFinal) :-

    call(Goal, P1, P2, P3, Result),
    Next is N - 1,
    repeat_goal_(Goal, Next, P1, P2, P3,
                 [Result|ResultProgress], ResultFinal).

%-------------------------------------------------------------------------------------

%! repeat_goal(:Goal:pred, +P1:data, +P2:data, +P3:data, +P4:data, +N:int, -Result:list) is det.
%
%  Unify Result with a list of results in the proper order, from invoking
%  Goal N times.
%
%  @param Goal    The predicate to invoke
%  @param N       Number of invocations
%  @param P1      1st parameter for the invocation
%  @param P2      2nd parameter for the invocation
%  @param P3      3rd parameter for the invocation
%  @param P4      4th parameter for the invocation
%  @param Result List of results from the invocations

repeat_goal(Goal, N, P1, P2, P3, P4, Result) :-

    % fail point
    N >= 0,
    repeat_goal_(Goal, N, P1, P2, P3, P4, [], Result).

% (done)
repeat_goal_(_Goal, 0, _P1, _P2, _P3, _P4, ResultProgress, ResultFinal) :-
    reverse(ResultProgress, ResultFinal).

% (iterate)
repeat_goal_(Goal, N, P1, P2, P3, P4,
             ResultProgress, ResultFinal) :-

    call(Goal, P1, P2, P3, P4, Result),
    Next is N - 1,
    repeat_goal_(Goal, Next, P1, P2, P3, P4,
                 [Result|ResultProgress], ResultFinal).

%-------------------------------------------------------------------------------------

%! repeat_goal(:Goal:pred, +P1:data, +P2:data, +P3:data, +P4:data, P5:data, +N:int, -Result:list) is det.
%
%  Unify Result with a list of results in the proper order, from invoking
%  Goal N times.
%
%  @param Goal    The predicate to invoke
%  @param N       Number of invocations
%  @param P1      1st parameter for the invocation
%  @param P2      2nd parameter for the invocation
%  @param P3      3rd parameter for the invocation
%  @param P4      4th parameter for the invocation
%  @param P5      5th parameter for the invocation
%  @param Result List of results from the invocations

repeat_goal(Goal, N, P1, P2, P3, P4, P5, Result) :-

    % fail point
    N >= 0,
    repeat_goal_(Goal, N, P1, P2, P3, P4, P5, [], Result).

% (done)
repeat_goal_(_Goal, 0, _P1, _P2, _P3, _P4, _P5,
             ResultProgress, ResultFinal) :-
    reverse(ResultProgress, ResultFinal).

% (iterate)
repeat_goal_(Goal, N, P1, P2, P3, P4, P5,
             ResultProgress, ResultFinal) :-

    call(Goal, P1, P2, P3, P4, P5, Result),
    Next is N - 1,
    repeat_goal_(Goal, Next, P1, P2, P3, P4, P5,
                 [Result|ResultProgress], ResultFinal).

%-------------------------------------------------------------------------------------

%! repeat_goal(:Goal:pred, +P1:data, +P2:data, +P3:data, +P4:data, P5:data, +P6:data, +N:int, -Result:list) is det.
%
%  Unify Result with a list of results in the proper order, from invoking
%  Goal N times.
%
%  @param Goal    The predicate to invoke
%  @param N       Number of invocations
%  @param P1      1st parameter for the invocation
%  @param P2      2nd parameter for the invocation
%  @param P3      3rd parameter for the invocation
%  @param P4      4th parameter for the invocation
%  @param P5      5th parameter for the invocation
%  @param P6      6th parameter for the invocation
%  @param Result List of results from the invocations

repeat_goal(Goal, N, P1, P2, P3, P4, P5, P6, Result) :-

    % fail point
    N >= 0,
    repeat_goal_(Goal, N, P1, P2, P3, P4, P6, P5, [], Result).

% (done)
repeat_goal_(_Goal, 0, _P1, _P2, _P3, _P4, _P5, _P6,
             ResultProgress, ResultFinal) :-
    reverse(ResultProgress, ResultFinal).

% (iterate)
repeat_goal_(Goal, N, P1, P2, P3, P4, P5, P6, ResultProgress, ResultFinal) :-

    call(Goal, P1, P2, P3, P4, P5, P6, Result),
    Next is N - 1,
    repeat_goal_(Goal, Next, P1, P2, P3, P4, P5, P6,
                 [Result|ResultProgress], ResultFinal).
