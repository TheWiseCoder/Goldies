/*******************************************************************************
* FILENAME / MODULE : repeat-goal.pl / repeat_goal
*
* DESCRIPTION :
*       Repeat invocation of goal a given number of times,
*       with varying number of parameters.
*
* PUBLIC PREDICATES :
*       repeat_goal(:Goal, N, -Results)
*       repeat_goal(:Goal, N, +P, -Results)
*       repeat_goal(:Goal, N, +P1, +P2, -Results)
*       repeat_goal(:Goal, N, +P1, +P2, +P3, -Results)
*       repeat_goal(:Goal, N, +P1, +P2, +P3, +P4, -Results)
*       repeat_goal(:Goal, N, +P1, +P2, +P3, +P4, P5, -Results)
*       repeat_goal(:Goal, N, +P1, +P2, +P3, +P4, P5, P6, -Results)
*
* NOTES :
*       None yet.
*
*       Copyright TheWiseProgrammer 2020.  All rights reserved.
*
* REVISION HISTORY :
*
* DATE        AUTHOR            REVISION
* ----------  ----------------  ------------------------------------------------
* 2020-10-06  GT Nunes          Module creation
*
*******************************************************************************/

:- module(repeat_goal,
    [
        repeat_goal/3,
        repeat_goal/4,
        repeat_goal/5,
        repeat_goal/6,
        repeat_goal/7,
        repeat_goal/8,
        repeat_goal/9
    ]).

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

% Unify Results with a list of results in the proper order, from invoking
% Goal N times, with up to 6 parameters. Unify with an empty list if N is 0. 
% repeat_goal(:Goal, +Params, +N, -Results)
% Goal      predicate to invoke
% Params    optionaL, up to 4 parameters (P1,..,P6)
% N         number of times do invoke
% Results   list of invocation results

%-------------------------------------------------------------------------------

% (start)
repeat_goal(Goal, N, Results) :-

    % fail point
    N >= 0,
    repeat_goal_(Goal, N, [], Results).

% (done)
repeat_goal_(_Goal, 0, ResultsProgress, ResultsFinal) :-
    reverse(ResultsProgress, ResultsFinal).

% (iterate)
repeat_goal_(Goal, N, ResultsProgress, ResultsFinal) :-

    call(Goal, Result),
    Next is N - 1,
    repeat_goal_(Goal, Next, [Result|ResultsProgress], ResultsFinal).

%-------------------------------------------------------------------------------

% (start)
repeat_goal(Goal, N, P, Results) :-

    % fail point
    N >= 0,
    repeat_goal_(Goal, N, P, [], Results).

% (done)
repeat_goal_(_Goal, 0, _P, ResultsProgress, ResultsFinal) :-
    reverse(ResultsProgress, ResultsFinal).

% (iterate)
repeat_goal_(Goal, N, P, ResultsProgress, ResultsFinal) :-

    call(Goal, P, Result),
    Next is N - 1,
    repeat_goal_(Goal, Next, P, [Result|ResultsProgress], ResultsFinal).

%-------------------------------------------------------------------------------

% (start)
repeat_goal(Goal, N, P1, P2, Results) :-

    % fail point
    N >= 0,
    repeat_goal_(Goal, P1, P2, N, [], Results).

% (done)
repeat_goal_(_Goal, 0, _P1, _P2, ResultsProgress, ResultsFinal) :-
    reverse(ResultsProgress, ResultsFinal).

% (iterate)
repeat_goal_(Goal, N, P1, P2, ResultsProgress, ResultsFinal) :-

    call(Goal, P1, P2, Result),
    Next is N - 1,
    repeat_goal_(Goal, Next, P1, P2, [Result|ResultsProgress], ResultsFinal).

%-------------------------------------------------------------------------------

% (start)
repeat_goal(Goal, N, P1, P2, P3, Results) :-

    % fail point
    N >= 0,
    repeat_goal_(Goal, N, P1, P2, P3, [], Results).

% (done)
repeat_goal_(_Goal, 0, _P1, _P2, _P3, ResultsProgress, ResultsFinal) :-
    reverse(ResultsProgress, ResultsFinal).

% (iterate)
repeat_goal_(Goal, N, P1, P2, P3, ResultsProgress, ResultsFinal) :-

    call(Goal, P1, P2, P3, Result),
    Next is N - 1,
    repeat_goal_(Goal, Next, P1, P2, P3,
                 [Result|ResultsProgress], ResultsFinal).

%-------------------------------------------------------------------------------

% (start)
repeat_goal(Goal, N, P1, P2, P3, P4, Results) :-

    % fail point
    N >= 0,
    repeat_goal_(Goal, N, P1, P2, P3, P4, [], Results).

% (done)
repeat_goal_(_Goal, 0, _P1, _P2, _P3, _P4, ResultsProgress, ResultsFinal) :-
    reverse(ResultsProgress, ResultsFinal).

% (iterate)
repeat_goal_(Goal, N, P1, P2, P3, P4,
             ResultsProgress, ResultsFinal) :-

    call(Goal, P1, P2, P3, P4, Result),
    Next is N - 1,
    repeat_goal_(Goal, Next, P1, P2, P3, P4,
                 [Result|ResultsProgress], ResultsFinal).

%-------------------------------------------------------------------------------

% (start)
repeat_goal(Goal, N, P1, P2, P3, P4, P5, Results) :-

    % fail point
    N >= 0,
    repeat_goal_(Goal, N, P1, P2, P3, P4, P5, [], Results).

% (done)
repeat_goal_(_Goal, 0, _P1, _P2, _P3, _P4, _P5,
             ResultsProgress, ResultsFinal) :-
    reverse(ResultsProgress, ResultsFinal).

% (iterate)
repeat_goal_(Goal, N, P1, P2, P3, P4, P5,
             ResultsProgress, ResultsFinal) :-

    call(Goal, P1, P2, P3, P4, P5, Result),
    Next is N - 1,
    repeat_goal_(Goal, Next, P1, P2, P3, P4, P5,
                 [Result|ResultsProgress], ResultsFinal).

%-------------------------------------------------------------------------------

% (start)
repeat_goal(Goal, N, P1, P2, P3, P4, P5, P6, Results) :-

    % fail point
    N >= 0,
    repeat_goal_(Goal, N, P1, P2, P3, P4, P6, P5, [], Results).

% (done)
repeat_goal_(_Goal, 0, _P1, _P2, _P3, _P4, _P5, _P6,
             ResultsProgress, ResultsFinal) :-
    reverse(ResultsProgress, ResultsFinal).

% (iterate)
repeat_goal_(Goal, N, P1, P2, P3, P4, P5, P6, ResultsProgress, ResultsFinal) :-

    call(Goal, P1, P2, P3, P4, P5, P6, Result),
    Next is N - 1,
    repeat_goal_(Goal, Next, P1, P2, P3, P4, P5, P6,
                 [Result|ResultsProgress], ResultsFinal).
