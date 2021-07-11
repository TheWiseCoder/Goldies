:- module(port_layer,
    [
        current_directory/1,
        current_directory/2,
        datime/1,
        datime/2,
        delete_directory/2,
        directory_exists/1,
        file_exists/1,
        guid/1,
        guids/2,
        make_directory/1,
        now/1,
        setrand/1
    ]).

/** <module> Portability layer for SWI-Prolog

These are predicates providing a portability layer for the SWI-Prolog platform,
by implementing some unique, or convenient, SICStus Prolog features.

@author GT Nunes
@version 1.3
@copyright (c) TheWiseCoder 2020-2021
@license BSD-3-Clause License
*/

%-------------------------------------------------------------------------------------

:- use_module(library(filesex),
    [
        delete_directory_and_contents/1
    ]).

:- use_module(library(uuid),
    [
        uuid/2
    ]).

:- use_module('../common/repeat_goal',
    [
        repeat_goal/3
    ]).

%-------------------------------------------------------------------------------------
% date/time portability

%! datime(-Datime:term) is det.
%
%  Unify Datime with the current datetime as a `datime/6` term.
%
%  `datime/6` term have the form `datime(Year,Month,Day,Hour,Min,Sec).`
%  All fields are integers.

%  @param Datime Current date and time as a `datime/6` term

datime(Datime) :-

    get_time(Now),
    stamp_date_time(Now,
                    date(Year, Month, Day, Hour, Min, SecF, _, _, _), local),
    Sec is round(SecF),
    Datime = datime(Year, Month, Day, Hour, Min, Sec).

%! datime(+When:int, -Datime:term) is det.
%
%  Unify Datime with the datetime given by the UNIX timestamp When,
%  as a `datime/6` term.
%
%  @param When   Date and time as a UNIX timestamp
%  @param Datime Date and time as a `datime/6` term

datime(When, Datime) :-

    stamp_date_time(When,
                    date(Year, Month, Day, Hour, Min, SecF, _, _, _), local),
    Sec is round(SecF),
    Datime = datime(Year, Month, Day, Hour, Min, Sec).

%! now(-Now:int) is det.
%
%  Unify Now with the current datetime as a UNIX timestamp.
%
%  @param Now The current datetime as a UNIX timestamp

now(Now) :-

    get_time(Time),
    Now is round(Time).

%-------------------------------------------------------------------------------------
% file system portability

%! current_directory(?CurrDir:atom) is det.
%
%  Unify CurrDir with the current working directory.
%
%  @param CurrDir The current working directory

current_directory(CurrDir) :-

    ( (var(CurrDir) , working_directory(CurrDir, CurrDir))
    ; (nonvar(CurrDir) , working_directory(_, CurrDir)) ),
    !.

%! current_directory(-OldDir:atom, +NewDir:atom) is det.
%
%  Unify OldDir with current working directory and change it to NewDir.
%
%  @param OldDir The old current working directory
%  @param NewDir The new current working directory

current_directory(OldDir, NewDir) :-
    working_directory(OldDir, NewDir).

%! delete_directory(-Dir:atom, +Options:list) is semidet.
%
%  Recursively delete directory Dir, according to Options.
%  Options are:
%  ~~~
%  [if_nonempty(delete)] - delete directory even if not empty
%  [if_nonempty(error)]  - delete directory if empty, throw error otherwise
%  [if_nonempty(fail)]   - delete directory if empty, fail otherwise
%  [if_nonempty(ignore)] - delete directory if empty, ignore otherwise
%  ~~~
%
%  @param Dir The directory to delete
%  @param Options The options directing the delete operation

delete_directory(Dir, [if_nonempty(delete)]) :-
    delete_directory_and_contents(Dir).

% 
delete_directory(Dir, [if_nonempty(error)]) :-
    delete_directory(Dir).

% delete directory Dir if empty, or fail if otherwise
delete_directory(Dir, [if_nonempty(fail)]) :-
    delete_directory(Dir).

% delete directory Dir if empty, or ignore it if otherwise
delete_directory(Dir, [if_nonempty(ignore)]) :-

    (delete_directory(Dir) ; true),
    !.

%! directory_exists(+Dir:atom) is semidet.
%
%  True if directory Dir exist, fail otherwise.
%
%  @param Dir Directory to assert

directory_exists(Dir) :-
    exists_directory(Dir).

%! make_directory(+Path:atom) is semidet.
%
%  Create the specified directory. Fail if operation is not successful.
%
%  @param Path The directory to create

make_directory(Path) :-
    make_directory_path(Path).

%! file_exists(+FilePath:atom) is semidet.
%
%  True if file FilePath exists, fail otherwise.
%
%  @param FilePath File to assert

file_exists(FilePath) :-
    exists_file(FilePath).

%-------------------------------------------------------------------------------------
% GUID generation

%! guid(-Guid:atom) is det.
%
%  Generate a GUID. GUIDs are UUIDs Version 4.
%
%  @param Guid The newly-generated GUID
guid(Guid) :-
    uuid(Guid, [version(4)]).

%! guids(+N:int, -Guids:list) is det.
%
%  Generate N distinct GUIDs. GUIDs are UUIDs Version 4.
%
%  @param N     Number of GUIDs to generate
%  @param Guids List of newly-generated GUIDs

guids(N, Guids) :-
    repeat_goal(guid, N, Guids).

%-------------------------------------------------------------------------------------

%! setrand(+Seed:int) is det.
%
%  Seed the random number generator SICStus style (with an integer).
%
%  @param Seed An arbitrary integer
%
setrand(Seed) :-
    set_random(seed(Seed)).
