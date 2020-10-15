/*******************************************************************************
* FILENAME / MODULE : port_layer.pl / port_layer
*
* DESCRIPTION :
*       These are predicates providing a portability layer for the SWI-Prolog
*       platform, by implementing some unique, or convenient, SICStus Prolog
*       features.
*
* PUBLIC PREDICATES :
*       current_directory(?CurrDir)
*       current_directory(-OldDir, +NewDir)
*       datime(-Datime)
*       datime(+When, -Datime)
*       delete_directory(+Dir, +Options)
*       directory_exists(+Dir)
*       file_exists(+FilePath)
*       guid(-Guid)
*       guids(+N, -Guids)
*       make_directory(+Path)
*       now(-When)
*       setrand(+Seed)
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
* 2020-08-08  GT Nunes          Module creation
*
*******************************************************************************/

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

%-------------------------------------------------------------------------------
% date/time portability

% Unify Datime with the current date and time as a datime/6 record of the
% form datime(Year,Month,Day,Hour,Min,Sec). All fields are integers.
% datime(-Datime)
% Datime        current date and time as datime(Year,Month,Day,Hour,Min,Sec)
datime(Datime) :-

    get_time(Now),
    stamp_date_time(Now,
                    date(Year, Month, Day, Hour, Min, SecF, _, _, _), local),
    Sec is round(SecF),
    Datime = datime(Year, Month, Day, Hour, Min, Sec).

% convert a UNIX timestamp When, to a datime/6 record
% datime(+When, -Datime)
% When      timestamp, as a UNIX timestamp
% Datime    date and time as datime(Year,Month,Day,Hour,Min,Sec)
datime(When, Datime) :-

    stamp_date_time(When,
                    date(Year, Month, Day, Hour, Min, SecF, _, _, _), local),
    Sec is round(SecF),
    Datime = datime(Year, Month, Day, Hour, Min, Sec).

% unify Now with the current datetime as a UNIX timestamp (integer)
% now(-Now)
% Now       integer holding the current datetime
now(Now) :-

    get_time(Time),
    Now is round(Time).

%-------------------------------------------------------------------------------
% file system portability

% unify CurrDir with the current working directory
% current_directory(?CurrDir)
% CurrDir   the current working directory
current_directory(CurrDir) :-

    ( (var(CurrDir) , working_directory(CurrDir, CurrDir))
    ; (nonvar(CurrDir) , working_directory(_, CurrDir)) ).

% unify OldDir with current working directory and change it to NewDir
% current_directory(-OldDir, +NewDir)
% OldDir    the old current working directory
% NewDir    the new current working directory
current_directory(OldDir, NewDir) :-
    working_directory(OldDir, NewDir).

% recursively delete directory Dir
delete_directory(Dir, [if_nonempty(delete)]) :-
    delete_directory_and_contents(Dir).

% delete directory Dir if empty, or fail if otherwise
delete_directory(Dir, [if_nonempty(error)]) :-
    delete_directory(Dir).

% delete directory Dir if empty, or fail if otherwise
delete_directory(Dir, [if_nonempty(fail)]) :-
    delete_directory(Dir).

% delete directory Dir if empty, or ignore it if otherwise
delete_directory(Dir, [if_nonempty(ignore)]) :-
    (delete_directory(Dir) ; true).

% succeed if directory Dir exist, fail otherwise
% directory_exists(+Dir)
% Dir   directory to assert
directory_exists(Dir) :-
    exists_directory(Dir).

% create the specified directory (fail if it already exists)
% make_directory(+Path)
% Path      the directory to create
make_directory(Path) :-
    make_directory_path(Path).

% succeed if file FilePath exists, fail otherwise
% file_exists(+FilePath)
% FilePath  file to assert
file_exists(FilePath) :-
    exists_file(FilePath).

%-------------------------------------------------------------------------------
% GUID generation

% generate a GUID (UUID Version 4)
% guid(-Guid)
% Guid      the new GUID
guid(Guid) :-
    uuid(Guid, [version(4)]).

% generate N GUIDs (UUID Version 4)
% guids(+N, -Guids)
% N         number of GUIDs to generate
% Guids     the list of new GUIDs
guids(N, Guids) :-
    repeat_goal(guid, N, Guids).

%-------------------------------------------------------------------------------

% seed the random number generator SICStus style (with integer Seed)
% setrand(+Seed)
% Seed      an arbitrary integer value
setrand(Seed) :-
    set_random(seed(Seed)).
