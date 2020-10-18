/*******************************************************************************
* FILENAME / MODULE : datetime_marks.pl / datetime_marks
*
* DESCRIPTION :
*       Date and time related utilities.
*
* PUBLIC PREDICATES :
*       date_now(-Year, -Month, -Day)
*       date_display(today, -Display)
*       date_display(+When, -Display)
*       date_display(+Year, +Month, +Day, -Display)
*       date_gregorian(+Year, +Month, +Day)
*       date_weekday(today, -Dow)
*       date_weekday(+Mjd, -Dow)
*       date_weekday(+Year, +Month, +Day, -Dow)
*       datetime_now(-Year, -Month, -Day, -Hour, -Min, -Sec)
*       datetime_display(now, -Display)
*       datetime_display(+When, -Display)
*       datetime_display(+Year, +Month, +Day, +Hour, +Min, +Sec, -Display)
*       datetime_ietf(now, -DtIetf)
*       datetime_ietf(today, DtIetf)
*       datetime_ietf(+Mjd, -DtIetf)
*       datetime_ietf(+Year, +Month, +Day, +Hour, +Min, +Sec, -DtIetf)
*       month_ordinal(?Month, ?Ord)
*       time_now(-Hour, -Min, -Sec)
*       time_display(now, -Display)
*       time_display(+When, -Display)
*       time_display(+Hour, +Min, +Sec, -Display)
*       weekday_ordinal(?Dow, ?Ord)
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
* 2020-08-09  GT Nunes          Added conditional compilation for SICStus/SWI
*
*******************************************************************************/

:- module(datetime_marks,
    [
        date_now/3,
        date_display/2,
        date_display/4,
        date_gregorian/3,
        date_weekday/2,
        date_weekday/4,
        datetime_now/6,
        datetime_display/2,
        datetime_display/7,
        datetime_ietf/2,
        datetime_ietf/7,
        month_ordinal/2,
        time_now/3,
        time_display/2,
        time_display/4,
        weekday_ordinal/2]).

:- use_module(library(clpfd)).

:- if(current_prolog_flag(dialect, sicstus)).   % SICStus ----------------------

:- use_module(library(system),
    [
        datime/1,
        datime/2
    ]).

date_weekday_(Year, Month, Day, Dow) :-
    gregorian_mjd(Year, Month, Day, MJD),
    (MJD + 2) mod 7 #= Ord,
    weekday_ordinal(Dow, Ord).

:- elif(current_prolog_flag(dialect, swi)).     % SWI-Prolog -------------------

:- use_module('../swi/port_layer',
    [
        datime/1,
        datime/2
    ]).

date_weekday_(Year, Month, Day, Dow) :-
    day_of_the_week(date(Year, Month, Day), OrdSwi),
    Ord is OrdSwi - 1,
    weekday_ordinal(Dow, Ord).

:- endif.                                       % ------------------------------

:- use_module(library(codesio),
    [
        format_to_codes/3
    ]).

:- use_module('atom_marks',
    [
        atom_int/3
    ]).

%-------------------------------------------------------------------------------
% these are useful datetime utility-like predicates

% unify Year/Month/Day with the corresponding values for today
% date_now(-Year, -Month, -Day)
date_now(Year, Month, Day) :-

   datime(Now),
   datime(Year, Month, Day, _Hour, _Min, _Sec) = Now.

% unify Year/Month/Day/Hour/Min/Sec with the corresponding values for now
% datetime_now(-Year, -Month, -Day, -Hour, -Min, -Sec)
datetime_now(Year, Month, Day, Hour, Min, Sec) :-

   datime(Now),
   datime(Year, Month, Day, Hour, Min, Sec) = Now.

% unify Hour/Min/Sec with the corresponding values for now
% time_now(-Hour, -Min, -Sec)
time_now(Hour, Min, Sec) :-

   datime(Now),
   datime(_Year, _Month, _Day, Hour, Min, Sec) = Now.

%-------------------------------------------------------------------------------
% date and time in display format

% unify Display with today's date as YYYY-MM-DD
% date_display(today, -Display)
% today     today's date
% Display   atom holding the date as YYYY-MM-DD
date_display(today, Display) :-

   datime(Now),
   datime(Year, Month, Day, _Hour, _Min, _Sec) = Now,
   date_display(Year, Month, Day, Display).

% unify Display with When's date as YYYY-MM-DD
% date_display(+When, -Display)
% When      UNIX-style timestamp (integer)
% Display   atom holding the date as YYYY-MM-DD
date_display(When, Display) :-

   datime(When, Timestamp),
   datime(Year, Month, Day, _Hour, _Min, _Sec) = Timestamp,
   date_display(Year, Month, Day, Display).

% unify Display with the given date as YYYY-MM-DD
% date_display(+Year, +Month, +Day, -Display)
% Year,Month,Day    valid reference date
% Display           atom holding the date as YYYY-MM-DD
date_display(Year, Month, Day, Display) :-

   atom_int(Ayear, Year, 4),
   atom_int(Amonth, Month, 2),
   atom_int(Aday, Day, 2),
   format_to_codes('~a-~a-~a', [Ayear,Amonth,Aday], Codes),
   atom_codes(Display, Codes).

% unify Display with the date and time now, as YYYY-MM-DD HH:MM:SS
% datetime_display(now, -Display)
% now       current datetime
% Display   atom holding the datetime as YYYY-MM-DD HH:MM:SS
datetime_display(now, Display) :-

   datime(Now),
   datime(Year, Month, Day, Hour, Min, Sec) = Now,
   datetime_display(Year, Month, Day, Hour, Min, Sec, Display).

% unify Display with When's date and time, as YYYY-MM-DD HH:MM:SS
% datetime_display(+When, -Display)
% When      UNIX-style timestamp (integer)
% Display   atom holding the datetime as YYYY-MM-DD HH:MM:SS
datetime_display(When, Display) :-

   datime(When, Timestamp),
   datime(Year, Month, Day, Hour, Min, Sec) = Timestamp,
   datetime_display(Year, Month, Day, Hour, Min, Sec, Display).

% unify Display with the given date and time, as YYYY-MM-DD HH:MM:SS
% datetime_display(+Year, +Month, +Day, +Hour, +Min, +Sec, -Display)
% Year,Month,Day    valid reference date
% Hour,Min,Sec      valid reference time of day
% Display           atom holding the datetime as YYYY-MM-DD HH:MM:SS
datetime_display(Year, Month, Day, Hour, Min, Sec, Display) :-

   atom_int(Ayear, Year, 4),
   atom_int(Amonth, Month, 2),
   atom_int(Aday, Day, 2),
   atom_int(Ahour, Hour, 2),
   atom_int(Amin, Min, 2),
   atom_int(Asec, Sec, 2),
   format_to_codes('~a-~a-~a ~a:~a:~a',
                   [Ayear,Amonth,Aday,Ahour,Amin,Asec], Codes),
   atom_codes(Display, Codes).

% unify Display with the time now as HH:MM:SS
% time_display(now, -Display)
% now       current time
% Display   atom holding the time as HH:MM:SS
time_display(now, Display) :-

   datime(Now),
   datime(_Year, _Month, _Day, Hour, Min, Sec) = Now,
   time_display(Hour, Min, Sec, Display).

% unify Display with When's time as HH:MM:SS
% time_display(+When, -Display)
% When      UNIX-style timestamp (integer)
% Display   atom holding the time as HH:MM:SS
time_display(When, Display) :-

   datime(When, Timestamp),
   datime(_Year, _Month, _Day, Hour, Min, Sec) = Timestamp,
   time_display(Hour, Min, Sec, Display).

% unify Display with the given time as HH:MM:SS
% time_display(+Hour, +Min, +Sec, -Display)
% Year,Month,Day    valid reference date
% Hour,Min,Sec      valid reference time of day
% Display           atom holding the time as HH:MM:SS
time_display(Hour, Min, Sec, Display) :-

   atom_int(Ahour, Hour, 2),
   atom_int(Amin, Min, 2),
   atom_int(Asec, Sec, 2),
   format_to_codes('~a:~a:~a', [Ahour,Amin,Asec], Codes),
   atom_codes(Display, Codes).

%-------------------------------------------------------------------------------
% date and time in the Internet Message Format
% (IETF timestamp, as defined in the RFC 2822 -
%  Example: Wed, 01 Jan 2020 08:05:03 GMT)

% unify DtIetf with date and time now, as defined by the Internet Message Format
% datetime_ietf(now, -DtIetf)
% now       today's datetime
% DtIetf    the corresponding datetime in IETF format
datetime_ietf(now, DtIetf) :-

   datime(Now),
   datime(Year, Month, Day, Hour, Min, Sec) = Now,
   datetime_ietf(Year, Month, Day, Hour, Min, Sec, DtIetf).

% unify DtIetf with today's date, as defined by the Internet Message Format
% datetime_ietf(today, -DtIetf)
% today     today's date
% DtIetf    the corresponding datetime in IETF format
datetime_ietf(today, DtIetf) :-

   datime(Now),
   datime(Year, Month, Day, _, _, _) = Now,
   datetime_ietf(Year, Month, Day, 0, 0, 0, DtIetf).

% unify DtIetf with Mjd (date and time in Modified Julian Date format),
% as defined by the Internet Message Format
% Mjd       date in Modified Julian Date format
% DtIetf    the corresponding datetime in IETF format
datetime_ietf(Mjd, DtIetf) :-

   gregorian_mjd(Year, Mon, Day, Mjd),
   datetime_ietf(Year, Mon, Day, 0, 0, 0, DtIetf).

% unify DtIetf with the given date and time, as defined by the
% Internet Message Format
% datetime_ietf(+Year, +Month, +Day, +Hour, +Min, +Sec, -DtIetf)
% Year,Month,Day    valid reference date
% Hour,Min,Sec      valid reference time of day
% DtIetf            the corresponding datetime in IETF format
datetime_ietf(Year, Month, Day, Hour, Min, Sec, DtIetf) :-

   date_weekday(Year, Month, Day, Dow),
   atom_int(Ayear, Year, 4),
   month_ordinal(Amonth, Month),
   atom_int(Aday, Day, 2),
   atom_int(Ahour, Hour, 2),
   atom_int(Amin, Min, 2),
   atom_int(Asec, Sec, 2),
   format_to_codes('~a, ~a ~a ~a ~a:~a:~a GMT',
                   [Dow,Aday,Amonth,Ayear,Ahour,Amin,Asec], Codes),
   atom_codes(DtIetf, Codes).

%-------------------------------------------------------------------------------
% bind a gregorian date to its corresponding short-hand weekday name

% unify Dow with today's day of the week
% date_weekday(today, -Dow)
% today     atom signifying today's date
% Dow       corresponding short-hand weekday name
date_weekday(today, Dow) :-

    date_now(Year, Month, Day),
    date_weekday(Year, Month, Day, Dow).

% unify Dow with the day of the week of Mjd (date and time in
% Modified Julian Date format)
% date_weekday(+Mjd, -Dow)
% Mjd       date in Modified Julian Date format
% Dow       corresponding short-hand weekday name
date_weekday(Mjd, Dow) :-

    (Mjd + 2) mod 7 #= Ord,
    weekday_ordinal(Dow, Ord).

% unify Dow with the given date's day of the week
% date_weekday(+Year, +Month, +Day, -Dow)
% Year,Month,Day    valid reference date
% Dow               corresponding short-hand weekday name
date_weekday(Year, Month, Day, Dow) :-
    date_weekday_(Year, Month, Day, Dow).

%-------------------------------------------------------------------------------
% True if Day, Month, and Year are a valid date in the Gregorian calendar.
% Note that all the code herein does is to constrain the relationships between
% the values of Day, Month, and Year. As an example, this would iterate on all
% leap years of the 20th century:
%    date_gregorian(Y, M, 29), Y #>= 1900 #/\ Y #< 2000, indomain(Y)

% date_gregorian(?Year, ?Month, ?Day)
% Year      a valid year in the Gregorian calendar (4713 BC - 3267 AD)
% Month     a valid month ordinal (1 - 12)
% Day       a valid day ordinal (1 - 31)
date_gregorian(Year, Month, Day) :-

    Year in -4713..3267,
    Month in 1..12,
    (   (Day in 1..28)
    #\/ (Month #\= 2 #/\ Day in 29..30)
    #\/ ((Month #= 1 #\/ Month #= 3 #\/ Month #= 5 #\/ Month #= 7 #\/
          Month #= 8 #\/ Month #= 10 #\/ Month #= 12) #/\ Day #= 31)
    #\/ (Month #= 2 #/\ Day #= 29 #/\ Year mod 400 #= 0)
    #\/ (Month #= 2 #/\ Day #= 29 #/\ Year mod 4 #= 0 #/\ Year mod 100 #\= 0)
    ).

%-------------------------------------------------------------------------------
% Bind a date in the Gregorian calendar to its corresponding Modified
% Julian Day number. For a competent explanation of the Julian day concept,
% see https://en.wikipedia.org/wiki/Julian_day. For an interesting approach
% to this subject (and the actual origin of this code), see Michael Hendricks'
% 'julian.pl' (https://gist.github.com/mndrix/5173377).

gregorian_mjd(Year, Month, Day, MJD) :-

    date_gregorian(Year, Month, Day),
    MJD in -2400328 .. 514671,

    Y = 4716,
    V = 3,
    J = 1401,
    U = 5,
    M = 2,
    S = 153,
    N = 12,
    W = 2,
    R = 4,
    B = 274277,
    P = 1461,
    C = -38,
    F0 #= JD + J,
    F1 #= F0 + (((4 * JD + B) / 146097) * 3) / 4 + C,
    E #= R * F1 + V,
    G #= mod(E, P) / R,
    H #= U * G + W,

    Day #= (mod(H, S)) / U + 1,
    Month #= mod(H / S + M, N) + 1,
    Year #= E / P - Y + (N + M - Month) / N,
    MJD #= JD - 2400001,

    ( ground(MJD) ->
        labeling([ff,up,bisect], [Year,Month,Day])
    ; ground(Year), ground(Month), ground(Day) ->
        labeling([leftmost, up, bisect], [MJD])
    ; true ).

%-------------------------------------------------------------------------------
% bind the short-hand month name to its ordinal

% month_ordinal(<month>, -Ord)
% <month>   the short-hand month name
% Ord       the corresponding ordinal
month_ordinal('Jan', Ord) :- Ord = 1.
month_ordinal('Feb', Ord) :- Ord = 2.
month_ordinal('Mar', Ord) :- Ord = 3.
month_ordinal('Apr', Ord) :- Ord = 4.
month_ordinal('May', Ord) :- Ord = 5.
month_ordinal('Jun', Ord) :- Ord = 6.
month_ordinal('Jul', Ord) :- Ord = 7.
month_ordinal('Aug', Ord) :- Ord = 8.
month_ordinal('Sep', Ord) :- Ord = 9.
month_ordinal('Oct', Ord) :- Ord = 10.
month_ordinal('Nov', Ord) :- Ord = 11.
month_ordinal('Dez', Ord) :- Ord = 12.

% month_ordinal(-Month, <ord>)
% Month     the short-hand month name
% <ord>     the corresponding ordinal
month_ordinal(Month,  1) :- Month = 'Jan'.
month_ordinal(Month,  2) :- Month = 'Feb'.
month_ordinal(Month,  3) :- Month = 'Mar'.
month_ordinal(Month,  4) :- Month = 'Apr'.
month_ordinal(Month,  5) :- Month = 'May'.
month_ordinal(Month,  6) :- Month = 'Jun'.
month_ordinal(Month,  7) :- Month = 'Jul'.
month_ordinal(Month,  8) :- Month = 'Aug'.
month_ordinal(Month,  9) :- Month = 'Sep'.
month_ordinal(Month, 10) :- Month = 'Oct'.
month_ordinal(Month, 11) :- Month = 'Nov'.
month_ordinal(Month, 12) :- Month = 'Dec'.

%-------------------------------------------------------------------------------
% bind the short-hand weekday name with its ISO ordinal value

% weekday_ordinal(+<dow>, -Ord)
% <dow>     short-hand weekday name
% Ord       the corresponding ISO ordinal number
weekday_ordinal('Mon', Ord) :- Ord = 0.
weekday_ordinal('Tue', Ord) :- Ord = 1.
weekday_ordinal('Wed', Ord) :- Ord = 2.
weekday_ordinal('Thu', Ord) :- Ord = 3.
weekday_ordinal('Fri', Ord) :- Ord = 4.
weekday_ordinal('Sat', Ord) :- Ord = 5.
weekday_ordinal('Sun', Ord) :- Ord = 6.

% weekday_ordinal(-Dow>, +<ord>)
% <dow>     short-hand weekday name
% Ord       the corresponding ISO ordinal number
weekday_ordinal(Dow, 0) :- Dow = 'Mon'.
weekday_ordinal(Dow, 1) :- Dow = 'Tue'.
weekday_ordinal(Dow, 2) :- Dow = 'Wed'.
weekday_ordinal(Dow, 3) :- Dow = 'Thu'.
weekday_ordinal(Dow, 4) :- Dow = 'Fri'.
weekday_ordinal(Dow, 5) :- Dow = 'Sat'.
weekday_ordinal(Dow, 6) :- Dow = 'Sun'.
