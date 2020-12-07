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
        weekday_ordinal/2
    ]).

/** <module>Date and time related utilities

@author GT Nunes
@version 1.0
@copyright (c) 2020 GT Nunes
@license BSD-3-Clause License
*/

%-------------------------------------------------------------------------------------

:- use_module(library(clpfd)).

:- if(current_prolog_flag(dialect, sicstus)).

:- use_module(library(system),
    [
        datime/1,
        datime/2
    ]).

date_weekday_(Year, Month, Day, Dow) :-
    gregorian_mjd(Year, Month, Day, MJD),
    (MJD + 2) mod 7 #= Ord,
    weekday_ordinal(Dow, Ord).

:- elif(current_prolog_flag(dialect, swi)).

:- use_module('../swi/port_layer',
    [
        datime/1,
        datime/2
    ]).

date_weekday_(Year, Month, Day, Dow) :-
    day_of_the_week(date(Year, Month, Day), OrdSwi),
    Ord is OrdSwi - 1,
    weekday_ordinal(Dow, Ord).

:- endif.

:- use_module(library(codesio),
    [
        format_to_codes/3
    ]).

:- use_module('atom_marks',
    [
        atom_int/3
    ]).

%-------------------------------------------------------------------------------------

%! date_now(-Year:int, -Month:int, -Day:int) is det.
%
%  Unify Year, Month, and Day with the corresponding values for today.
%
%  @param Year  The current year
%  @param Month The current month
%  @param Day   The day of the month today

date_now(Year, Month, Day) :-

   datime(Now),
   datime(Year, Month, Day, _Hour, _Min, _Sec) = Now.

%! datetime_now(-Year:int, -Month:int, -Day:int, -Hour:int, -Min:int, -Sec:int) is det.
%
%  Unify Year, Month, Day, Hour, Min, and Sec with the corresponding values for now.
%
%  @param Year  The current year
%  @param Month The current month
%  @param Day   The day of the month today
%  @param Hour  The hour of day now
%  @param Min   The minute within the hour now
%  @param Sec   The second within the minute now

datetime_now(Year, Month, Day, Hour, Min, Sec) :-

   datime(Now),
   datime(Year, Month, Day, Hour, Min, Sec) = Now.

%! time_now(-Hour:int, -Min:int, -Sec:int) is det.
%
%  Unify Hour, Min, and Sec with the corresponding values for now.
%
%  @param Hour The hour of day now
%  @param Min  The minute within the hour now
%  @param Sec  The second within the minute now

time_now(Hour, Min, Sec) :-

   datime(Now),
   datime(_Year, _Month, _Day, Hour, Min, Sec) = Now.

%-------------------------------------------------------------------------------------

%! date_display(+When:int, -Display:atom) is det.
%
%  Unify Display with today's or When's date as YYYY-MM-DD.
%
%  @param When    UNIX-style timestamp
%  @param Display Atom holding the date as YYYY-MM-DD

date_display(today, Display) :-

   datime(Now),
   datime(Year, Month, Day, _Hour, _Min, _Sec) = Now,
   date_display(Year, Month, Day, Display).

date_display(When, Display) :-

   datime(When, Timestamp),
   datime(Year, Month, Day, _Hour, _Min, _Sec) = Timestamp,
   date_display(Year, Month, Day, Display).

%! date_display(+Year:int, +Month:int, +Day:int, -Display)
%
%  Unify Display with the given date as YYYY-MM-DD.
%
%  @param Year    The reference year
%  @param Month   The reference month
%  @param Day     The reference day
%  @param Display Atom holding the date as YYYY-MM-DD

date_display(Year, Month, Day, Display) :-

   atom_int(Ayear, Year, 4),
   atom_int(Amonth, Month, 2),
   atom_int(Aday, Day, 2),
   format_to_codes('~a-~a-~a', [Ayear,Amonth,Aday], Codes),
   atom_codes(Display, Codes).

%-------------------------------------------------------------------------------------

%! datetime_display(+When:int, -Display:atom) is det
%
%  Unify Display with the date and time `now` or at When, as YYYY-MM-DD HH:MM:SS.
%
%  @param When    UNIX-style timestamp
%  @param Display Atom holding the datetime as YYYY-MM-DD HH:MM:SS

datetime_display(now, Display) :-

   datime(Now),
   datime(Year, Month, Day, Hour, Min, Sec) = Now,
   datetime_display(Year, Month, Day, Hour, Min, Sec, Display).

datetime_display(When, Display) :-

   datime(When, Timestamp),
   datime(Year, Month, Day, Hour, Min, Sec) = Timestamp,
   datetime_display(Year, Month, Day, Hour, Min, Sec, Display).

%! datetime_display(+Year:int, +Month:int, +Day:int, +Hour:int, +Min:int, +Sec:int, -Display:atom) is det.
%
%  Unify Display with the given date and time, as YYYY-MM-DD HH:MM:SS.
%
%  @param Year    The reference year
%  @param Month   The reference month
%  @param Day     The reference day
%  @param Hour    The reference hour
%  @param Min     The reference minute
%  @param Sec     The reference second
%  @param Display Atom holding the datetime as YYYY-MM-DD HH:MM:SS

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

%-------------------------------------------------------------------------------------

%! time_display(+When:int, -Display:atom) is det.
%
%  Unify Display with the time `now` or at When, as HH:MM:SS.
%
%  @param now     Current time indicator
%  @param When    UNIX-style timestamp
%  @param Display Atom holding the time as HH:MM:SS

time_display(now, Display) :-

   datime(Now),
   datime(_Year, _Month, _Day, Hour, Min, Sec) = Now,
   time_display(Hour, Min, Sec, Display).

time_display(When, Display) :-

   datime(When, Timestamp),
   datime(_Year, _Month, _Day, Hour, Min, Sec) = Timestamp,
   time_display(Hour, Min, Sec, Display).

%! time_display(+Hour:int, +Min:int, +Sec:int, -Display:atom) is det.
%
%  Unify Display with the given time as HH:MM:SS.
%
%  @param Hour    The reference hour
%  @param Min     The reference minute
%  @param Sec     The reference second
%  @param Display Atom holding the time as HH:MM:SS

time_display(Hour, Min, Sec, Display) :-

   atom_int(Ahour, Hour, 2),
   atom_int(Amin, Min, 2),
   atom_int(Asec, Sec, 2),
   format_to_codes('~a:~a:~a', [Ahour,Amin,Asec], Codes),
   atom_codes(Display, Codes).

%-------------------------------------------------------------------------------------

%! datetime_ietf(+Mjd:int, -DtIetf:atom) is det.
%
%  Unify DtIetf with the IETF timestamp for `today`, `now` or at Mjd, as defined
%  in RFC 2822 (example: Wed, 01 Jan 2020 08:05:03 GMT).
%
%  @param Mjd    Date in Modified Julian Date format
%  @param DtIetf Atom holding the corresponding datetime in IETF format

datetime_ietf(now, DtIetf) :-

   datime(Now),
   datime(Year, Month, Day, Hour, Min, Sec) = Now,
   datetime_ietf(Year, Month, Day, Hour, Min, Sec, DtIetf).

datetime_ietf(today, DtIetf) :-

   datime(Now),
   datime(Year, Month, Day, _, _, _) = Now,
   datetime_ietf(Year, Month, Day, 0, 0, 0, DtIetf).

datetime_ietf(Mjd, DtIetf) :-

   gregorian_mjd(Year, Mon, Day, Mjd),
   datetime_ietf(Year, Mon, Day, 0, 0, 0, DtIetf).

%! datetime_ietf(+Year:int, +Month:int, +Day:int, +Hour:int, +Min:int, +Sec:int, -DtIetf:atom) is det.
%
%  Unify DtIetf with the IETF timestamp for the given date and time, as defined
%  in RFC 2822 (example: Wed, 01 Jan 2020 08:05:03 GMT).
%
%  @param Year   The reference year
%  @param Month  The reference month
%  @param Day    The reference day
%  @param Hour   The reference hour
%  @param Min    The reference minute
%  @param Sec    The reference second
%  @param DtIetf Atom holding the corresponding datetime in IETF format

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

%-------------------------------------------------------------------------------------

%! date_weekday(Mjd:int, -Dow:atom) is det.
%
%  Unify Dow with today's or Mjd's short-hand 3-letter weekday name.
%
%  @param Mjd Date in Modified Julian Date format
%  @param Dow Corresponding short-hand 3-letter weekday name

date_weekday(today, Dow) :-

    date_now(Year, Month, Day),
    date_weekday(Year, Month, Day, Dow).

date_weekday(Mjd, Dow) :-

    (Mjd + 2) mod 7 #= Ord,
    weekday_ordinal(Dow, Ord).

%! date_weekday(+Year:int, +Month:int, +Day:int, -Dow:atom) is det.
%
%  Unify Dow with the given date's short-hand 3-letter weekday name.
%
%  @param Year  The reference year
%  @param Month The reference month
%  @param Day   The reference day
%  @param Dow   Corresponding short-hand 3-letter weekday name

date_weekday(Year, Month, Day, Dow) :-
    date_weekday_(Year, Month, Day, Dow).

%-------------------------------------------------------------------------------------

%! date_gregorian(+Year:int, +Month:int, +Day:int) is semidet.
%
%  True if Day, Month, and Year constitute a valid date in the Gregorian calendar.
%
%  Note that all the code herein does is to constrain the relationships between
%  the values of Day, Month, and Year. As an example, this would iterate on all
%  leap years of the 20th century:
%  ~~~
%  date_gregorian(Y, M, 29), Y #>= 1900 #/\ Y #< 2000, indomain(Y)
%  ~~~
%
%  @param Year  Year in the Gregorian calendar (4713 BC - 3267 AD)
%  @param Month Month ordinal (1 - 12)
%  @param Day   Day ordinal (1 - 31)

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

%-------------------------------------------------------------------------------------

%! gregorian_mjd(+Year, +Month, +Day, -MJD) is det
%
%  Unify MJD with the Modified Julian Day number corresponding to the given date
%  in the Gregorian calendar.
%
%  For a competent explanation of the Julian day concept, see
%  https://en.wikipedia.org/wiki/Julian_day.
%  For an interesting approach to this subject (and the actual origin of this code),
%  see Michael Hendricks's `julian.pl` (https://gist.github.com/mndrix/5173377).
%
%  @param Year  The reference year
%  @param Month The reference month
%  @param Day   The reference day
%  @param Mjd   Date in Modified Julian Date format

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

%-------------------------------------------------------------------------------------

%! month_ordinal(+Month:atom, -Ord:int) is det.
%! month_ordinal(-Month:atom, +Ord:int) is det.
%
%  Unify Month or Ord with the short-hand 3-letter month name or the month's
%  1-based ordinal, respectively.
%
%  @param Month The short-hand month name
%  @param Ord   The corresponding 1-based ordinal
%
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

%-------------------------------------------------------------------------------------

%! weekday_ordinal(+Dow:atom, -Ord:int) is det.
%! weekday_ordinal(-Dow:atom, +Ord:int) is det.
%
%  Bind the short-hand weekday name with its ISO ordinal value.
%
%  @param Dow Short-hand 3-letter weekday name
%  @param Ord The corresponding ISO ordinal number

weekday_ordinal('Mon', Ord) :- Ord = 0.
weekday_ordinal('Tue', Ord) :- Ord = 1.
weekday_ordinal('Wed', Ord) :- Ord = 2.
weekday_ordinal('Thu', Ord) :- Ord = 3.
weekday_ordinal('Fri', Ord) :- Ord = 4.
weekday_ordinal('Sat', Ord) :- Ord = 5.
weekday_ordinal('Sun', Ord) :- Ord = 6.

weekday_ordinal(Dow, 0) :- Dow = 'Mon'.
weekday_ordinal(Dow, 1) :- Dow = 'Tue'.
weekday_ordinal(Dow, 2) :- Dow = 'Wed'.
weekday_ordinal(Dow, 3) :- Dow = 'Thu'.
weekday_ordinal(Dow, 4) :- Dow = 'Fri'.
weekday_ordinal(Dow, 5) :- Dow = 'Sat'.
weekday_ordinal(Dow, 6) :- Dow = 'Sun'.
