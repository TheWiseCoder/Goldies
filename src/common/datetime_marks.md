Small utilities handling date and time. The available predicates are listed below. Make sure to complement these brief descriptions with the documentation in the source code file.  
  
- `date_now(-Year, -Month, -Day)` - unify *Year*/*Month*/*Day* with the corresponding values for today  
- `datetime_now(-Year, -Month, -Day, -Hour, -Min, -Sec)` - unify *Year*/*Month*/*Day*/*Hour*/*Min*/*Sec* with the corresponding values for now  
- `time_now(-Hour, -Min, -Sec)` - unify *Hour*/*Min*/*Sec* with the corresponding values for now  
  
- `date_display(today, -Display)` - unify *Display* with today's date as YYYY-MM-DD  
- `date_display(+When, -Display)`-  unify *Display* with *When*'s date as YYYY-MM-DD  
- `date_display(+Year, +Month, +Day, -Display)` - unify *Display* with the given date as YYYY-MM-DD  
- `datetime_display(now, -Display)` - unify *Display* with the date and time now, as YYYY-MM-DD HH:MM:SS  
- `datetime_display(+When, -Display)` - unify *Display* with *When*'s date and time, as YYYY-MM-DD HH:MM:SS  
- `datetime_display(+Year, +Month, +Day, +Hour, +Min, +Sec, -Display)` - unify *Display* with the given date and time, as YYYY-MM-DD HH:MM:SS  
- `time_display(now, -Display)` - unify *Display* with the time now as HH:MM:SS  
- `time_display(+When, -Display)` - unify *Display* with *When*'s time as HH:MM:SS  
- `time_display(+Hour, +Min, +Sec, -Display)` - unify *Display* with the given time as HH:MM:SS  
  
- `datetime_ietf(now, -DtIetf)` - unify *DtIetf* with date and time now, as defined by the Internet Message Format  
- `datetime_ietf(today, DtIetf)` - unify *DtIetf* with today's date, as defined by the Internet Message Format  
- `datetime_ietf(+Mjd, -DtIetf)` - unify *DtIetf* with *Mjd* (date and time in Modified Julian Date format), as defined by the Internet Message Format  
- `datetime_ietf(+Year, +Month, +Day, +Hour, +Min, +Sec, -DtIetf)` - - unify *DtIetf* with the given date and time, as defined by the Internet Message Format  
  
- `date_weekday(today, -Dow)` - unify *Dow* with today's day of the week  
- `date_weekday(+Mjd, -Dow)` - unify *Dow* with the day of the week of Mjd (date and time in Modified Julian Date format)  
- `date_weekday(+Year, +Month, +Day, -Dow)` - unify *Dow* with the given date's day of the week  
  
- `month_ordinal(?Month, ?Ord)` - unify *Month* or *Ord* with the corresponding month's 3-letter abbreviation and its ordinal 1-base sequence, respectively  
- `weekday_ordinal(?Dow, ?Ord)` - unify *Dow* or *Ord* with the corresponding day of the week's 3-letter abbbreviation and its 0-based ordinal, respectively (Mon is 0)
  
- `date_gregorian(+Year, +Month, +Day)` - *true* if *Day*, *Month*, and Year are a valid date in the Gregorian calendar  