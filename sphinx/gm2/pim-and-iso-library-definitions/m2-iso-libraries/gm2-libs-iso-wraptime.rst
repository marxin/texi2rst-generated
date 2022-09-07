.. _gm2-libs-iso-wraptime:

gm2-libs-iso/wraptime
^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE wraptime ;

  (*
      Description: provides an interface to various time related
                   entities on the underlying host operating system.
                   It provides access to the glibc/libc functions:
                   gettimeofday, settimeofday and localtime_r.
  *)

  FROM SYSTEM IMPORT ADDRESS ;

  TYPE
  timeval (type)
     timeval  = ADDRESS ;
  timezone (type)
     timezone = ADDRESS ;
  tm (type)
     tm       = ADDRESS ;

  (*
     InitTimeval - returns a newly created opaque type.
  *)

  InitTimeval
  PROCEDURE InitTimeval () : timeval ;

  (*
     KillTimeval - deallocates the memory associated with an
                   opaque type.
  *)

  KillTimeval
  PROCEDURE KillTimeval (tv: timeval) : timeval ;

  (*
     InitTimezone - returns a newly created opaque type.
  *)

  InitTimezone
  PROCEDURE InitTimezone () : timezone ;

  (*
     KillTimezone - deallocates the memory associated with an
                    opaque type.
  *)

  KillTimezone
  PROCEDURE KillTimezone (tv: timezone) : timezone ;

  (*
     InitTM - returns a newly created opaque type.
  *)

  InitTM
  PROCEDURE InitTM () : tm ;

  (*
     KillTM - deallocates the memory associated with an
              opaque type.
  *)

  KillTM
  PROCEDURE KillTM (tv: tm) : tm ;

  (*
     gettimeofday - calls gettimeofday(2) with the same parameters, tv,
                    and, tz.  It returns 0 on success.
  *)

  gettimeofday
  PROCEDURE gettimeofday (tv: timeval; tz: timezone) : INTEGER ;

  (*
     settimeofday - calls settimeofday(2) with the same parameters, tv,
                    and, tz.  It returns 0 on success.
  *)

  settimeofday
  PROCEDURE settimeofday (tv: timeval; tz: timezone) : INTEGER ;

  (*
     GetFractions - returns the tv_usec field inside the timeval structure
                    as a CARDINAL.
  *)

  GetFractions
  PROCEDURE GetFractions (tv: timeval) : CARDINAL ;

  (*
     localtime_r - returns the tm parameter, m, after it has been assigned with
                   appropriate contents determined by, tv.  Notice that
                   this procedure function expects, timeval, as its first
                   parameter and not a time_t (as expected by the posix
                   equivalent).  This avoids having to expose a time_t
                   system dependant definition.
  *)

  localtime_r
  PROCEDURE localtime_r (tv: timeval; m: tm) : tm ;

  (*
     GetYear - returns the year from the structure, m.
  *)

  GetYear
  PROCEDURE GetYear (m: tm) : CARDINAL ;

  (*
     GetMonth - returns the month from the structure, m.
  *)

  GetMonth
  PROCEDURE GetMonth (m: tm) : CARDINAL ;

  (*
     GetDay - returns the day of the month from the structure, m.
  *)

  GetDay
  PROCEDURE GetDay (m: tm) : CARDINAL ;

  (*
     GetHour - returns the hour of the day from the structure, m.
  *)

  GetHour
  PROCEDURE GetHour (m: tm) : CARDINAL ;

  (*
     GetMinute - returns the minute within the hour from the structure, m.
  *)

  GetMinute
  PROCEDURE GetMinute (m: tm) : CARDINAL ;

  (*
     GetSecond - returns the seconds in the minute from the structure, m.
                 The return value will always be in the range 0..59.
                 A leap minute of value 60 will be truncated to 59.
  *)

  GetSecond
  PROCEDURE GetSecond (m: tm) : CARDINAL ;

  (*
     GetSummerTime - returns a boolean indicating whether summer time is
                     set.
  *)

  GetSummerTime
  PROCEDURE GetSummerTime (tz: timezone) : BOOLEAN ;

  (*
     GetDST - returns the number of minutes west of GMT.
  *)

  GetDST
  PROCEDURE GetDST (tz: timezone) : INTEGER ;

  (*
     SetTimeval - sets the fields in timeval, tv, with:
                  second, minute, hour, day, month, year, fractions.
  *)

  SetTimeval
  PROCEDURE SetTimeval (tv: timeval;
                        second, minute, hour, day,
                        month, year, yday, wday, isdst: CARDINAL) ;

  (*
     SetTimezone - set the timezone field inside timeval, tv.
  *)

  SetTimezone
  PROCEDURE SetTimezone (tv: timeval;
                         zone: CARDINAL; minuteswest: INTEGER) ;

  END wraptime.

.. -

