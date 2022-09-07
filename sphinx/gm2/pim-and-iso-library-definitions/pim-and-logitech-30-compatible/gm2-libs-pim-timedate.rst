.. _gm2-libs-pim-timedate:

gm2-libs-pim/TimeDate
^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE TimeDate ;

  (*
     Legacy compatibility - you are advised to use cleaner
     designed modules based on 'man 3 strtime'
     and friends for new projects as the day value here is ugly.
     [it was mapped onto MSDOS pre 2000].
  *)

  EXPORT QUALIFIED Time, GetTime, SetTime, CompareTime, TimeToZero,
                   TimeToString ;

  TYPE
  (*
     day holds:  bits 0..4 = day of month (1..31)
                      5..8 = month of year (1..12)
                      9..  = year - 1900
     minute holds:    hours * 60 + minutes
     millisec holds:  seconds * 1000 + millisec
                      which is reset to 0 every minute
  *)

     Time = RECORD
               day, minute, millisec: CARDINAL ;
            END ;

  (*
     GetTime - returns the current date and time.
  *)

  GetTime
  PROCEDURE GetTime (VAR curTime: Time) ;

  (*
     SetTime - does nothing, but provides compatibility with
               the Logitech-3.0 library.
  *)

  SetTime
  PROCEDURE SetTime (curTime: Time) ;

  (*
     CompareTime - compare two dates and time which returns:

                   -1  if t1 < t2
                    0  if t1 = t2
                    1  if t1 > t2
  *)

  CompareTime
  PROCEDURE CompareTime (t1, t2: Time) : INTEGER ;

  (*
     TimeToZero - initializes, t, to zero.
  *)

  TimeToZero
  PROCEDURE TimeToZero (VAR t: Time) ;

  (*
     TimeToString - convert time, t, to a string.
                    The string, s, should be at least 19 characters
                    long and the returned string will be

                    yyyy-mm-dd hh:mm:ss
  *)

  TimeToString
  PROCEDURE TimeToString (t: Time; VAR s: ARRAY OF CHAR) ;

  END TimeDate.

.. -

