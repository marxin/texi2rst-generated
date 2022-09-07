.. _gm2-libs-iso-sysclock:

gm2-libs-iso/SysClock
^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE SysClock;

  (* Facilities for accessing a system clock that records the date
     and time of day *)

  CONST
  maxSecondParts  (const)
    maxSecondParts = 1000000 ;

  TYPE
  Month (type)
    Month    = [1 .. 12];
  Day (type)
    Day      = [1 .. 31];
  Hour (type)
    Hour     = [0 .. 23];
  Min (type)
    Min      = [0 .. 59];
  Sec (type)
    Sec      = [0 .. 59];
  Fraction (type)
    Fraction = [0 .. maxSecondParts];
  UTCDiff (type)
    UTCDiff  = [-780 .. 720];
  DateTime (type)
    DateTime =
      RECORD
        year:      CARDINAL;
        month:     Month;
        day:       Day;
        hour:      Hour;
        minute:    Min;
        second:    Sec;
        fractions: Fraction;      (* parts of a second *)
        zone:      UTCDiff;       (* Time zone differential
                                     factor which is the number
                                     of minutes to add to local
                                     time to obtain UTC. *)
        summerTimeFlag: BOOLEAN;  (* Interpretation of flag
                                     depends on local usage. *)
      END;

  CanGetClock
  PROCEDURE CanGetClock(): BOOLEAN;
  (* Tests if the clock can be read *)

  CanSetClock
  PROCEDURE CanSetClock(): BOOLEAN;
  (* Tests if the clock can be set *)

  IsValidDateTime
  PROCEDURE IsValidDateTime(userData: DateTime): BOOLEAN;
  (* Tests if the value of userData is a valid *)

  GetClock
  PROCEDURE GetClock(VAR userData: DateTime);
  (* Assigns local date and time of the day to userData *)

  SetClock
  PROCEDURE SetClock(userData: DateTime);
  (* Sets the system time clock to the given local date and
     time *)

  END SysClock.

