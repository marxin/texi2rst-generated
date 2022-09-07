.. _gm2-libs-selective:

gm2-libs/Selective
^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE Selective ;

  FROM SYSTEM IMPORT ADDRESS ;

  EXPORT QUALIFIED SetOfFd, Timeval,
                   InitSet, KillSet, InitTime, KillTime,
                   GetTime, SetTime,
                   FdZero, FdSet, FdClr, FdIsSet, Select,
                   MaxFdsPlusOne, WriteCharRaw, ReadCharRaw,
                   GetTimeOfDay ;

  TYPE
  SetOfFd (type)
     SetOfFd = ADDRESS ;    (* Hidden type in Selective.c *)
  Timeval (type)
     Timeval = ADDRESS ;    (* Hidden type in Selective.c *)

  Select
  PROCEDURE Select (nooffds: CARDINAL;
                    readfds, writefds, exceptfds: SetOfFd;
                    timeout: Timeval) : INTEGER ;

  InitTime
  PROCEDURE InitTime (sec, usec: CARDINAL) : Timeval ;
  KillTime
  PROCEDURE KillTime (t: Timeval) : Timeval ;
  GetTime
  PROCEDURE GetTime (t: Timeval; VAR sec, usec: CARDINAL) ;
  SetTime
  PROCEDURE SetTime (t: Timeval; sec, usec: CARDINAL) ;
  InitSet
  PROCEDURE InitSet () : SetOfFd ;
  KillSet
  PROCEDURE KillSet (s: SetOfFd) : SetOfFd ;
  FdZero
  PROCEDURE FdZero (s: SetOfFd) ;
  FdSet
  PROCEDURE FdSet (fd: INTEGER; s: SetOfFd) ;
  FdClr
  PROCEDURE FdClr (fd: INTEGER; s: SetOfFd) ;
  FdIsSet
  PROCEDURE FdIsSet (fd: INTEGER; s: SetOfFd) : BOOLEAN ;
  MaxFdsPlusOne
  PROCEDURE MaxFdsPlusOne (a, b: INTEGER) : INTEGER ;

  (* you must use the raw routines with select - not the FIO buffered routines *)
  WriteCharRaw
  PROCEDURE WriteCharRaw (fd: INTEGER; ch: CHAR) ;
  ReadCharRaw
  PROCEDURE ReadCharRaw (fd: INTEGER) : CHAR ;

  (*
     GetTimeOfDay - fills in a record, Timeval, filled in with the
                    current system time in seconds and microseconds.
                    It returns zero (see man 3p gettimeofday)
  *)

  GetTimeOfDay
  PROCEDURE GetTimeOfDay (tv: Timeval) : INTEGER ;

  END Selective.

