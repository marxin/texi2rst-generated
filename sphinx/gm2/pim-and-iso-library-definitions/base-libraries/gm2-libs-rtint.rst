.. _gm2-libs-rtint:

gm2-libs/RTint
^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE RTint ;

  (* Provides users of the COROUTINES library with the
     ability to create interrupt sources based on
     file descriptors and timeouts.  *)

  FROM SYSTEM IMPORT ADDRESS ;

  TYPE
  DispatchVector (type)
     DispatchVector = PROCEDURE (CARDINAL, CARDINAL, ADDRESS) ;

  (*
     InitInputVector - returns an interrupt vector which is associated
                       with the file descriptor, fd.
  *)

  InitInputVector
  PROCEDURE InitInputVector (fd: INTEGER; pri: CARDINAL) : CARDINAL ;

  (*
     InitOutputVector - returns an interrupt vector which is associated
                        with the file descriptor, fd.
  *)

  InitOutputVector
  PROCEDURE InitOutputVector (fd: INTEGER; pri: CARDINAL) : CARDINAL ;

  (*
     InitTimeVector - returns an interrupt vector associated with
                      the relative time.
  *)

  InitTimeVector
  PROCEDURE InitTimeVector (micro, secs: CARDINAL; pri: CARDINAL) : CARDINAL ;

  (*
     ReArmTimeVector - reprimes the vector, vec, to deliver an interrupt
                       at the new relative time.
  *)

  ReArmTimeVector
  PROCEDURE ReArmTimeVector (vec: CARDINAL; micro, secs: CARDINAL) ;

  (*
     GetTimeVector - assigns, micro, and, secs, with the remaining
                     time before this interrupt will expire.
                     This value is only updated when a Listen
                     occurs.
  *)

  GetTimeVector
  PROCEDURE GetTimeVector (vec: CARDINAL; VAR micro, secs: CARDINAL) ;

  (*
     AttachVector - adds the pointer, p, to be associated with the interrupt
                    vector. It returns the previous value attached to this
                    vector.
  *)

  AttachVector
  PROCEDURE AttachVector (vec: CARDINAL; p: ADDRESS) : ADDRESS ;

  (*
     IncludeVector - includes, vec, into the dispatcher list of
                     possible interrupt causes.
  *)

  IncludeVector
  PROCEDURE IncludeVector (vec: CARDINAL) ;

  (*
     ExcludeVector - excludes, vec, from the dispatcher list of
                     possible interrupt causes.
  *)

  ExcludeVector
  PROCEDURE ExcludeVector (vec: CARDINAL) ;

  (*
     Listen - will either block indefinitely (until an interrupt)
              or alteratively will test to see whether any interrupts
              are pending.
              If a pending interrupt was found then, call, is called
              and then this procedure returns.
              It only listens for interrupts > pri.
  *)

  Listen
  PROCEDURE Listen (untilInterrupt: BOOLEAN;
                    call: DispatchVector;
                    pri: CARDINAL) ;

  (*
     Init - allows the user to force the initialize order.
  *)

  Init
  PROCEDURE Init ;

  END RTint.

