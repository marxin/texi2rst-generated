.. _gm2-libs-gdbif:

gm2-libs/gdbif
^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE gdbif ;

  (*  Provides interactive connectivity with gdb useful for debugging
      Modula-2 shared libraries.  *)

  EXPORT UNQUALIFIED sleepSpin, finishSpin, connectSpin ;

  (*
     finishSpin - sets boolean mustWait to FALSE.
  *)

  finishSpin
  PROCEDURE finishSpin ;

  (*
     sleepSpin - waits for the boolean variable mustWait to become FALSE.
                 It sleeps for a second between each test of the variable.
  *)

  sleepSpin
  PROCEDURE sleepSpin ;

  (*
     connectSpin - breakpoint placeholder.  Its only purpose is to allow users
                   to set a breakpoint.  This procedure is called once
                   sleepSpin is released from its spin (via a call from
                   finishSpin).
  *)

  connectSpin
  PROCEDURE connectSpin ;

  END gdbif.

