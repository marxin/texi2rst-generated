.. _gm2-libs-iso-rtco:

gm2-libs-iso/RTco
^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE RTco ;

  FROM SYSTEM IMPORT ADDRESS ;

  (* init initializes the module and allows the application to lazily invoke threads.  *)

  init
  PROCEDURE init () : INTEGER ;

  initThread
  PROCEDURE initThread (p: PROC; stackSize: CARDINAL; interruptLevel: CARDINAL) : INTEGER ;

  initSemaphore
  PROCEDURE initSemaphore (value: CARDINAL) : INTEGER ;

  wait
  PROCEDURE wait (semaphore: INTEGER) ;

  signal
  PROCEDURE signal (semaphore: INTEGER) ;

  transfer
  PROCEDURE transfer (VAR p1: INTEGER; p2: INTEGER) ;

  waitThread
  PROCEDURE waitThread (tid: INTEGER) ;

  signalThread
  PROCEDURE signalThread (tid: INTEGER) ;

  currentThread
  PROCEDURE currentThread () : INTEGER ;

  (* currentInterruptLevel returns the interrupt level of the current thread.  *)

  currentInterruptLevel
  PROCEDURE currentInterruptLevel () : CARDINAL ;

  (* turninterrupts returns the old interrupt level and assigns the interrupt level
     to newLevel.  *)

  turnInterrupts
  PROCEDURE turnInterrupts (newLevel: CARDINAL) : CARDINAL ;

  (*
     select access to the select system call which will be thread safe.
     This is typically called from the idle process to wait for an interrupt.
  *)

  select
  PROCEDURE select (p1: INTEGER;
                    p2: ADDRESS;
                    p3: ADDRESS;
                    p4: ADDRESS;
                    p5: ADDRESS) : INTEGER ;

  END RTco.

