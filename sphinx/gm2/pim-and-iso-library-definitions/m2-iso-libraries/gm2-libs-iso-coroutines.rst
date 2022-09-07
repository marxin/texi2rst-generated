.. _gm2-libs-iso-coroutines:

gm2-libs-iso/COROUTINES
^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE COROUTINES;

  (* Facilities for coroutines and the handling of interrupts *)

  IMPORT SYSTEM ;

  CONST
  UnassignedPriority  (const)
    UnassignedPriority = 0 ;

  TYPE
  COROUTINE (type)
    COROUTINE ; (* Values of this type are created dynamically by NEWCOROUTINE
                   and identify the coroutine in subsequent operations *)
  INTERRUPTSOURCE (type)
    INTERRUPTSOURCE = CARDINAL ;
  PROTECTION (type)
    PROTECTION = [UnassignedPriority..7] ;

  NEWCOROUTINE
  PROCEDURE NEWCOROUTINE (procBody: PROC;
                          workspace: SYSTEM.ADDRESS;
                          size: CARDINAL;
                          VAR cr: COROUTINE;
                          [initProtection: PROTECTION = UnassignedPriority]);
    (* Creates a new coroutine whose body is given by procBody, and
       returns the identity of the coroutine in cr. workspace is a
       pointer to the work space allocated to the coroutine; size
       specifies the size of this workspace in terms of SYSTEM.LOC.

       The optarg, initProtection, may contain a single parameter which
       specifies the initial protection level of the coroutine.
    *)

  TRANSFER
  PROCEDURE TRANSFER (VAR from: COROUTINE; to: COROUTINE);
    (* Returns the identity of the calling coroutine in from, and
       transfers control to the coroutine specified by to.
    *)

  IOTRANSFER
  PROCEDURE IOTRANSFER (VAR from: COROUTINE; to: COROUTINE);
    (* Returns the identity of the calling coroutine in from and
       transfers control to the coroutine specified by to.  On
       occurrence of an interrupt, associated with the caller, control
       is transferred back to the caller, and the identity of the
       interrupted coroutine is returned in from.  The calling coroutine
       must be associated with a source of interrupts.
    *)

  ATTACH
  PROCEDURE ATTACH (source: INTERRUPTSOURCE);
    (* Associates the specified source of interrupts with the calling
       coroutine. *)

  DETACH
  PROCEDURE DETACH (source: INTERRUPTSOURCE);
    (* Dissociates the specified source of interrupts from the calling
       coroutine. *)

  IsATTACHED
  PROCEDURE IsATTACHED (source: INTERRUPTSOURCE): BOOLEAN;
    (* Returns TRUE if and only if the specified source of interrupts is
       currently associated with a coroutine; otherwise returns FALSE.
    *)

  HANDLER
  PROCEDURE HANDLER (source: INTERRUPTSOURCE): COROUTINE;
    (* Returns the coroutine, if any, that is associated with the source
       of interrupts. The result is undefined if IsATTACHED(source) =
       FALSE.
    *)

  CURRENT
  PROCEDURE CURRENT (): COROUTINE;
    (* Returns the identity of the calling coroutine. *)

  LISTEN
  PROCEDURE LISTEN (p: PROTECTION);
    (* Momentarily changes the protection of the calling coroutine to
       p. *)

  PROT
  PROCEDURE PROT (): PROTECTION;
    (* Returns the protection of the calling coroutine. *)

  (*
     TurnInterrupts - switches processor interrupts to the protection
                      level, to.  It returns the old value.
  *)

  TurnInterrupts
  PROCEDURE TurnInterrupts (to: PROTECTION) : PROTECTION ;

  (*
     ListenLoop - should be called instead of users writing:

                  LOOP
                     LISTEN
                  END

                  It performs the same function but yields
                  control back to the underlying operating system.
                  It also checks for deadlock.
                  Note that this function does return when an interrupt occurs.
                  (File descriptor becomes ready or time event expires).
  *)

  ListenLoop
  PROCEDURE ListenLoop ;

  END COROUTINES.

