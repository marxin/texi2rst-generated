.. _gm2-libs-coroutines-system:

gm2-libs-coroutines/SYSTEM
^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE SYSTEM ;

  (* This module is designed to be used on a native operating system
     rather than an embedded system as it implements the coroutine
     primitives TRANSFER, IOTRANSFER and
     NEWPROCESS through the GNU Pthread library.  *)

  FROM COROUTINES IMPORT PROTECTION ;

  EXPORT QUALIFIED (* the following are built into the compiler: *)
                   LOC, WORD, BYTE, ADDRESS, INTEGER8,
                   INTEGER16, INTEGER32, INTEGER64, CARDINAL8,
                   CARDINAL16, CARDINAL32, CARDINAL64, WORD16,
                   WORD32, WORD64, BITSET8, BITSET16,
                   BITSET32, REAL32, REAL64, REAL128,
                   COMPLEX32, COMPLEX64, COMPLEX128, CSIZE_T,
                   CSSIZE_T,
                   ADR, TSIZE, ROTATE, SHIFT, THROW, TBITSIZE,
                   (* SIZE is exported depending upon -fpim2 and
                      -fpedantic *)
                   (* and the rest are implemented in SYSTEM.mod *)
                   PROCESS, TRANSFER, NEWPROCESS, IOTRANSFER,
                   LISTEN,
                   ListenLoop, TurnInterrupts,
                   (* Internal GM2 compiler functions *)
                   ShiftVal, ShiftLeft, ShiftRight,
                   RotateVal, RotateLeft, RotateRight ;

  TYPE
  PROCESS (type)
     PROCESS  = RECORD
                   context: INTEGER ;
  END (type)
                END ;
  (* all the following types are declared internally to gm2
     LOC ;
     WORD ;
     BYTE ;
     ADDRESS ;
     INTEGER8 ;
     INTEGER16 ;
     INTEGER32 ;
     INTEGER64 ;
     CARDINAL8 ;
     CARDINAL16 ;
     CARDINAL32 ;
     CARDINAL64 ;
     WORD16 ;
     WORD32 ;
     WORD64 ;
     BITSET8 ;
     BITSET16 ;
     BITSET32 ;
     REAL32 ;
     REAL64 ;
     REAL128 ;
     COMPLEX32 ;
     COMPLEX64 ;
     COMPLEX128 ;
     CSIZE_T ;
     CSSIZE_T ;
  *)

  (*
     TRANSFER - save the current volatile environment into, p1.
                Restore the volatile environment from, p2.
  *)

  TRANSFER
  PROCEDURE TRANSFER (VAR p1: PROCESS; p2: PROCESS) ;

  (*
     NEWPROCESS - p is a parameterless procedure, a, is the origin of
                  the workspace used for the process stack and containing
                  the volatile environment of the process.  StackSize, is
                  the maximum size of the stack in bytes which can be used
                  by this process.  new, is the new process.
  *)

  NEWPROCESS
  PROCEDURE NEWPROCESS (p: PROC; a: ADDRESS; StackSize: CARDINAL; VAR new: PROCESS) ;

  (*
     IOTRANSFER - saves the current volatile environment into, First,
                  and restores volatile environment, Second.
                  When an interrupt, InterruptNo, is encountered then
                  the reverse takes place. (The then current volatile
                  environment is shelved onto Second and First is resumed).

                  NOTE: that upon interrupt the Second might not be the
                        same process as that before the original call to
                        IOTRANSFER.
  *)

  IOTRANSFER
  PROCEDURE IOTRANSFER (VAR First, Second: PROCESS; InterruptNo: CARDINAL) ;

  (*
     LISTEN - briefly listen for any interrupts.
  *)

  LISTEN
  PROCEDURE LISTEN ;

  (*
     ListenLoop - should be called instead of users writing:

                  LOOP
                     LISTEN
                  END

                  It performs the same function but yields
                  control back to the underlying operating system
                  via a call to pth_select.
                  It also checks for deadlock.
                  This function returns when an interrupt occurs ie
                  a file descriptor becomes ready or a time event expires.
                  See the module RTint.
  *)

  ListenLoop
  PROCEDURE ListenLoop ;

  (*
     TurnInterrupts - switches processor interrupts to the protection
                      level, to.  It returns the old value.
  *)

  TurnInterrupts
  PROCEDURE TurnInterrupts (to: PROTECTION) : PROTECTION ;

  (*
     all the functions below are declared internally to gm2
     ====================================================

  ADR
  PROCEDURE ADR (VAR v: <anytype>): ADDRESS;
    (* Returns the address of variable v. *)

  SIZE
  PROCEDURE SIZE (v: <type>) : ZType;
    (* Returns the number of BYTES used to store a v of
       any specified <type>.  Only available if -fpim2 is used.
    *)

  TSIZE
  PROCEDURE TSIZE (<type>) : CARDINAL;
    (* Returns the number of BYTES used to store a value of the
       specified <type>.
    *)

  ROTATE
  PROCEDURE ROTATE (val: <a set type>;
                    num: INTEGER): <type of first parameter>;
    (* Returns a bit sequence obtained from val by rotating up or down
       (left or right) by the absolute value of num.  The direction is
       down if the sign of num is negative, otherwise the direction is up.
    *)

  SHIFT
  PROCEDURE SHIFT (val: <a set type>;
                   num: INTEGER): <type of first parameter>;
    (* Returns a bit sequence obtained from val by shifting up or down
       (left or right) by the absolute value of num, introducing
       zeros as necessary.  The direction is down if the sign of
       num is negative, otherwise the direction is up.
    *)

  THROW
  PROCEDURE THROW (i: INTEGER) ;
    (*
       THROW is a GNU extension and was not part of the PIM or ISO
       standards.  It throws an exception which will be caught by the EXCEPT
       block (assuming it exists).  This is a compiler builtin function which
       interfaces to the GCC exception handling runtime system.
       GCC uses the term throw, hence the naming distinction between
       the GCC builtin and the Modula-2 runtime library procedure Raise.
       The later library procedure Raise will call SYSTEM.THROW after
       performing various housekeeping activities.
    *)

  TBITSIZE
  PROCEDURE TBITSIZE (<type>) : CARDINAL ;
    (* Returns the minimum number of bits necessary to represent
       <type>.  This procedure function is only useful for determining
       the number of bits used for any type field within a packed RECORD.
       It is not particularly useful elsewhere since <type> might be
       optimized for speed, for example a BOOLEAN could occupy a WORD.
    *)
  *)

  (* The following procedures are invoked by GNU Modula-2 to
     shift non word sized set types. They are not strictly part
     of the core PIM Modula-2, however they are used by
     GNU Modula-2 to implement the SHIFT procedure defined above,
     which are in turn used by the Logitech compatible libraries.

     Users will access these procedures by using the procedure
     SHIFT above and GNU Modula-2 will map SHIFT onto one of
     the following procedures.
  *)

  (*
     ShiftVal - is a runtime procedure whose job is to implement
                the SHIFT procedure of ISO SYSTEM. GNU Modula-2 will
                inline a SHIFT of a single WORD sized set and will only
                call this routine for larger sets.
  *)

  ShiftVal
  PROCEDURE ShiftVal (VAR s, d: ARRAY OF BITSET;
                      SetSizeInBits: CARDINAL;
                      ShiftCount: INTEGER) ;

  (*
     ShiftLeft - performs the shift left for a multi word set.
                 This procedure might be called by the back end of
                 GNU Modula-2 depending whether amount is known at compile
                 time.
  *)

  ShiftLeft
  PROCEDURE ShiftLeft (VAR s, d: ARRAY OF BITSET;
                       SetSizeInBits: CARDINAL;
                       ShiftCount: CARDINAL) ;

  (*
     ShiftRight - performs the shift left for a multi word set.
                  This procedure might be called by the back end of
                  GNU Modula-2 depending whether amount is known at compile
                  time.
  *)

  ShiftRight
  PROCEDURE ShiftRight (VAR s, d: ARRAY OF BITSET;
                       SetSizeInBits: CARDINAL;
                       ShiftCount: CARDINAL) ;

  (*
     RotateVal - is a runtime procedure whose job is to implement
                 the ROTATE procedure of ISO SYSTEM. GNU Modula-2 will
                 inline a ROTATE of a single WORD (or less)
                 sized set and will only call this routine for larger sets.
  *)

  RotateVal
  PROCEDURE RotateVal (VAR s, d: ARRAY OF BITSET;
                       SetSizeInBits: CARDINAL;
                       RotateCount: INTEGER) ;

  (*
     RotateLeft - performs the rotate left for a multi word set.
                  This procedure might be called by the back end of
                  GNU Modula-2 depending whether amount is known at compile
                  time.
  *)

  RotateLeft
  PROCEDURE RotateLeft (VAR s, d: ARRAY OF BITSET;
                        SetSizeInBits: CARDINAL;
                        RotateCount: CARDINAL) ;

  (*
     RotateRight - performs the rotate right for a multi word set.
                   This procedure might be called by the back end of
                   GNU Modula-2 depending whether amount is known at compile
                   time.
  *)

  RotateRight
  PROCEDURE RotateRight (VAR s, d: ARRAY OF BITSET;
                         SetSizeInBits: CARDINAL;
                         RotateCount: CARDINAL) ;

  END SYSTEM.

