.. _gm2-libs-system:

gm2-libs/SYSTEM
^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE SYSTEM ;

  EXPORT QUALIFIED BITSPERBYTE, BYTESPERWORD,
                   LOC, WORD, BYTE, ADDRESS, INTEGER8,
                   INTEGER16, INTEGER32, INTEGER64, CARDINAL8,
                   CARDINAL16, CARDINAL32, CARDINAL64, WORD16,
                   WORD32, WORD64, BITSET8, BITSET16,
                   BITSET32, REAL32, REAL64, REAL128,
                   COMPLEX32, COMPLEX64, COMPLEX128, CSIZE_T,
                   CSSIZE_T,
                   ADR, TSIZE, ROTATE, SHIFT, THROW, TBITSIZE ;
                   (* SIZE is also exported if -fpim2 is used *)

  CONST
  BITSPERBYTE    (const)
    BITSPERBYTE   = __ATTRIBUTE__ __BUILTIN__ ((BITS_PER_UNIT)) ;
  BYTESPERWORD   (const)
    BYTESPERWORD  = __ATTRIBUTE__ __BUILTIN__ ((UNITS_PER_WORD)) ;

  (* all the following types are declared internally to gm2
  TYPE
  LOC (type)
     LOC ;
  WORD (type)
     WORD ;
  BYTE (type)
     BYTE ;
  ADDRESS (type)
     ADDRESS ;
  INTEGER8 (type)
     INTEGER8 ;
  INTEGER16 (type)
     INTEGER16 ;
  INTEGER32 (type)
     INTEGER32 ;
  INTEGER64 (type)
     INTEGER64 ;
  CARDINAL8 (type)
     CARDINAL8 ;
  CARDINAL16 (type)
     CARDINAL16 ;
  CARDINAL32 (type)
     CARDINAL32 ;
  CARDINAL64 (type)
     CARDINAL64 ;
  WORD16 (type)
     WORD16 ;
  WORD32 (type)
     WORD32 ;
  WORD64 (type)
     WORD64 ;
  BITSET8 (type)
     BITSET8 ;
  BITSET16 (type)
     BITSET16 ;
  BITSET32 (type)
     BITSET32 ;
  REAL32 (type)
     REAL32 ;
  REAL64 (type)
     REAL64 ;
  REAL128 (type)
     REAL128 ;
  COMPLEX32 (type)
     COMPLEX32 ;
  COMPLEX64 (type)
     COMPLEX64 ;
  COMPLEX128 (type)
     COMPLEX128 ;
  CSIZE_T (type)
     CSIZE_T ;
  CSSIZE_T (type)
     CSSIZE_T ;
  *)

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
    (* Returns a bit sequence obtained from val by rotating up/right
       or down/right by the absolute value of num.  The direction is
       down/right if the sign of num is negative, otherwise the direction
       is up/left.
    *)

  SHIFT
  PROCEDURE SHIFT (val: <a set type>;
                   num: INTEGER): <type of first parameter>;
    (* Returns a bit sequence obtained from val by shifting up/left
       or down/right by the absolute value of num, introducing
       zeros as necessary.  The direction is down/right if the sign of
       num is negative, otherwise the direction is up/left.
    *)

  THROW
  PROCEDURE THROW (i: INTEGER) ;
    (*
       THROW is a GNU extension and was not part of the PIM or ISO
       standards.  It throws an exception which will be caught by the
       EXCEPT block (assuming it exists).  This is a compiler builtin
       function which interfaces to the GCC exception handling runtime
       system.
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
                 GNU Modula-2 depending whether amount is known at
                 compile time.
  *)

  ShiftLeft
  PROCEDURE ShiftLeft (VAR s, d: ARRAY OF BITSET;
                       SetSizeInBits: CARDINAL;
                       ShiftCount: CARDINAL) ;

  (*
     ShiftRight - performs the shift left for a multi word set.
                  This procedure might be called by the back end of
                  GNU Modula-2 depending whether amount is known at
                  compile time.
  *)

  ShiftRight
  PROCEDURE ShiftRight (VAR s, d: ARRAY OF BITSET;
                        SetSizeInBits: CARDINAL;
                        ShiftCount: CARDINAL) ;

  (*
     RotateVal - is a runtime procedure whose job is to implement
                 the ROTATE procedure of ISO SYSTEM. GNU Modula-2 will
                 inline a ROTATE of a single WORD (or less)
                 sized set and will only call this routine for larger
                 sets.
  *)

  RotateVal
  PROCEDURE RotateVal (VAR s, d: ARRAY OF BITSET;
                       SetSizeInBits: CARDINAL;
                       RotateCount: INTEGER) ;

  (*
     RotateLeft - performs the rotate left for a multi word set.
                  This procedure might be called by the back end of
                  GNU Modula-2 depending whether amount is known at
                  compile time.
  *)

  RotateLeft
  PROCEDURE RotateLeft (VAR s, d: ARRAY OF BITSET;
                        SetSizeInBits: CARDINAL;
                        RotateCount: CARDINAL) ;

  (*
     RotateRight - performs the rotate right for a multi word set.
                   This procedure might be called by the back end of
                   GNU Modula-2 depending whether amount is known at
                   compile time.
  *)

  RotateRight
  PROCEDURE RotateRight (VAR s, d: ARRAY OF BITSET;
                         SetSizeInBits: CARDINAL;
                         RotateCount: CARDINAL) ;

  END SYSTEM.

