.. _gm2-libs-iso-lowreal:

gm2-libs-iso/LowReal
^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE LowReal;

    (* Access to underlying properties of the type REAL *)

  CONST
  radix       (const)
    radix      = __ATTRIBUTE__ __BUILTIN__ (( <REAL, radix> )) ;      (* ZType *)
  places      (const)
    places     = __ATTRIBUTE__ __BUILTIN__ (( <REAL, places> )) ;     (* ZType *)
  expoMin     (const)
    expoMin    = __ATTRIBUTE__ __BUILTIN__ (( <REAL, expoMin> )) ;    (* ZType *)
  expoMax     (const)
    expoMax    = __ATTRIBUTE__ __BUILTIN__ (( <REAL, expoMax> )) ;    (* ZType *)
  large       (const)
    large      = __ATTRIBUTE__ __BUILTIN__ (( <REAL, large> )) ;      (* RType *)
  small       (const)
    small      = __ATTRIBUTE__ __BUILTIN__ (( <REAL, small> )) ;      (* RType *)
  IEC559      (const)
    IEC559     = __ATTRIBUTE__ __BUILTIN__ (( <REAL, IEC559> )) ;     (* BOOLEAN *)
  LIA1        (const)
    LIA1       = __ATTRIBUTE__ __BUILTIN__ (( <REAL, LIA1> )) ;       (* BOOLEAN *)
  ISO         (const)
    ISO        = __ATTRIBUTE__ __BUILTIN__ (( <REAL, ISO> )) ;        (* BOOLEAN *)
  IEEE        (const)
    IEEE       = __ATTRIBUTE__ __BUILTIN__ (( <REAL, IEEE> )) ;       (* BOOLEAN *)
  rounds      (const)
    rounds     = __ATTRIBUTE__ __BUILTIN__ (( <REAL, rounds> )) ;     (* BOOLEAN *)
  gUnderflow  (const)
    gUnderflow = __ATTRIBUTE__ __BUILTIN__ (( <REAL, gUnderflow> )) ; (* BOOLEAN *)
  exception   (const)
    exception  = __ATTRIBUTE__ __BUILTIN__ (( <REAL, exception> )) ;  (* BOOLEAN *)
  extend      (const)
    extend     = __ATTRIBUTE__ __BUILTIN__ (( <REAL, extend> )) ;     (* BOOLEAN *)
  nModes      (const)
    nModes     = __ATTRIBUTE__ __BUILTIN__ (( <REAL, nModes> )) ;     (* ZType *)

  TYPE
  Modes (type)
    Modes = PACKEDSET OF [0..nModes-1];

  exponent
  PROCEDURE exponent (x: REAL): INTEGER;
    (* Returns the exponent value of x *)

  fraction
  PROCEDURE fraction (x: REAL): REAL;
    (* Returns the significand (or significant part) of x *)

  sign
  PROCEDURE sign (x: REAL): REAL;
    (* Returns the signum of x *)

  succ
  PROCEDURE succ (x: REAL): REAL;
    (* Returns the next value of the type REAL greater than x *)

  ulp
  PROCEDURE ulp (x: REAL): REAL;
    (* Returns the value of a unit in the last place of x *)

  pred
  PROCEDURE pred (x: REAL): REAL;
    (* Returns the previous value of the type REAL less than x *)

  intpart
  PROCEDURE intpart (x: REAL): REAL;
    (* Returns the integer part of x *)

  fractpart
  PROCEDURE fractpart (x: REAL): REAL;
    (* Returns the fractional part of x *)

  scale
  PROCEDURE scale (x: REAL; n: INTEGER): REAL;
    (* Returns the value of x * radix ** n *)

  trunc
  PROCEDURE trunc (x: REAL; n: INTEGER): REAL;
    (* Returns the value of the first n places of x *)

  round
  PROCEDURE round (x: REAL; n: INTEGER): REAL;
    (* Returns the value of x rounded to the first n places *)

  synthesize
  PROCEDURE synthesize (expart: INTEGER; frapart: REAL): REAL;
    (* Returns a value of the type REAL constructed from the given expart and frapart *)

  setMode
  PROCEDURE setMode (m: Modes);
    (* Sets status flags appropriate to the underlying implementation of the type REAL *)

  currentMode
  PROCEDURE currentMode (): Modes;
    (* Returns the current status flags in the form set by setMode *)

  IsLowException
  PROCEDURE IsLowException (): BOOLEAN;
    (* Returns TRUE if the current coroutine is in the exceptional execution state
       because of the raising of an exception in a routine from this module; otherwise
       returns FALSE.
    *)

  END LowReal.

