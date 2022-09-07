.. _gm2-libs-iso-lowlong:

gm2-libs-iso/LowLong
^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE LowLong;

    (* Access to underlying properties of the type LONGREAL *)

  CONST
  radix       (const)
    radix      = __ATTRIBUTE__ __BUILTIN__ (( <LONGREAL, radix> )) ;      (* ZType *)
  places      (const)
    places     = __ATTRIBUTE__ __BUILTIN__ (( <LONGREAL, places> )) ;     (* ZType *)
  expoMin     (const)
    expoMin    = __ATTRIBUTE__ __BUILTIN__ (( <LONGREAL, expoMin> )) ;    (* ZType *)
  expoMax     (const)
    expoMax    = __ATTRIBUTE__ __BUILTIN__ (( <LONGREAL, expoMax> )) ;    (* ZType *)
  large       (const)
    large      = __ATTRIBUTE__ __BUILTIN__ (( <LONGREAL, large> )) ;      (* RType *)
  small       (const)
    small      = __ATTRIBUTE__ __BUILTIN__ (( <LONGREAL, small> )) ;      (* RType *)
  IEC559      (const)
    IEC559     = __ATTRIBUTE__ __BUILTIN__ (( <LONGREAL, IEC559> )) ;     (* BOOLEAN *)
  LIA1        (const)
    LIA1       = __ATTRIBUTE__ __BUILTIN__ (( <LONGREAL, LIA1> )) ;       (* BOOLEAN *)
  ISO         (const)
    ISO        = __ATTRIBUTE__ __BUILTIN__ (( <LONGREAL, ISO> )) ;        (* BOOLEAN *)
  IEEE        (const)
    IEEE       = __ATTRIBUTE__ __BUILTIN__ (( <LONGREAL, IEEE> )) ;       (* BOOLEAN *)
  rounds      (const)
    rounds     = __ATTRIBUTE__ __BUILTIN__ (( <LONGREAL, rounds> )) ;     (* BOOLEAN *)
  gUnderflow  (const)
    gUnderflow = __ATTRIBUTE__ __BUILTIN__ (( <LONGREAL, gUnderflow> )) ; (* BOOLEAN *)
  exception   (const)
    exception  = __ATTRIBUTE__ __BUILTIN__ (( <LONGREAL, exception> )) ;  (* BOOLEAN *)
  extend      (const)
    extend     = __ATTRIBUTE__ __BUILTIN__ (( <LONGREAL, extend> )) ;     (* BOOLEAN *)
  nModes      (const)
    nModes     = __ATTRIBUTE__ __BUILTIN__ (( <LONGREAL, nModes> )) ;     (* ZType *)

  TYPE
  Modes (type)
    Modes = PACKEDSET OF [0 .. nModes-1];

  exponent
  PROCEDURE exponent (x: LONGREAL): INTEGER;
    (* Returns the exponent value of x *)

  fraction
  PROCEDURE fraction (x: LONGREAL): LONGREAL;
    (* Returns the significand (or significant part) of x *)

  sign
  PROCEDURE sign (x: LONGREAL): LONGREAL;
    (* Returns the signum of x *)

  succ
  PROCEDURE succ (x: LONGREAL): LONGREAL;
    (* Returns the next value of the type LONGREAL greater than x *)

  ulp
  PROCEDURE ulp (x: LONGREAL): LONGREAL;
    (* Returns the value of a unit in the last place of x *)

  pred
  PROCEDURE pred (x: LONGREAL): LONGREAL;
    (* Returns the previous value of the type LONGREAL less than x *)

  intpart
  PROCEDURE intpart (x: LONGREAL): LONGREAL;
    (* Returns the integer part of x *)

  fractpart
  PROCEDURE fractpart (x: LONGREAL): LONGREAL;
    (* Returns the fractional part of x *)

  scale
  PROCEDURE scale (x: LONGREAL; n: INTEGER): LONGREAL;
    (* Returns the value of x * radix ** n *)

  trunc
  PROCEDURE trunc (x: LONGREAL; n: INTEGER): LONGREAL;
    (* Returns the value of the first n places of x *)

  round
  PROCEDURE round (x: LONGREAL; n: INTEGER): LONGREAL;
    (* Returns the value of x rounded to the first n places *)

  synthesize
  PROCEDURE synthesize (expart: INTEGER; frapart: LONGREAL): LONGREAL;
    (* Returns a value of the type LONGREAL constructed from the given expart and frapart *)

  setMode
  PROCEDURE setMode (m: Modes);
    (* Sets status flags appropriate to the underlying implementation of the type LONGREAL *)

  currentMode
  PROCEDURE currentMode (): Modes;
    (* Returns the current status flags in the form set by setMode *)

  IsLowException
  PROCEDURE IsLowException (): BOOLEAN;
    (* Returns TRUE if the current coroutine is in the exceptional execution state
       because of the raising of an exception in a routine from this module; otherwise
       returns FALSE.
    *)

  END LowLong.

