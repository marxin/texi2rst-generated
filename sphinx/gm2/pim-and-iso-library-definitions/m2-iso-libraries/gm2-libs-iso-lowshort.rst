.. _gm2-libs-iso-lowshort:

gm2-libs-iso/LowShort
^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE LowShort;

    (* Access to underlying properties of the type SHORTREAL *)

  CONST
  radix       (const)
    radix      = __ATTRIBUTE__ __BUILTIN__ (( <SHORTREAL, radix> )) ;      (* ZType *)
  places      (const)
    places     = __ATTRIBUTE__ __BUILTIN__ (( <SHORTREAL, places> )) ;     (* ZType *)
  expoMin     (const)
    expoMin    = __ATTRIBUTE__ __BUILTIN__ (( <SHORTREAL, expoMin> )) ;    (* ZType *)
  expoMax     (const)
    expoMax    = __ATTRIBUTE__ __BUILTIN__ (( <SHORTREAL, expoMax> )) ;    (* ZType *)
  large       (const)
    large      = __ATTRIBUTE__ __BUILTIN__ (( <SHORTREAL, large> )) ;      (* RType *)
  small       (const)
    small      = __ATTRIBUTE__ __BUILTIN__ (( <SHORTREAL, small> )) ;      (* RType *)
  IEC559      (const)
    IEC559     = __ATTRIBUTE__ __BUILTIN__ (( <SHORTREAL, IEC559> )) ;     (* BOOLEAN *)
  LIA1        (const)
    LIA1       = __ATTRIBUTE__ __BUILTIN__ (( <SHORTREAL, LIA1> )) ;       (* BOOLEAN *)
  ISO         (const)
    ISO        = __ATTRIBUTE__ __BUILTIN__ (( <SHORTREAL, ISO> )) ;        (* BOOLEAN *)
  IEEE        (const)
    IEEE       = __ATTRIBUTE__ __BUILTIN__ (( <SHORTREAL, IEEE> )) ;       (* BOOLEAN *)
  rounds      (const)
    rounds     = __ATTRIBUTE__ __BUILTIN__ (( <SHORTREAL, rounds> )) ;     (* BOOLEAN *)
  gUnderflow  (const)
    gUnderflow = __ATTRIBUTE__ __BUILTIN__ (( <SHORTREAL, gUnderflow> )) ; (* BOOLEAN *)
  exception   (const)
    exception  = __ATTRIBUTE__ __BUILTIN__ (( <SHORTREAL, exception> )) ;  (* BOOLEAN *)
  extend      (const)
    extend     = __ATTRIBUTE__ __BUILTIN__ (( <SHORTREAL, extend> )) ;     (* BOOLEAN *)
  nModes      (const)
    nModes     = __ATTRIBUTE__ __BUILTIN__ (( <SHORTREAL, nModes> )) ;     (* ZType *)

  TYPE
  Modes (type)
    Modes = PACKEDSET OF [0 .. nModes-1];

  exponent
  PROCEDURE exponent (x: SHORTREAL): INTEGER;
    (* Returns the exponent value of x *)

  fraction
  PROCEDURE fraction (x: SHORTREAL): SHORTREAL;
    (* Returns the significand (or significant part) of x *)

  sign
  PROCEDURE sign (x: SHORTREAL): SHORTREAL;
    (* Returns the signum of x *)

  succ
  PROCEDURE succ (x: SHORTREAL): SHORTREAL;
    (* Returns the next value of the type SHORTREAL greater than x *)

  ulp
  PROCEDURE ulp (x: SHORTREAL): SHORTREAL;
    (* Returns the value of a unit in the last place of x *)

  pred
  PROCEDURE pred (x: SHORTREAL): SHORTREAL;
    (* Returns the previous value of the type SHORTREAL less than x *)

  intpart
  PROCEDURE intpart (x: SHORTREAL): SHORTREAL;
    (* Returns the integer part of x *)

  fractpart
  PROCEDURE fractpart (x: SHORTREAL): SHORTREAL;
    (* Returns the fractional part of x *)

  scale
  PROCEDURE scale (x: SHORTREAL; n: INTEGER): SHORTREAL;
    (* Returns the value of x * radix ** n *)

  trunc
  PROCEDURE trunc (x: SHORTREAL; n: INTEGER): SHORTREAL;
    (* Returns the value of the first n places of x *)

  round
  PROCEDURE round (x: SHORTREAL; n: INTEGER): SHORTREAL;
    (* Returns the value of x rounded to the first n places *)

  synthesize
  PROCEDURE synthesize (expart: INTEGER; frapart: SHORTREAL): SHORTREAL;
    (* Returns a value of the type SHORTREAL constructed from the given expart and frapart *)

  setMode
  PROCEDURE setMode (m: Modes);
    (* Sets status flags appropriate to the underlying implementation of the type SHORTREAL *)

  currentMode
  PROCEDURE currentMode (): Modes;
    (* Returns the current status flags in the form set by setMode *)

  IsLowException
  PROCEDURE IsLowException (): BOOLEAN;
    (* Returns TRUE if the current coroutine is in the exceptional execution state
       because of the raising of an exception in a routine from this module; otherwise
       returns FALSE.
    *)

  END LowShort.

