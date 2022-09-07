.. _gm2-libs-iso-longconv:

gm2-libs-iso/LongConv
^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE LongConv;

    (* Low-level LONGREAL/string conversions *)

  IMPORT
    ConvTypes;

  TYPE
  ConvResults (type)
    ConvResults = ConvTypes.ConvResults; (* strAllRight, strOutOfRange,
                                            strWrongFormat, strEmpty *)

  ScanReal
  PROCEDURE ScanReal (inputCh: CHAR; VAR chClass: ConvTypes.ScanClass;
                      VAR nextState: ConvTypes.ScanState);
    (* Represents the start state of a finite state scanner for real
       numbers - assigns class of inputCh to chClass and a procedure
       representing the next state to nextState.
    *)

  FormatReal
  PROCEDURE FormatReal (str: ARRAY OF CHAR): ConvResults;
    (* Returns the format of the string value for conversion to LONGREAL. *)

  ValueReal
  PROCEDURE ValueReal (str: ARRAY OF CHAR): LONGREAL;
    (* Returns the value corresponding to the real number string value
       str if str is well-formed; otherwise raises the LongConv exception.
    *)

  LengthFloatReal
  PROCEDURE LengthFloatReal (real: LONGREAL; sigFigs: CARDINAL): CARDINAL;
    (* Returns the number of characters in the floating-point string
       representation of real with sigFigs significant figures.
    *)

  LengthEngReal
  PROCEDURE LengthEngReal (real: LONGREAL; sigFigs: CARDINAL): CARDINAL;
    (* Returns the number of characters in the floating-point engineering
       string representation of real with sigFigs significant figures.
    *)

  LengthFixedReal
  PROCEDURE LengthFixedReal (real: LONGREAL; place: INTEGER): CARDINAL;
    (* Returns the number of characters in the fixed-point string
       representation of real rounded to the given place relative to the
       decimal point.
    *)

  IsRConvException
  PROCEDURE IsRConvException (): BOOLEAN;
    (* Returns TRUE if the current coroutine is in the exceptional
       execution state because of the raising of an exception in a
       routine from this module; otherwise returns FALSE.
    *)

  END LongConv.

