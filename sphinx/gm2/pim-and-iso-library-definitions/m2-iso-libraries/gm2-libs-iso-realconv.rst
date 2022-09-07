.. _gm2-libs-iso-realconv:

gm2-libs-iso/RealConv
^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE RealConv;

    (* Low-level REAL/string conversions *)

  IMPORT
    ConvTypes;

  TYPE
    (* strAllRight, strOutOfRange, strWrongFormat, strEmpty *)
  ConvResults (type)
    ConvResults = ConvTypes.ConvResults;

  ScanReal
  PROCEDURE ScanReal (inputCh: CHAR; VAR chClass: ConvTypes.ScanClass;
                      VAR nextState: ConvTypes.ScanState);
    (* Represents the start state of a finite state scanner for real
       numbers - assigns class of inputCh to chClass and a procedure
       representing the next state to nextState.
     *)

  FormatReal
  PROCEDURE FormatReal (str: ARRAY OF CHAR): ConvResults;
    (* Returns the format of the string value for conversion to REAL. *)

  ValueReal
  PROCEDURE ValueReal (str: ARRAY OF CHAR): REAL;
    (* Returns the value corresponding to the real number string value
       str if str is well-formed; otherwise raises the RealConv
       exception.
    *)

  LengthFloatReal
  PROCEDURE LengthFloatReal (real: REAL; sigFigs: CARDINAL): CARDINAL;
    (* Returns the number of characters in the floating-point string
       representation of real with sigFigs significant figures.
    *)

  LengthEngReal
  PROCEDURE LengthEngReal (real: REAL; sigFigs: CARDINAL): CARDINAL;
    (* Returns the number of characters in the floating-point engineering
       string representation of real with sigFigs significant figures.
    *)

  LengthFixedReal
  PROCEDURE LengthFixedReal (real: REAL; place: INTEGER): CARDINAL;
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

  END RealConv.

