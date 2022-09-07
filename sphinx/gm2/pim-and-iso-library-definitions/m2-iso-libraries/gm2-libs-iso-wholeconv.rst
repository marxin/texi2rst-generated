.. _gm2-libs-iso-wholeconv:

gm2-libs-iso/WholeConv
^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE WholeConv;

    (* Low-level whole-number/string conversions *)

  IMPORT
    ConvTypes;

  TYPE
  ConvResults (type)
    ConvResults = ConvTypes.ConvResults;
          (* strAllRight, strOutOfRange, strWrongFormat, strEmpty *)

  ScanInt
  PROCEDURE ScanInt (inputCh: CHAR;
                     VAR chClass: ConvTypes.ScanClass;
                     VAR nextState: ConvTypes.ScanState) ;
    (* Represents the start state of a finite state scanner for signed
       whole numbers - assigns class of inputCh to chClass and a
       procedure representing the next state to nextState.
    *)

  FormatInt
  PROCEDURE FormatInt (str: ARRAY OF CHAR): ConvResults;
    (* Returns the format of the string value for conversion to INTEGER. *)

  ValueInt
  PROCEDURE ValueInt (str: ARRAY OF CHAR): INTEGER;
    (* Returns the value corresponding to the signed whole number string
       value str if str is well-formed; otherwise raises the WholeConv
       exception.
     *)

  LengthInt
  PROCEDURE LengthInt (int: INTEGER): CARDINAL;
    (* Returns the number of characters in the string representation of
       int.
     *)

  ScanCard
  PROCEDURE ScanCard (inputCh: CHAR; VAR chClass: ConvTypes.ScanClass;
                      VAR nextState: ConvTypes.ScanState);
    (* Represents the start state of a finite state scanner for unsigned
       whole numbers - assigns class of inputCh to chClass and a procedure
       representing the next state to nextState.
     *)

  FormatCard
  PROCEDURE FormatCard (str: ARRAY OF CHAR): ConvResults;
    (* Returns the format of the string value for conversion to CARDINAL.
     *)

  ValueCard
  PROCEDURE ValueCard (str: ARRAY OF CHAR): CARDINAL;
    (* Returns the value corresponding to the unsigned whole number string
       value str if str is well-formed; otherwise raises the WholeConv
       exception.
     *)

  LengthCard
  PROCEDURE LengthCard (card: CARDINAL): CARDINAL;
    (* Returns the number of characters in the string representation of
       card.
     *)

  IsWholeConvException
  PROCEDURE IsWholeConvException (): BOOLEAN;
    (* Returns TRUE if the current coroutine is in the exceptional execution
       state because of the raising of an exception in a routine from this
       module; otherwise returns FALSE.
    *)

  END WholeConv.

