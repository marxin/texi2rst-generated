.. _gm2-libs-iso-convtypes:

gm2-libs-iso/ConvTypes
^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE ConvTypes;

    (* Common types used in the string conversion modules *)

  TYPE
  ConvResults (type)
    ConvResults =     (* Values of this type are used to express the format of a string *)
    (
      strAllRight,    (* the string format is correct for the corresponding conversion *)
      strOutOfRange,  (* the string is well-formed but the value cannot be represented *)
      strWrongFormat, (* the string is in the wrong format for the conversion *)
      strEmpty        (* the given string is empty *)
    );

  ScanClass (type)
    ScanClass =  (* Values of this type are used to classify input to finite state scanners *)
    (
      padding,   (* a leading or padding character at this point in the scan - ignore it *)
      valid,     (* a valid character at this point in the scan - accept it *)
      invalid,   (* an invalid character at this point in the scan - reject it *)
      terminator (* a terminating character at this point in the scan (not part of token) *)
    );

  ScanState (type)
    ScanState =  (* The type of lexical scanning control procedures *)
      PROCEDURE (CHAR, VAR ScanClass, VAR ScanState);

  END ConvTypes.

