.. _gm2-libs-iso-sioresult:

gm2-libs-iso/SIOResult
^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE SIOResult;

    (* Read results for the default input channel *)

  IMPORT IOConsts;

  TYPE
  ReadResults (type)
    ReadResults = IOConsts.ReadResults;

    (*
  ReadResults (type)
      ReadResults =   (* This type is used to classify the result of an input operation *)
      (
        notKnown,     (* no read result is set *)
        allRight,     (* data is as expected or as required *)
        outOfRange,   (* data cannot be represented *)
        wrongFormat,  (* data not in expected format *)
        endOfLine,    (* end of line seen before expected data *)
        endOfInput    (* end of input seen before expected data *)
      );
    *)

  ReadResult
  PROCEDURE ReadResult (): ReadResults;
    (* Returns the result for the last read operation on the default input channel. *)

  END SIOResult.

