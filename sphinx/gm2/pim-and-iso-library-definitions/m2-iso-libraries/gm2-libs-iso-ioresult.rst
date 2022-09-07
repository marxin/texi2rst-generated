.. _gm2-libs-iso-ioresult:

gm2-libs-iso/IOResult
^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE IOResult;

    (* Read results for specified channels *)

  IMPORT IOConsts, IOChan;

  TYPE
  ReadResults (type)
    ReadResults = IOConsts.ReadResults;

    (*
  ReadResults (type)
      ReadResults =  (* This type is used to classify the result of an input operation *)
      (
        notKnown,    (* no read result is set *)
        allRight,    (* data is as expected or as required *)
        outOfRange,  (* data cannot be represented *)
        wrongFormat, (* data not in expected format *)
        endOfLine,   (* end of line seen before expected data *)
        endOfInput   (* end of input seen before expected data *)
      );
    *)

  ReadResult
  PROCEDURE ReadResult (cid: IOChan.ChanId): ReadResults;
    (* Returns the result for the last read operation on the channel cid. *)

  END IOResult.

