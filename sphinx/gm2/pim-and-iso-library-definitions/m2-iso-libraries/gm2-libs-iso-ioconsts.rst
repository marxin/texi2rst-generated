.. _gm2-libs-iso-ioconsts:

gm2-libs-iso/IOConsts
^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE IOConsts;

    (* Types and constants for input/output modules *)

  TYPE
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

  END IOConsts.

