.. _gm2-libs-timestring:

gm2-libs/TimeString
^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE TimeString ;

  EXPORT QUALIFIED GetTimeString ;

  (*
     GetTimeString - places the time in ascii format into array, a.

  *)

  GetTimeString
  PROCEDURE GetTimeString (VAR a: ARRAY OF CHAR) ;

  END TimeString.

