.. _gm2-libs-pim-conversions:

gm2-libs-pim/Conversions
^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE Conversions ;

  EXPORT QUALIFIED ConvertOctal, ConvertHex, ConvertCardinal,
                   ConvertInteger, ConvertLongInt, ConvertShortInt ;

  (*
     ConvertOctal - converts a CARDINAL, num, into an octal/hex/decimal
                    string and right justifies the string. It adds
                    spaces rather than '0' to pad out the string
                    to len characters.

                    If the length of str is < num then the number is
                    truncated on the right.
  *)

  ConvertOctal
  PROCEDURE ConvertOctal    (num, len: CARDINAL; VAR str: ARRAY OF CHAR) ;
  ConvertHex
  PROCEDURE ConvertHex      (num, len: CARDINAL; VAR str: ARRAY OF CHAR) ;
  ConvertCardinal
  PROCEDURE ConvertCardinal (num, len: CARDINAL; VAR str: ARRAY OF CHAR) ;

  (*
     The INTEGER counterparts will add a '-' if, num, is <0
  *)

  ConvertInteger
  PROCEDURE ConvertInteger  (num: INTEGER; len: CARDINAL; VAR str: ARRAY OF CHAR) ;
  ConvertLongInt
  PROCEDURE ConvertLongInt  (num: LONGINT; len: CARDINAL; VAR str: ARRAY OF CHAR) ;
  ConvertShortInt
  PROCEDURE ConvertShortInt (num: SHORTINT; len: CARDINAL; VAR str: ARRAY OF CHAR) ;

  END Conversions.

