.. _gm2-libs-ldtoa:

gm2-libs/ldtoa
^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE ldtoa ;

  FROM SYSTEM IMPORT ADDRESS ;

  TYPE
  Mode (type)
     Mode = (maxsignificant, decimaldigits) ;

  (*
     strtold - returns a LONGREAL given a C string, s.  It will set
               error to TRUE if the number is too large or badly formed.
  *)

  strtold
  PROCEDURE strtold (s: ADDRESS; VAR error: BOOLEAN) : LONGREAL ;

  (*
     ldtoa - converts a LONGREAL, d, into a string.  The address of the
             string is returned.
             mode       indicates the type of conversion required.
             ndigits    determines the number of digits according to mode.
             decpt      the position of the decimal point.
             sign       does the string have a sign?
  *)

  ldtoa
  PROCEDURE ldtoa (d        : LONGREAL;
                   mode     : Mode;
                   ndigits  : INTEGER;
                   VAR decpt: INTEGER;
                   VAR sign : BOOLEAN) : ADDRESS ;

  END ldtoa.

