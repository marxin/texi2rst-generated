.. _gm2-libs-dtoa:

gm2-libs/dtoa
^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE dtoa ;

  FROM SYSTEM IMPORT ADDRESS ;

  TYPE
  Mode (type)
     Mode = (maxsignificant, decimaldigits) ;

  (*
     strtod - returns a REAL given a string, s.  It will set
              error to TRUE if the number is too large.
  *)

  strtod
  PROCEDURE strtod (s: ADDRESS; VAR error: BOOLEAN) : REAL ;

  (*
     dtoa - converts a REAL, d, into a string.  The address of the
            string is returned.
            mode       indicates the type of conversion required.
            ndigits    determines the number of digits according to mode.
            decpt      the position of the decimal point.
            sign       does the string have a sign?
  *)

  dtoa
  PROCEDURE dtoa (d        : REAL;
                  mode     : Mode;
                  ndigits  : INTEGER;
  	        VAR decpt: INTEGER;
  	        VAR sign : BOOLEAN) : ADDRESS ;

  END dtoa.

