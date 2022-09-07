.. _gm2-libs-pim-realconversions:

gm2-libs-pim/RealConversions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE RealConversions ;

  EXPORT QUALIFIED SetNoOfExponentDigits,
                   RealToString, StringToReal,
                   LongRealToString, StringToLongReal ;

  (*
     SetNoOfExponentDigits - sets the number of exponent digits to be
                             used during future calls of LongRealToString
                             and RealToString providing that the width
                             is sufficient.
                             If this value is set to 0 (the default) then
                             the number digits used is the minimum necessary.
  *)

  SetNoOfExponentDigits
  PROCEDURE SetNoOfExponentDigits (places: CARDINAL) ;

  (*
     RealToString - converts a real, r, into a right justified string, str.
                    The number of digits to the right of the decimal point
                    is given in, digits.  The value, width, represents the
                    maximum number of characters to be used in the string,
                    str.

                    If digits is negative then exponent notation is used
                    whereas if digits is positive then fixed point notation
                    is used.

                    If, r, is less than 0.0 then a '-' preceeds the value,
                    str.  However, if, r, is >= 0.0 a '+' is not added.

                    If the conversion of, r, to a string requires more
                    than, width, characters then the string, str, is set
                    to a nul string and, ok is assigned FALSE.

                    For fixed point notation the minimum width required is
                    ABS(width)+8

                    For exponent notation the minimum width required is
                    ABS(digits)+2+log10(magnitude).

                    if r is a NaN then the string 'nan' is returned formatted and
                    ok will be FALSE.
  *)

  RealToString
  PROCEDURE RealToString (r: REAL; digits, width: INTEGER;
                          VAR str: ARRAY OF CHAR; VAR ok: BOOLEAN) ;

  (*
     LongRealToString - converts a real, r, into a right justified string, str.
                        The number of digits to the right of the decimal point
                        is given in, digits. The value, width, represents the
                        maximum number of characters to be used in the string,
                        str.

                        If digits is negative then exponent notation is used
                        whereas if digits is positive then fixed point notation
                        is used.

                        If, r, is less than 0.0 then a '-' preceeds the value,
                        str. However, if, r, is >= 0.0 a '+' is not added.

                        If the conversion of, r, to a string requires more
                        than, width, characters then the string, str, is set
                        to a nul string and, ok is assigned FALSE.

                        For fixed point notation the minimum width required is
                        ABS(width)+8

                        For exponent notation the minimum width required is
                        ABS(digits)+2+log10(magnitude).

                        Examples:
                        RealToString(100.0, 10, 10, a, ok)       ->  '100.000000'
                        RealToString(100.0, -5, 12, a, ok)       ->  '  1.00000E+2'

                        RealToString(123.456789, 10, 10, a, ok)  ->  '123.456789'
                        RealToString(123.456789, -5, 13, a, ok)  ->  '    1.23456E+2'

                        RealToString(123.456789, -2, 15, a, ok)  ->  '          1.23E+2'

                        if r is a NaN then the string 'nan' is returned formatted and
                        ok will be FALSE.
  *)

  LongRealToString
  PROCEDURE LongRealToString (r: LONGREAL; digits, width: INTEGER;
                              VAR str: ARRAY OF CHAR; VAR ok: BOOLEAN) ;

  (*
     StringToReal - converts, str, into a REAL, r. The parameter, ok, is
                    set to TRUE if the conversion was successful.
  *)

  StringToReal
  PROCEDURE StringToReal (str: ARRAY OF CHAR; VAR r: REAL; VAR ok: BOOLEAN) ;

  (*
     StringToLongReal - converts, str, into a LONGREAL, r. The parameter, ok, is
                        set to TRUE if the conversion was successful.
  *)

  StringToLongReal
  PROCEDURE StringToLongReal (str: ARRAY OF CHAR; VAR r: LONGREAL; VAR ok: BOOLEAN) ;

  END RealConversions.

