.. _gm2-libs-iso-convstringlong:

gm2-libs-iso/ConvStringLong
^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE ConvStringLong ;

  FROM DynamicStrings IMPORT String ;

  (*
     RealToFloatString - converts a real with, sigFigs, into a string
                         and returns the result as a string.
  *)

  RealToFloatString
  PROCEDURE RealToFloatString (real: LONGREAL; sigFigs: CARDINAL) : String ;

  (*
     RealToEngString - converts the value of real to floating-point
                       string form, with sigFigs significant figures.
                       The number is scaled with one to three digits
                       in the whole number part and with an exponent
                       that is a multiple of three.
  *)

  RealToEngString
  PROCEDURE RealToEngString (real: LONGREAL; sigFigs: CARDINAL) : String ;

  (*
     RealToFixedString - returns the number of characters in the fixed-point
                         string representation of real rounded to the given
                         place relative to the decimal point.
  *)

  RealToFixedString
  PROCEDURE RealToFixedString (real: LONGREAL; place: INTEGER) : String ;

  END ConvStringLong.

