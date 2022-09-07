.. _gm2-libs-iso-convstringreal:

gm2-libs-iso/ConvStringReal
^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE ConvStringReal ;

  FROM DynamicStrings IMPORT String ;

  (*
     RealToFloatString - converts a real with, sigFigs, into a string
                         and returns the result as a string.
  *)

  RealToFloatString
  PROCEDURE RealToFloatString (real: REAL; sigFigs: CARDINAL) : String ;

  (*
     RealToEngString - converts the value of real to floating-point
                       string form, with sigFigs significant figures.
                       The number is scaled with one to three digits
                       in the whole number part and with an exponent
                       that is a multiple of three.
  *)

  RealToEngString
  PROCEDURE RealToEngString (real: REAL; sigFigs: CARDINAL) : String ;

  (*
     RealToFixedString - returns the number of characters in the fixed-point
                         string representation of real rounded to the given
                         place relative to the decimal point.
  *)

  RealToFixedString
  PROCEDURE RealToFixedString (real: REAL; place: INTEGER) : String ;

  END ConvStringReal.

