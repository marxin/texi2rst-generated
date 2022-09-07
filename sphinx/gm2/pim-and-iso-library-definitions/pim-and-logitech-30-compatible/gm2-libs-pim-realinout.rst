.. _gm2-libs-pim-realinout:

gm2-libs-pim/RealInOut
^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE RealInOut ;

  EXPORT QUALIFIED SetNoOfDecimalPlaces,
                   ReadReal, WriteReal, WriteRealOct,
                   ReadLongReal, WriteLongReal, WriteLongRealOct,
                   ReadShortReal, WriteShortReal, WriteShortRealOct,
                   Done ;

  CONST
  DefaultDecimalPlaces  (const)
     DefaultDecimalPlaces = 6 ;

  VAR
  Done (var)
     Done: BOOLEAN ;

  (*
     SetNoOfDecimalPlaces - number of decimal places WriteReal and
                            WriteLongReal should emit.  This procedure
                            can be used to override the default
                            DefaultDecimalPlaces constant.
  *)

  SetNoOfDecimalPlaces
  PROCEDURE SetNoOfDecimalPlaces (places: CARDINAL) ;

  (*
     ReadReal - reads a real number, legal syntaxes include:
                100, 100.0, 100e0, 100E0, 100E-1, E2, +1E+2, 1e+2
  *)

  ReadReal
  PROCEDURE ReadReal (VAR x: REAL) ;

  (*
     WriteReal - writes a real to the terminal. The real number
                 is right justified and, n, is the minimum field
                 width.
  *)

  WriteReal
  PROCEDURE WriteReal (x: REAL; n: CARDINAL) ;

  (*
     WriteRealOct - writes the real to terminal in octal words.
  *)

  WriteRealOct
  PROCEDURE WriteRealOct (x: REAL) ;

  (*
     ReadLongReal - reads a LONGREAL number, legal syntaxes include:
                    100, 100.0, 100e0, 100E0, 100E-1, E2, +1E+2, 1e+2
  *)

  ReadLongReal
  PROCEDURE ReadLongReal (VAR x: LONGREAL) ;

  (*
     WriteLongReal - writes a LONGREAL to the terminal. The real number
                     is right justified and, n, is the minimum field
                     width.
  *)

  WriteLongReal
  PROCEDURE WriteLongReal (x: LONGREAL; n: CARDINAL) ;

  (*
     WriteLongRealOct - writes the LONGREAL to terminal in octal words.
  *)

  WriteLongRealOct
  PROCEDURE WriteLongRealOct (x: LONGREAL) ;

  (*
     ReadShortReal - reads a SHORTREAL number, legal syntaxes include:
                     100, 100.0, 100e0, 100E0, 100E-1, E2, +1E+2, 1e+2
  *)

  ReadShortReal
  PROCEDURE ReadShortReal (VAR x: SHORTREAL) ;

  (*
     WriteShortReal - writes a SHORTREAL to the terminal. The real number
                      is right justified and, n, is the minimum field
                      width.
  *)

  WriteShortReal
  PROCEDURE WriteShortReal (x: SHORTREAL; n: CARDINAL) ;

  (*
     WriteShortRealOct - writes the SHORTREAL to terminal in octal words.
  *)

  WriteShortRealOct
  PROCEDURE WriteShortRealOct (x: SHORTREAL) ;

  END RealInOut.

