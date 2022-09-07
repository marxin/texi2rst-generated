.. _gm2-libs-fpuio:

gm2-libs/FpuIO
^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE FpuIO ;

  EXPORT QUALIFIED ReadReal, WriteReal, StrToReal, RealToStr,
                   ReadLongReal, WriteLongReal, StrToLongReal,
                   LongRealToStr,
                   ReadLongInt, WriteLongInt, StrToLongInt,
                   LongIntToStr ;

  ReadReal
  PROCEDURE ReadReal (VAR x: REAL) ;
  WriteReal
  PROCEDURE WriteReal (x: REAL; TotalWidth, FractionWidth: CARDINAL) ;
  StrToReal
  PROCEDURE StrToReal (a: ARRAY OF CHAR ; VAR x: REAL) ;
  RealToStr
  PROCEDURE RealToStr (x: REAL; TotalWidth, FractionWidth: CARDINAL;
                       VAR a: ARRAY OF CHAR) ;

  ReadLongReal
  PROCEDURE ReadLongReal (VAR x: LONGREAL) ;
  WriteLongReal
  PROCEDURE WriteLongReal (x: LONGREAL;
                           TotalWidth, FractionWidth: CARDINAL) ;
  StrToLongReal
  PROCEDURE StrToLongReal (a: ARRAY OF CHAR ; VAR x: LONGREAL) ;
  LongRealToStr
  PROCEDURE LongRealToStr (x: LONGREAL;
                           TotalWidth, FractionWidth: CARDINAL;
                           VAR a: ARRAY OF CHAR) ;

  ReadLongInt
  PROCEDURE ReadLongInt (VAR x: LONGINT) ;
  WriteLongInt
  PROCEDURE WriteLongInt (x: LONGINT; n: CARDINAL) ;
  StrToLongInt
  PROCEDURE StrToLongInt (a: ARRAY OF CHAR ; VAR x: LONGINT) ;
  LongIntToStr
  PROCEDURE LongIntToStr (x: LONGINT; n: CARDINAL; VAR a: ARRAY OF CHAR) ;

  END FpuIO.

