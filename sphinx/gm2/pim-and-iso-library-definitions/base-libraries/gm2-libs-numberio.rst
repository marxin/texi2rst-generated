.. _gm2-libs-numberio:

gm2-libs/NumberIO
^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE NumberIO ;

  EXPORT QUALIFIED ReadCard, WriteCard, ReadHex, WriteHex, ReadInt, WriteInt,
                   CardToStr, StrToCard, StrToHex, HexToStr, StrToInt, IntToStr,
                   ReadOct, WriteOct, OctToStr, StrToOct,
                   ReadBin, WriteBin, BinToStr, StrToBin,
                   StrToBinInt, StrToHexInt, StrToOctInt ;

  ReadCard
  PROCEDURE ReadCard (VAR x: CARDINAL) ;

  WriteCard
  PROCEDURE WriteCard (x, n: CARDINAL) ;

  ReadHex
  PROCEDURE ReadHex (VAR x: CARDINAL) ;

  WriteHex
  PROCEDURE WriteHex (x, n: CARDINAL) ;

  ReadInt
  PROCEDURE ReadInt (VAR x: INTEGER) ;

  WriteInt
  PROCEDURE WriteInt (x: INTEGER ; n: CARDINAL) ;

  CardToStr
  PROCEDURE CardToStr (x, n: CARDINAL ; VAR a: ARRAY OF CHAR) ;

  StrToCard
  PROCEDURE StrToCard (a: ARRAY OF CHAR ; VAR x: CARDINAL) ;

  HexToStr
  PROCEDURE HexToStr (x, n: CARDINAL ; VAR a: ARRAY OF CHAR) ;

  StrToHex
  PROCEDURE StrToHex (a: ARRAY OF CHAR ; VAR x: CARDINAL) ;

  IntToStr
  PROCEDURE IntToStr (x: INTEGER ; n: CARDINAL ; VAR a: ARRAY OF CHAR) ;

  StrToInt
  PROCEDURE StrToInt (a: ARRAY OF CHAR ; VAR x: INTEGER) ;

  ReadOct
  PROCEDURE ReadOct (VAR x: CARDINAL) ;

  WriteOct
  PROCEDURE WriteOct (x, n: CARDINAL) ;

  OctToStr
  PROCEDURE OctToStr (x, n: CARDINAL ; VAR a: ARRAY OF CHAR) ;

  StrToOct
  PROCEDURE StrToOct (a: ARRAY OF CHAR ; VAR x: CARDINAL) ;

  ReadBin
  PROCEDURE ReadBin (VAR x: CARDINAL) ;

  WriteBin
  PROCEDURE WriteBin (x, n: CARDINAL) ;

  BinToStr
  PROCEDURE BinToStr (x, n: CARDINAL ; VAR a: ARRAY OF CHAR) ;

  StrToBin
  PROCEDURE StrToBin (a: ARRAY OF CHAR ; VAR x: CARDINAL) ;

  StrToBinInt
  PROCEDURE StrToBinInt (a: ARRAY OF CHAR ; VAR x: INTEGER) ;

  StrToHexInt
  PROCEDURE StrToHexInt (a: ARRAY OF CHAR ; VAR x: INTEGER) ;

  StrToOctInt
  PROCEDURE StrToOctInt (a: ARRAY OF CHAR ; VAR x: INTEGER) ;

  END NumberIO.

