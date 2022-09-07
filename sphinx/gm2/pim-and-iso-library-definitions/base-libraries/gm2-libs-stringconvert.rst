.. _gm2-libs-stringconvert:

gm2-libs/StringConvert
^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE StringConvert ;

  FROM DynamicStrings IMPORT String ;
  EXPORT QUALIFIED IntegerToString, StringToInteger,
                   StringToLongInteger, LongIntegerToString,
                   StringToCardinal, CardinalToString,
                   StringToLongCardinal, LongCardinalToString,
                   StringToShortCardinal, ShortCardinalToString,
                   StringToLongreal, LongrealToString,
                   ToSigFig,
                   stoi, itos, ctos, stoc, hstoi, ostoi, bstoi,
                   hstoc, ostoc, bstoc,
                   stor, stolr ;

  (*
     IntegerToString - converts INTEGER, i, into a String. The field with
                       can be specified if non zero. Leading characters
                       are defined by padding and this function will
                       prepend a + if sign is set to TRUE.
                       The base allows the caller to generate binary,
                       octal, decimal, hexidecimal numbers.
                       The value of lower is only used when hexidecimal
                       numbers are generated and if TRUE then digits
                       abcdef are used, and if FALSE then ABCDEF are used.
  *)

  IntegerToString
  PROCEDURE IntegerToString (i: INTEGER; width: CARDINAL; padding: CHAR; sign: BOOLEAN;
                             base: CARDINAL; lower: BOOLEAN) : String ;

  (*
     CardinalToString - converts CARDINAL, c, into a String. The field
                        width can be specified if non zero. Leading
                        characters are defined by padding.
                        The base allows the caller to generate binary,
                        octal, decimal, hexidecimal numbers.
                        The value of lower is only used when hexidecimal
                        numbers are generated and if TRUE then digits
                        abcdef are used, and if FALSE then ABCDEF are used.
  *)

  CardinalToString
  PROCEDURE CardinalToString (c: CARDINAL; width: CARDINAL; padding: CHAR;
                              base: CARDINAL; lower: BOOLEAN) : String ;

  (*
     StringToInteger - converts a string, s, of, base, into an INTEGER.
                       Leading white space is ignored. It stops converting
                       when either the string is exhausted or if an illegal
                       numeral is found.
                       The parameter found is set TRUE if a number was found.
  *)

  StringToInteger
  PROCEDURE StringToInteger (s: String; base: CARDINAL; VAR found: BOOLEAN) : INTEGER ;

  (*
     StringToCardinal - converts a string, s, of, base, into a CARDINAL.
                        Leading white space is ignored. It stops converting
                        when either the string is exhausted or if an illegal
                        numeral is found.
                        The parameter found is set TRUE if a number was found.
  *)

  StringToCardinal
  PROCEDURE StringToCardinal (s: String; base: CARDINAL; VAR found: BOOLEAN) : CARDINAL ;

  (*
     LongIntegerToString - converts LONGINT, i, into a String. The field with
                           can be specified if non zero. Leading characters
                           are defined by padding and this function will
                           prepend a + if sign is set to TRUE.
                           The base allows the caller to generate binary,
                           octal, decimal, hexidecimal numbers.
                           The value of lower is only used when hexidecimal
                           numbers are generated and if TRUE then digits
                           abcdef are used, and if FALSE then ABCDEF are used.
  *)

  LongIntegerToString
  PROCEDURE LongIntegerToString (i: LONGINT; width: CARDINAL; padding: CHAR;
                                 sign: BOOLEAN; base: CARDINAL; lower: BOOLEAN) : String ;

  (*
     StringToLongInteger - converts a string, s, of, base, into an LONGINT.
                           Leading white space is ignored. It stops converting
                           when either the string is exhausted or if an illegal
                           numeral is found.
                           The parameter found is set TRUE if a number was found.
  *)

  StringToLongInteger
  PROCEDURE StringToLongInteger (s: String; base: CARDINAL; VAR found: BOOLEAN) : LONGINT ;

  (*
     LongCardinalToString - converts LONGCARD, c, into a String. The field
                            width can be specified if non zero. Leading
                            characters are defined by padding.
                            The base allows the caller to generate binary,
                            octal, decimal, hexidecimal numbers.
                            The value of lower is only used when hexidecimal
                            numbers are generated and if TRUE then digits
                            abcdef are used, and if FALSE then ABCDEF are used.
  *)

  LongCardinalToString
  PROCEDURE LongCardinalToString (c: LONGCARD; width: CARDINAL; padding: CHAR;
                                  base: CARDINAL; lower: BOOLEAN) : String ;

  (*
     StringToLongCardinal - converts a string, s, of, base, into a LONGCARD.
                            Leading white space is ignored. It stops converting
                            when either the string is exhausted or if an illegal
                            numeral is found.
                            The parameter found is set TRUE if a number was found.
  *)

  StringToLongCardinal
  PROCEDURE StringToLongCardinal (s: String; base: CARDINAL; VAR found: BOOLEAN) : LONGCARD ;

  (*
     ShortCardinalToString - converts SHORTCARD, c, into a String. The field
                             width can be specified if non zero. Leading
                             characters are defined by padding.
                             The base allows the caller to generate binary,
                             octal, decimal, hexidecimal numbers.
                             The value of lower is only used when hexidecimal
                             numbers are generated and if TRUE then digits
                             abcdef are used, and if FALSE then ABCDEF are used.
  *)

  ShortCardinalToString
  PROCEDURE ShortCardinalToString (c: SHORTCARD; width: CARDINAL; padding: CHAR;
                                   base: CARDINAL; lower: BOOLEAN) : String ;

  (*
     StringToShortCardinal - converts a string, s, of, base, into a SHORTCARD.
                             Leading white space is ignored. It stops converting
                             when either the string is exhausted or if an illegal
                             numeral is found.
                             The parameter found is set TRUE if a number was found.
  *)

  StringToShortCardinal
  PROCEDURE StringToShortCardinal (s: String; base: CARDINAL;
                                   VAR found: BOOLEAN) : SHORTCARD ;

  (*
     stoi - decimal string to INTEGER
  *)

  stoi
  PROCEDURE stoi (s: String) : INTEGER ;

  (*
     itos - integer to decimal string.
  *)

  itos
  PROCEDURE itos (i: INTEGER; width: CARDINAL; padding: CHAR; sign: BOOLEAN) : String ;

  (*
     ctos - cardinal to decimal string.
  *)

  ctos
  PROCEDURE ctos (c: CARDINAL; width: CARDINAL; padding: CHAR) : String ;

  (*
     stoc - decimal string to CARDINAL
  *)

  stoc
  PROCEDURE stoc (s: String) : CARDINAL ;

  (*
     hstoi - hexidecimal string to INTEGER
  *)

  hstoi
  PROCEDURE hstoi (s: String) : INTEGER ;

  (*
     ostoi - octal string to INTEGER
  *)

  ostoi
  PROCEDURE ostoi (s: String) : INTEGER ;

  (*
     bstoi - binary string to INTEGER
  *)

  bstoi
  PROCEDURE bstoi (s: String) : INTEGER ;

  (*
     hstoc - hexidecimal string to CARDINAL
  *)

  hstoc
  PROCEDURE hstoc (s: String) : CARDINAL ;

  (*
     ostoc - octal string to CARDINAL
  *)

  ostoc
  PROCEDURE ostoc (s: String) : CARDINAL ;

  (*
     bstoc - binary string to CARDINAL
  *)

  bstoc
  PROCEDURE bstoc (s: String) : CARDINAL ;

  (*
     StringToLongreal - returns a LONGREAL and sets found to TRUE
                        if a legal number is seen.
  *)

  StringToLongreal
  PROCEDURE StringToLongreal (s: String; VAR found: BOOLEAN) : LONGREAL ;

  (*
     LongrealToString - converts a LONGREAL number, Real, which has,
                        TotalWidth, and FractionWidth into a string.

                        So for example:

                        LongrealToString(1.0, 4, 2)  -> '1.00'
                        LongrealToString(12.3, 5, 2) -> '12.30'
                        LongrealToString(12.3, 6, 2) -> ' 12.30'
                        LongrealToString(12.3, 6, 3) -> '12.300'

                        if total width is too small then the fraction
                        becomes truncated.

                        LongrealToString(12.3, 5, 3) -> '12.30'

                        If TotalWidth is 0 then the function
                        will return the value of x which is converted
                        into as a fixed point number with exhaustive
                        precision.
  *)

  LongrealToString
  PROCEDURE LongrealToString (x: LONGREAL;
                              TotalWidth, FractionWidth: CARDINAL) : String ;

  (*
     stor - returns a REAL given a string.
  *)

  stor
  PROCEDURE stor (s: String) : REAL ;

  (*
     stolr - returns a LONGREAL given a string.
  *)

  stolr
  PROCEDURE stolr (s: String) : LONGREAL ;

  (*
     ToSigFig - returns a floating point or base 10 integer
                string which is accurate to, n, significant
                figures.  It will return a new String
                and, s, will be destroyed.

                So:  12.345

                rounded to the following significant figures yields

                5      12.345
                4      12.34
                3      12.3
                2      12
                1      10
  *)

  ToSigFig
  PROCEDURE ToSigFig (s: String; n: CARDINAL) : String ;

  (*
     ToDecimalPlaces - returns a floating point or base 10 integer
                       string which is accurate to, n, decimal
                       places.  It will return a new String
                       and, s, will be destroyed.
                       Decimal places yields, n, digits after
                       the .

                       So:  12.345

                       rounded to the following decimal places yields

                       5      12.34500
                       4      12.3450
                       3      12.345
                       2      12.34
                       1      12.3
  *)

  ToDecimalPlaces
  PROCEDURE ToDecimalPlaces (s: String; n: CARDINAL) : String ;

  END StringConvert.

