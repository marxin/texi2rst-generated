.. _gm2-libs-formatstrings:

gm2-libs/FormatStrings
^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE FormatStrings ;

  FROM SYSTEM IMPORT BYTE ;
  FROM DynamicStrings IMPORT String ;
  EXPORT QUALIFIED Sprintf0, Sprintf1, Sprintf2, Sprintf3, Sprintf4,
                   HandleEscape ;

  (*
     Sprintf0 - returns a String containing, fmt, after it has had its
                escape sequences translated.
  *)

  Sprintf0
  PROCEDURE Sprintf0 (fmt: String) : String ;

  (*
     Sprintf1 - returns a String containing, fmt, together with
                encapsulated entity, w. It only formats the
                first %s or %d with n.
  *)

  Sprintf1
  PROCEDURE Sprintf1 (fmt: String; w: ARRAY OF BYTE) : String ;

  (*
     Sprintf2 - returns a string, fmt, which has been formatted.
  *)

  Sprintf2
  PROCEDURE Sprintf2 (fmt: String; w1, w2: ARRAY OF BYTE) : String ;

  (*
     Sprintf3 - returns a string, fmt, which has been formatted.
  *)

  Sprintf3
  PROCEDURE Sprintf3 (fmt: String; w1, w2, w3: ARRAY OF BYTE) : String ;

  (*
     Sprintf4 - returns a string, fmt, which has been formatted.
  *)

  Sprintf4
  PROCEDURE Sprintf4 (fmt: String;
                      w1, w2, w3, w4: ARRAY OF BYTE) : String ;

  (*
     HandleEscape - translates \a, \b, \e, \f, \n, \r, \x[hex] \[octal]
                    into their respective ascii codes.  It also converts
                    \[any] into a single [any] character.
  *)

  HandleEscape
  PROCEDURE HandleEscape (s: String) : String ;

  END FormatStrings.

