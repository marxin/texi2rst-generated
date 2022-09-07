.. _gm2-libs-strcase:

gm2-libs/StrCase
^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE StrCase ;

  EXPORT QUALIFIED StrToUpperCase, StrToLowerCase, Cap, Lower ;

  (*
     StrToUpperCase - converts string, a, to uppercase returning the
                      result in, b.
  *)

  StrToUpperCase
  PROCEDURE StrToUpperCase (a: ARRAY OF CHAR ; VAR b: ARRAY OF CHAR) ;

  (*
     StrToLowerCase - converts string, a, to lowercase returning the
                      result in, b.
  *)

  StrToLowerCase
  PROCEDURE StrToLowerCase (a: ARRAY OF CHAR ; VAR b: ARRAY OF CHAR) ;

  (*
     Cap - converts a lower case character into a capital character.
           If the character is not a lower case character 'a'..'z'
           then the character is simply returned unaltered.
  *)

  Cap
  PROCEDURE Cap (ch: CHAR) : CHAR ;

  (*
     Lower - converts an upper case character into a lower case character.
             If the character is not an upper case character 'A'..'Z'
             then the character is simply returned unaltered.
  *)

  Lower
  PROCEDURE Lower (ch: CHAR) : CHAR ;

  END StrCase.

