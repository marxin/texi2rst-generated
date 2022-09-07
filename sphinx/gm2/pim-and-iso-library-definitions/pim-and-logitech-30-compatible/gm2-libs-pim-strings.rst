.. _gm2-libs-pim-strings:

gm2-libs-pim/Strings
^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE Strings ;

  EXPORT QUALIFIED Assign, Insert, Delete, Pos, Copy, ConCat, Length,
                   CompareStr ;

  (*
     Assign - dest := source.
  *)

  Assign
  PROCEDURE Assign (VAR dest: ARRAY OF CHAR; source: ARRAY OF CHAR) ;

  (*
     Insert - insert the string, substr, into str at position, index.
              substr, is added to the end of, str, if, index >= length(str)
  *)

  Insert
  PROCEDURE Insert (substr: ARRAY OF CHAR; VAR str: ARRAY OF CHAR;
                    index: CARDINAL) ;

  (*
     Delete - delete len characters from, str, starting at, index.
  *)

  Delete
  PROCEDURE Delete (VAR str: ARRAY OF CHAR; index: CARDINAL; length: CARDINAL) ;

  (*
     Pos - return the first position of, substr, in, str.
  *)

  Pos
  PROCEDURE Pos (substr, str: ARRAY OF CHAR) : CARDINAL ;

  (*
     Copy - copy at most, length, characters in, substr, to, str,
            starting at position, index.
  *)

  Copy
  PROCEDURE Copy (str: ARRAY OF CHAR;
                  index, length: CARDINAL; VAR result: ARRAY OF CHAR) ;

  (*
     ConCat - concatenates two strings, s1, and, s2
              and places the result into, dest.
  *)

  ConCat
  PROCEDURE ConCat (s1, s2: ARRAY OF CHAR; VAR dest: ARRAY OF CHAR) ;

  (*
     Length - return the length of string, s.
  *)

  Length
  PROCEDURE Length (s: ARRAY OF CHAR) : CARDINAL ;

  (*
     CompareStr - compare two strings, left, and, right.
  *)

  CompareStr
  PROCEDURE CompareStr (left, right: ARRAY OF CHAR) : INTEGER ;

  END Strings.

