.. _gm2-libs-strio:

gm2-libs/StrIO
^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE StrIO ;

  EXPORT QUALIFIED ReadString, WriteString,
                   WriteLn ;

  (*
     WriteLn - writes a carriage return and a newline
               character.
  *)

  WriteLn
  PROCEDURE WriteLn ;

  (*
     ReadString - reads a sequence of characters into a string.
                  Line editing accepts Del, Ctrl H, Ctrl W and
                  Ctrl U.
  *)

  ReadString
  PROCEDURE ReadString (VAR a: ARRAY OF CHAR) ;

  (*
     WriteString - writes a string to the default output.
  *)

  WriteString
  PROCEDURE WriteString (a: ARRAY OF CHAR) ;

  END StrIO.

