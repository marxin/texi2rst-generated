.. _gm2-libs-pim-terminal:

gm2-libs-pim/Terminal
^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE Terminal ;

  (*
     It provides simple terminal input output
     routines which all utilize the TermBase module.
  *)

  EXPORT QUALIFIED Read, KeyPressed, ReadAgain, ReadString, Write,
                   WriteString, WriteLn ;

  (*
     Read - reads a single character.
  *)

  Read
  PROCEDURE Read (VAR ch: CHAR) ;

  (*
     KeyPressed - returns TRUE if a character can be read without blocking
                  the caller.
  *)

  KeyPressed
  PROCEDURE KeyPressed () : BOOLEAN ;

  (*
     ReadString - reads a sequence of characters.
                  Tabs are expanded into 8 spaces and <cr> or <lf> terminates
                  the string.
  *)

  ReadString
  PROCEDURE ReadString (VAR s: ARRAY OF CHAR) ;

  (*
     ReadAgain - makes the last character readable again.
  *)

  ReadAgain
  PROCEDURE ReadAgain ;

  (*
     Write - writes a single character to the Termbase module.
  *)

  Write
  PROCEDURE Write (ch: CHAR) ;

  (*
     WriteString - writes out a string which is terminated by a <nul>
                   character or the end of string HIGH(s).
  *)

  WriteString
  PROCEDURE WriteString (s: ARRAY OF CHAR) ;

  (*
     WriteLn - writes a lf character.
  *)

  WriteLn
  PROCEDURE WriteLn ;

  END Terminal.

