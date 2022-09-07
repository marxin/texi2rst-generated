.. _gm2-libs-iso-stextio:

gm2-libs-iso/STextIO
^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE STextIO;

    (* Input and output of character and string types over default channels. The read result
       is of the type IOConsts.ReadResults.
    *)

    (* The following procedures do not read past line marks *)

  ReadChar
  PROCEDURE ReadChar (VAR ch: CHAR);
    (* If possible, removes a character from the default input stream, and assigns the
       corresponding value to ch.  The read result is set to allRight, endOfLine or
       endOfInput.
    *)

  ReadRestLine
  PROCEDURE ReadRestLine (VAR s: ARRAY OF CHAR);
    (* Removes any remaining characters from the default input stream before the next line
       mark, copying to s as many as can be accommodated as a string value.  The read result
       is set to the value allRight, outOfRange, endOfLine, or endOfInput.
    *)

  ReadString
  PROCEDURE ReadString (VAR s: ARRAY OF CHAR);
    (* Removes only those characters from the default input stream before the next line mark
       that can be accommodated in s as a string value, and copies them to s. The read result
       is set to the value allRight, endOfLine, or endOfInput.
    *)

  ReadToken
  PROCEDURE ReadToken (VAR s: ARRAY OF CHAR);
    (* Skips leading spaces, and then removes characters from the default input stream before
       the next space or line mark, copying to s as many as can be accommodated as a string
       value.  The read result is set to the value allRight, outOfRange, endOfLine, or
       endOfInput.
    *)

    (* The following procedure reads past the next line mark *)

  SkipLine
  PROCEDURE SkipLine;
    (* Removes successive items from the default input stream up to and including the next
       line mark or until the end of input is reached. The read result is set to the value
       allRight, or endOfInput.
    *)

    (* Output procedures *)

  WriteChar
  PROCEDURE WriteChar (ch: CHAR);
    (* Writes the value of ch to the default output stream. *)

  WriteLn
  PROCEDURE WriteLn;
    (* Writes a line mark to the default output stream. *)

  WriteString
  PROCEDURE WriteString (s: ARRAY OF CHAR);
    (* Writes the string value of s to the default output stream. *)

  END STextIO.

