.. _gm2-libs-iso-textio:

gm2-libs-iso/TextIO
^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE TextIO;

    (* Input and output of character and string types over
       specified channels.  The read result is of the type
       IOConsts.ReadResults.
    *)

  IMPORT IOChan;

    (* The following procedures do not read past line marks *)

  ReadChar
  PROCEDURE ReadChar (cid: IOChan.ChanId; VAR ch: CHAR);
    (* If possible, removes a character from the input stream
       cid and assigns the corresponding value to ch.  The
       read result is set to the value allRight, endOfLine, or
       endOfInput.
    *)

  ReadRestLine
  PROCEDURE ReadRestLine (cid: IOChan.ChanId; VAR s: ARRAY OF CHAR);
    (* Removes any remaining characters from the input stream
       cid before the next line mark,  copying to s as many as
       can be accommodated as a string value.  The read result is
       set to the value allRight, outOfRange, endOfLine, or
       endOfInput.
    *)

  ReadString
  PROCEDURE ReadString (cid: IOChan.ChanId; VAR s: ARRAY OF CHAR);
    (* Removes only those characters from the input stream cid
       before the next line mark that can be accommodated in s
       as a string value, and copies them to s.  The read result
       is set to the value allRight, endOfLine, or endOfInput.
    *)

  ReadToken
  PROCEDURE ReadToken (cid: IOChan.ChanId; VAR s: ARRAY OF CHAR);
    (* Skips leading spaces, and then removes characters from
       the input stream cid before the next space or line mark,
       copying to s as many as can be accommodated as a string
       value.  The read result is set to the value allRight,
       outOfRange, endOfLine, or endOfInput.
    *)

    (* The following procedure reads past the next line mark *)

  SkipLine
  PROCEDURE SkipLine (cid: IOChan.ChanId);
    (* Removes successive items from the input stream cid up
       to and including the next line mark, or until the end
       of input is reached.  The read result is set to the
       value allRight, or endOfInput.
    *)

    (* Output procedures *)

  WriteChar
  PROCEDURE WriteChar (cid: IOChan.ChanId; ch: CHAR);
    (* Writes the value of ch to the output stream cid. *)

  WriteLn
  PROCEDURE WriteLn (cid: IOChan.ChanId);
    (* Writes a line mark to the output stream cid. *)

  WriteString
  PROCEDURE WriteString (cid: IOChan.ChanId; s: ARRAY OF CHAR);
    (* Writes the string value in s to the output stream cid. *)

  END TextIO.

