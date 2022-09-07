.. _gm2-libs-iso-shortwholeio:

gm2-libs-iso/ShortWholeIO
^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE ShortWholeIO;

    (* Input and output of whole numbers in decimal text form
       over specified channels.  The read result is of the
       type IOConsts.ReadResults.
    *)

  IMPORT IOChan;

    (* The text form of a signed whole number is
         ["+" | "-"], decimal digit, {decimal digit}

       The text form of an unsigned whole number is
         decimal digit, {decimal digit}
    *)

  ReadInt
  PROCEDURE ReadInt (cid: IOChan.ChanId; VAR int: SHORTINT);
    (* Skips leading spaces, and removes any remaining characters
       from cid that form part of a signed whole number.  The
       value of this number is assigned to int.  The read result
       is set to the value allRight, outOfRange, wrongFormat,
       endOfLine, or endOfInput.
    *)

  WriteInt
  PROCEDURE WriteInt (cid: IOChan.ChanId; int: SHORTINT;
                      width: CARDINAL);
    (* Writes the value of int to cid in text form, in a field of
       the given minimum width. *)

  ReadCard
  PROCEDURE ReadCard (cid: IOChan.ChanId; VAR card: SHORTCARD);
    (* Skips leading spaces, and removes any remaining characters
       from cid that form part of an unsigned whole number.  The
       value of this number is assigned to card. The read result
       is set to the value allRight, outOfRange, wrongFormat,
       endOfLine, or endOfInput.
    *)

  WriteCard
  PROCEDURE WriteCard (cid: IOChan.ChanId; card: SHORTCARD;
                       width: CARDINAL);
    (* Writes the value of card to cid in text form, in a field
       of the given minimum width. *)

  END ShortWholeIO.

