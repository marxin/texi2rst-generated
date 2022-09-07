.. _gm2-libs-iso-slongwholeio:

gm2-libs-iso/SLongWholeIO
^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE SLongWholeIO;

    (* Input and output of whole numbers in decimal text form over
       default channels.  The read result is of the type
       IOConsts.ReadResults.
    *)

    (* The text form of a signed whole number is
         ["+" | "-"], decimal digit, {decimal digit}

       The text form of an unsigned whole number is
         decimal digit, {decimal digit}
    *)

  ReadInt
  PROCEDURE ReadInt (VAR int: LONGINT);
    (* Skips leading spaces, and removes any remaining characters
       from the default input channel that form part of a signed
       whole number.  The value of this number is assigned
       to int.  The read result is set to the value allRight,
       outOfRange, wrongFormat, endOfLine, or endOfInput.
    *)

  WriteInt
  PROCEDURE WriteInt (int: LONGINT; width: CARDINAL);
    (* Writes the value of int to the default output channel in
       text form, in a field of the given minimum width.
    *)

  ReadCard
  PROCEDURE ReadCard (VAR card: LONGCARD);
    (* Skips leading spaces, and removes any remaining characters
       from the default input channel that form part of an
       unsigned whole number.  The value of this number is
       assigned to card.  The read result is set to the value
       allRight, outOfRange, wrongFormat, endOfLine, or endOfInput.
    *)

  WriteCard
  PROCEDURE WriteCard (card: LONGCARD; width: CARDINAL);
    (* Writes the value of card to the default output channel in
       text form, in a field of the given minimum width.
    *)

  END SLongWholeIO.

