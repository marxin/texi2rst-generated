.. _gm2-libs-iso-sshortio:

gm2-libs-iso/SShortIO
^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE SShortIO;

    (* Input and output of short real numbers in decimal text form
       using default channels.  The read result is of the type
       IOConsts.ReadResults.
    *)

    (* The text form of a signed fixed-point real number is
         ["+" | "-"], decimal digit, {decimal digit},
         [".", {decimal digit}]

       The text form of a signed floating-point real number is
         signed fixed-point real number,
         "E", ["+" | "-"], decimal digit, {decimal digit}
    *)

  ReadReal
  PROCEDURE ReadReal (VAR real: SHORTREAL);
    (* Skips leading spaces, and removes any remaining characters
       from the default input channel that form part of a signed
       fixed or floating point number. The value of this number
       is assigned to real.  The read result is set to the value
       allRight, outOfRange, wrongFormat, endOfLine, or endOfInput.
    *)

  WriteFloat
  PROCEDURE WriteFloat (real: SHORTREAL; sigFigs: CARDINAL;
                        width: CARDINAL);
    (* Writes the value of real to the default output channel in
       floating-point text form, with sigFigs significant figures,
       in a field of the given minimum width.
    *)

  WriteEng
  PROCEDURE WriteEng (real: SHORTREAL; sigFigs: CARDINAL;
                      width: CARDINAL);
    (* As for WriteFloat, except that the number is scaled with
       one to three digits in the whole number part, and with an
       exponent that is a multiple of three.
    *)

  WriteFixed
  PROCEDURE WriteFixed (real: SHORTREAL; place: INTEGER;
                        width: CARDINAL);
    (* Writes the value of real to the default output channel in
       fixed-point text form, rounded to the given place relative
       to the decimal point, in a field of the given minimum width.
    *)

  WriteReal
  PROCEDURE WriteReal (real: SHORTREAL; width: CARDINAL);
    (* Writes the value of real to the default output channel, as
       WriteFixed if the sign and magnitude can be shown in the
       given width, or otherwise as WriteFloat. The number of
       places or significant digits depends on the given width.
    *)

  END SShortIO.

