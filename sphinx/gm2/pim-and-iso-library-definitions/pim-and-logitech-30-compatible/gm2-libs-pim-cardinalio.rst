.. _gm2-libs-pim-cardinalio:

gm2-libs-pim/CardinalIO
^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE CardinalIO ;

  EXPORT QUALIFIED Done,
                   ReadCardinal, WriteCardinal, ReadHex, WriteHex,
                   ReadLongCardinal, WriteLongCardinal, ReadLongHex,
                   WriteLongHex,
                   ReadShortCardinal, WriteShortCardinal, ReadShortHex,
                   WriteShortHex ;

  VAR
  Done (var)
     Done: BOOLEAN ;

  (*
     ReadCardinal - read an unsigned decimal number from the terminal.
                    The read continues until a space, newline, esc or
                    end of file is reached.
  *)

  ReadCardinal
  PROCEDURE ReadCardinal (VAR c: CARDINAL) ;

  (*
     WriteCardinal - writes the value, c, to the terminal and ensures
                     that at least, n, characters are written. The number
                     will be padded out by preceeding spaces if necessary.
  *)

  WriteCardinal
  PROCEDURE WriteCardinal (c: CARDINAL; n: CARDINAL) ;

  (*
     ReadHex - reads in an unsigned hexadecimal number from the terminal.
               The read continues until a space, newline, esc or
               end of file is reached.
  *)

  ReadHex
  PROCEDURE ReadHex (VAR c: CARDINAL) ;

  (*
     WriteHex - writes out a CARDINAL, c, in hexadecimal format padding
                with, n, characters (leading with '0')
  *)

  WriteHex
  PROCEDURE WriteHex (c: CARDINAL; n: CARDINAL) ;

  (*
     ReadLongCardinal - read an unsigned decimal number from the terminal.
                        The read continues until a space, newline, esc or
                        end of file is reached.
  *)

  ReadLongCardinal
  PROCEDURE ReadLongCardinal (VAR c: LONGCARD) ;

  (*
     WriteLongCardinal - writes the value, c, to the terminal and ensures
                         that at least, n, characters are written. The number
                         will be padded out by preceeding spaces if necessary.
  *)

  WriteLongCardinal
  PROCEDURE WriteLongCardinal (c: LONGCARD; n: CARDINAL) ;

  (*
     ReadLongHex - reads in an unsigned hexadecimal number from the terminal.
                   The read continues until a space, newline, esc or
                   end of file is reached.
  *)

  ReadLongHex
  PROCEDURE ReadLongHex (VAR c: LONGCARD) ;

  (*
     WriteLongHex - writes out a LONGCARD, c, in hexadecimal format padding
                    with, n, characters (leading with '0')
  *)

  WriteLongHex
  PROCEDURE WriteLongHex (c: LONGCARD; n: CARDINAL) ;

  (*
     WriteShortCardinal - writes the value, c, to the terminal and ensures
                         that at least, n, characters are written. The number
                         will be padded out by preceeding spaces if necessary.
  *)

  WriteShortCardinal
  PROCEDURE WriteShortCardinal (c: SHORTCARD; n: CARDINAL) ;

  (*
     ReadShortCardinal - read an unsigned decimal number from the terminal.
                         The read continues until a space, newline, esc or
                         end of file is reached.
  *)

  ReadShortCardinal
  PROCEDURE ReadShortCardinal (VAR c: SHORTCARD) ;

  (*
     ReadShortHex - reads in an unsigned hexadecimal number from the terminal.
                   The read continues until a space, newline, esc or
                   end of file is reached.
  *)

  ReadShortHex
  PROCEDURE ReadShortHex (VAR c: SHORTCARD) ;

  (*
     WriteShortHex - writes out a SHORTCARD, c, in hexadecimal format padding
                    with, n, characters (leading with '0')
  *)

  WriteShortHex
  PROCEDURE WriteShortHex (c: SHORTCARD; n: CARDINAL) ;

  END CardinalIO.

