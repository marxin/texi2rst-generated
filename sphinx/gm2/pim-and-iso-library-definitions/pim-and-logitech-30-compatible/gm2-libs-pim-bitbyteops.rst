.. _gm2-libs-pim-bitbyteops:

gm2-libs-pim/BitByteOps
^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE BitByteOps ;

  FROM SYSTEM IMPORT BYTE ;

  (*
     GetBits - returns the bits firstBit..lastBit from source.
               Bit 0 of byte maps onto the firstBit of source.
  *)

  GetBits
  PROCEDURE GetBits (source: BYTE; firstBit, lastBit: CARDINAL) : BYTE ;

  (*
     SetBits - sets bits in, byte, starting at, firstBit, and ending at,
               lastBit, with, pattern.  The bit zero of, pattern, will
               be placed into, byte, at position, firstBit.
  *)

  SetBits
  PROCEDURE SetBits (VAR byte: BYTE; firstBit, lastBit: CARDINAL;
                     pattern: BYTE) ;

  (*
     ByteAnd - returns a bitwise (left AND right)
  *)

  ByteAnd
  PROCEDURE ByteAnd (left, right: BYTE) : BYTE ;

  (*
     ByteOr - returns a bitwise (left OR right)
  *)

  ByteOr
  PROCEDURE ByteOr (left, right: BYTE) : BYTE ;

  (*
     ByteXor - returns a bitwise (left XOR right)
  *)

  ByteXor
  PROCEDURE ByteXor (left, right: BYTE) : BYTE ;

  (*
     ByteNot - returns a byte with all bits inverted.
  *)

  ByteNot
  PROCEDURE ByteNot (byte: BYTE) : BYTE ;

  (*
     ByteShr - returns a, byte, which has been shifted, count
               bits to the right.
  *)

  ByteShr
  PROCEDURE ByteShr (byte: BYTE; count: CARDINAL) : BYTE ;

  (*
     ByteShl - returns a, byte, which has been shifted, count
               bits to the left.
  *)

  ByteShl
  PROCEDURE ByteShl (byte: BYTE; count: CARDINAL) : BYTE ;

  (*
     ByteSar - shift byte arthemetic right.  Preserves the top
               end bit and as the value is shifted right.
  *)

  ByteSar
  PROCEDURE ByteSar (byte: BYTE; count: CARDINAL) : BYTE ;

  (*
     ByteRor - returns a, byte, which has been rotated, count
               bits to the right.
  *)

  ByteRor
  PROCEDURE ByteRor (byte: BYTE; count: CARDINAL) : BYTE ;

  (*
     ByteRol - returns a, byte, which has been rotated, count
               bits to the left.
  *)

  ByteRol
  PROCEDURE ByteRol (byte: BYTE; count: CARDINAL) : BYTE ;

  (*
     HighNibble - returns the top nibble only from, byte.
                  The top nibble of, byte, is extracted and
                  returned in the bottom nibble of the return
                  value.
  *)

  HighNibble
  PROCEDURE HighNibble (byte: BYTE) : BYTE ;

  (*
     LowNibble - returns the low nibble only from, byte.
                 The top nibble is replaced by zeros.
  *)

  LowNibble
  PROCEDURE LowNibble (byte: BYTE) : BYTE ;

  (*
     Swap - swaps the low and high nibbles in the, byte.
  *)

  Swap
  PROCEDURE Swap (byte: BYTE) : BYTE ;

  END BitByteOps.

