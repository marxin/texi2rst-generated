.. _gm2-libs-pim-bitwordops:

gm2-libs-pim/BitWordOps
^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE BitWordOps ;

  FROM SYSTEM IMPORT WORD ;

  (*
     GetBits - returns the bits firstBit..lastBit from source.
               Bit 0 of word maps onto the firstBit of source.
  *)

  GetBits
  PROCEDURE GetBits (source: WORD; firstBit, lastBit: CARDINAL) : WORD ;

  (*
     SetBits - sets bits in, word, starting at, firstBit, and ending at,
               lastBit, with, pattern.  The bit zero of, pattern, will
               be placed into, word, at position, firstBit.
  *)

  SetBits
  PROCEDURE SetBits (VAR word: WORD; firstBit, lastBit: CARDINAL;
                     pattern: WORD) ;

  (*
     WordAnd - returns a bitwise (left AND right)
  *)

  WordAnd
  PROCEDURE WordAnd (left, right: WORD) : WORD ;

  (*
     WordOr - returns a bitwise (left OR right)
  *)

  WordOr
  PROCEDURE WordOr (left, right: WORD) : WORD ;

  (*
     WordXor - returns a bitwise (left XOR right)
  *)

  WordXor
  PROCEDURE WordXor (left, right: WORD) : WORD ;

  (*
     WordNot - returns a word with all bits inverted.
  *)

  WordNot
  PROCEDURE WordNot (word: WORD) : WORD ;

  (*
     WordShr - returns a, word, which has been shifted, count
               bits to the right.
  *)

  WordShr
  PROCEDURE WordShr (word: WORD; count: CARDINAL) : WORD ;

  (*
     WordShl - returns a, word, which has been shifted, count
               bits to the left.
  *)

  WordShl
  PROCEDURE WordShl (word: WORD; count: CARDINAL) : WORD ;

  (*
     WordSar - shift word arthemetic right.  Preserves the top
               end bit and as the value is shifted right.
  *)

  WordSar
  PROCEDURE WordSar (word: WORD; count: CARDINAL) : WORD ;

  (*
     WordRor - returns a, word, which has been rotated, count
               bits to the right.
  *)

  WordRor
  PROCEDURE WordRor (word: WORD; count: CARDINAL) : WORD ;

  (*
     WordRol - returns a, word, which has been rotated, count
               bits to the left.
  *)

  WordRol
  PROCEDURE WordRol (word: WORD; count: CARDINAL) : WORD ;

  (*
     HighByte - returns the top byte only from, word.
                The byte is returned in the bottom byte
                in the return value.
  *)

  HighByte
  PROCEDURE HighByte (word: WORD) : WORD ;

  (*
     LowByte - returns the low byte only from, word.
               The byte is returned in the bottom byte
               in the return value.
  *)

  LowByte
  PROCEDURE LowByte (word: WORD) : WORD ;

  (*
     Swap - byte flips the contents of word.
  *)

  Swap
  PROCEDURE Swap (word: WORD) : WORD ;

  END BitWordOps.

