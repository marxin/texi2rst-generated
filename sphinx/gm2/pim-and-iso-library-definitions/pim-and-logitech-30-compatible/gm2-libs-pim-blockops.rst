.. _gm2-libs-pim-blockops:

gm2-libs-pim/BlockOps
^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE BlockOps ;

  FROM SYSTEM IMPORT ADDRESS ;

  (*
     MoveBlockForward - moves, n, bytes from, src, to, dest.
                        Starts copying from src and keep copying
                        until, n, bytes have been copied.
  *)

  BlockMoveForward
  PROCEDURE BlockMoveForward (dest, src: ADDRESS; n: CARDINAL) ;

  (*
     MoveBlockBackward - moves, n, bytes from, src, to, dest.
                         Starts copying from src+n and keeps copying
                         until, n, bytes have been copied.
                         The last datum to be copied will be the byte
                         at address, src.
  *)

  BlockMoveBackward
  PROCEDURE BlockMoveBackward (dest, src: ADDRESS; n: CARDINAL) ;

  (*
     BlockClear - fills, block..block+n-1, with zero's.
  *)

  BlockClear
  PROCEDURE BlockClear (block: ADDRESS; n: CARDINAL) ;

  (*
     BlockSet - fills, n, bytes starting at, block, with a pattern
                defined at address pattern..pattern+patternSize-1.
  *)

  BlockSet
  PROCEDURE BlockSet (block: ADDRESS; n: CARDINAL;
                      pattern: ADDRESS; patternSize: CARDINAL) ;

  (*
     BlockEqual - returns TRUE if the blocks defined, a..a+n-1, and,
                  b..b+n-1 contain the same bytes.
  *)

  BlockEqual
  PROCEDURE BlockEqual (a, b: ADDRESS; n: CARDINAL) : BOOLEAN ;

  (*
     BlockPosition - searches for a pattern as defined by
                     pattern..patternSize-1 in the block,
                     block..block+blockSize-1.  It returns
                     the offset from block indicating the
                     first occurence of, pattern.
                     MAX(CARDINAL) is returned if no match
                     is detected.
  *)

  BlockPosition
  PROCEDURE BlockPosition (block: ADDRESS; blockSize: CARDINAL;
                           pattern: ADDRESS; patternSize: CARDINAL) : CARDINAL ;

  END BlockOps.

