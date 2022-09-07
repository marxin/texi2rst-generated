.. _gm2-libs-pim-bitblockops:

gm2-libs-pim/BitBlockOps
^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE BitBlockOps ;

  FROM SYSTEM IMPORT ADDRESS ;

  (*
     BlockAnd - performs a bitwise AND on blocks
                [dest..dest+size-1] := [dest..dest+size-1] AND
                                       [src..src+size-1]
  *)

  BlockAnd
  PROCEDURE BlockAnd (dest, src: ADDRESS; size: CARDINAL) ;

  (*
     BlockOr - performs a bitwise OR on blocks
               [dest..dest+size-1] := [dest..dest+size-1] OR
                                      [src..src+size-1]
  *)

  BlockOr
  PROCEDURE BlockOr (dest, src: ADDRESS; size: CARDINAL) ;

  (*
     BlockXor - performs a bitwise XOR on blocks
                [dest..dest+size-1] := [dest..dest+size-1] XOR
                                       [src..src+size-1]
  *)

  BlockXor
  PROCEDURE BlockXor (dest, src: ADDRESS; size: CARDINAL) ;

  (*
     BlockNot - performs a bitsize NOT on the block as defined
                by:  [dest..dest+size-1]
  *)

  BlockNot
  PROCEDURE BlockNot (dest: ADDRESS; size: CARDINAL) ;

  (*
     BlockShr - performs a block shift right of, count, bits.
                Where the block is defined as:
                [dest..dest+size-1].
                The block is considered to be an ARRAY OF BYTEs
                which is shifted, bit at a time over each byte in
                turn.  The left most byte is considered the byte
                located at the lowest address.
                If you require an endianness SHIFT use
                the SYSTEM.SHIFT procedure and declare the
                block as a POINTER TO set type.
  *)

  BlockShr
  PROCEDURE BlockShr (dest: ADDRESS; size, count: CARDINAL) ;

  (*
     BlockShl - performs a block shift left of, count, bits.
                Where the block is defined as:
                [dest..dest+size-1].
                The block is considered to be an ARRAY OF BYTEs
                which is shifted, bit at a time over each byte in
                turn.  The left most byte is considered the byte
                located at the lowest address.
                If you require an endianness SHIFT use
                the SYSTEM.SHIFT procedure and declare the
                block as a POINTER TO set type.
  *)

  BlockShl
  PROCEDURE BlockShl (dest: ADDRESS; size, count: CARDINAL) ;

  (*
     BlockRor - performs a block rotate right of, count, bits.
                Where the block is defined as:
                [dest..dest+size-1].
                The block is considered to be an ARRAY OF BYTEs
                which is rotated, bit at a time over each byte in
                turn.  The left most byte is considered the byte
                located at the lowest address.
                If you require an endianness ROTATE use
                the SYSTEM.ROTATE procedure and declare the
                block as a POINTER TO set type.
  *)

  BlockRor
  PROCEDURE BlockRor (dest: ADDRESS; size, count: CARDINAL) ;

  (*
     BlockRol - performs a block rotate left of, count, bits.
                Where the block is defined as:
                [dest..dest+size-1].
                The block is considered to be an ARRAY OF BYTEs
                which is rotated, bit at a time over each byte in
                turn.  The left most byte is considered the byte
                located at the lowest address.
                If you require an endianness ROTATE use
                the SYSTEM.ROTATE procedure and declare the
                block as a POINTER TO set type.
  *)

  BlockRol
  PROCEDURE BlockRol (dest: ADDRESS; size, count: CARDINAL) ;

  END BitBlockOps.

