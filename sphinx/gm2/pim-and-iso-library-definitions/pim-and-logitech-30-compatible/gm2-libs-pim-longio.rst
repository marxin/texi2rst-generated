.. _gm2-libs-pim-longio:

gm2-libs-pim/LongIO
^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE LongIO ;

  EXPORT QUALIFIED Done, ReadLongInt, WriteLongInt ;

  VAR
  Done (var)
     Done: BOOLEAN ;

  ReadLongInt
  PROCEDURE ReadLongInt (VAR i: LONGINT) ;
  WriteLongInt
  PROCEDURE WriteLongInt (i: LONGINT; n: CARDINAL) ;

  END LongIO.

