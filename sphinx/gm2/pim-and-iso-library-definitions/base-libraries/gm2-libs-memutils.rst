.. _gm2-libs-memutils:

gm2-libs/MemUtils
^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE MemUtils ;

  FROM SYSTEM IMPORT ADDRESS ;
  EXPORT QUALIFIED MemCopy, MemZero ;

  (*
     MemCopy - copys a region of memory to the required destination.
  *)

  MemCopy
  PROCEDURE MemCopy (from: ADDRESS; length: CARDINAL; to: ADDRESS) ;

  (*
     MemZero - sets a region of memory: a..a+length to zero.
  *)

  MemZero
  PROCEDURE MemZero (a: ADDRESS; length: CARDINAL) ;

  END MemUtils.

