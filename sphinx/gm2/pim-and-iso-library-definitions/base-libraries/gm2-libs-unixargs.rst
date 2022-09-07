.. _gm2-libs-unixargs:

gm2-libs/UnixArgs
^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE UnixArgs ;

  FROM SYSTEM IMPORT ADDRESS ;

  EXPORT QUALIFIED GetArgC, GetArgV, GetEnvV ;

  GetArgC
  PROCEDURE GetArgC () : INTEGER ;
  GetArgV
  PROCEDURE GetArgV () : ADDRESS ;
  GetEnvV
  PROCEDURE GetEnvV () : ADDRESS ;

  END UnixArgs.

