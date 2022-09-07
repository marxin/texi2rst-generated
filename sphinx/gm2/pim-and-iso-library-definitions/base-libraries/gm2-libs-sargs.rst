.. _gm2-libs-sargs:

gm2-libs/SArgs
^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE SArgs ;

  FROM DynamicStrings IMPORT String ;
  EXPORT QUALIFIED GetArg, Narg ;

  (*
     GetArg - returns the nth argument from the command line.
              The success of the operation is returned.
              If TRUE is returned then the string, s, contains a
              new string, otherwise s is set to NIL.
  *)

  GetArg
  PROCEDURE GetArg (VAR s: String ; n: CARDINAL) : BOOLEAN ;

  (*
     Narg - returns the number of arguments available from
            command line.
  *)

  Narg
  PROCEDURE Narg() : CARDINAL ;

  END SArgs.

