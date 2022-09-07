.. _gm2-libs-scmdargs:

gm2-libs/SCmdArgs
^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE SCmdArgs ;

  FROM DynamicStrings IMPORT String ;

  EXPORT QUALIFIED GetArg, Narg ;

  (*
     GetArg - returns the nth argument from the command line, CmdLine
              the success of the operation is returned.
  *)

  GetArg
  PROCEDURE GetArg (CmdLine: String;
                    n: CARDINAL; VAR Argi: String) : BOOLEAN ;

  (*
     Narg - returns the number of arguments available from
            command line, CmdLine.
  *)

  Narg
  PROCEDURE Narg (CmdLine: String) : CARDINAL ;

  END SCmdArgs.

