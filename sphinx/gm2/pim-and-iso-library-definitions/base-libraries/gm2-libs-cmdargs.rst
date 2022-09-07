.. _gm2-libs-cmdargs:

gm2-libs/CmdArgs
^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE CmdArgs ;

  EXPORT QUALIFIED GetArg, Narg ;

  (*
     GetArg - returns the nth argument from the command line, CmdLine
              the success of the operation is returned.
  *)

  GetArg
  PROCEDURE GetArg (CmdLine: ARRAY OF CHAR;
                    n: CARDINAL; VAR Argi: ARRAY OF CHAR) : BOOLEAN ;

  (*
     Narg - returns the number of arguments available from
            command line, CmdLine.
  *)

  Narg
  PROCEDURE Narg (CmdLine: ARRAY OF CHAR) : CARDINAL ;

  END CmdArgs.

