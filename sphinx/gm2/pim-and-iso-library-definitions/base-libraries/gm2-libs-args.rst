.. _gm2-libs-args:

gm2-libs/Args
^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE Args ;

  EXPORT QUALIFIED GetArg, Narg ;

  (*
     GetArg - returns the nth argument from the command line.
              The success of the operation is returned.
  *)

  GetArg
  PROCEDURE GetArg (VAR a: ARRAY OF CHAR; n: CARDINAL) : BOOLEAN ;

  (*
     Narg - returns the number of arguments available from
            command line.
  *)

  Narg
  PROCEDURE Narg () : CARDINAL ;

  END Args.

