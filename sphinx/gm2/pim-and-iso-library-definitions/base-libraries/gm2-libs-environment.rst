.. _gm2-libs-environment:

gm2-libs/Environment
^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE Environment ;

  EXPORT QUALIFIED GetEnvironment, PutEnvironment ;

  (*
     GetEnvironment - gets the environment variable Env and places
        	       	    a copy of its value into string, dest.
                      It returns TRUE if the string Env was found in
                      the processes environment.
  *)

  GetEnvironment
  PROCEDURE GetEnvironment (Env: ARRAY OF CHAR;
                            VAR dest: ARRAY OF CHAR) : BOOLEAN ;

  (*
     PutEnvironment - change or add an environment variable definition
                      EnvDef.
                      TRUE is returned if the environment variable was
                      set or changed successfully.
  *)

  PutEnvironment
  PROCEDURE PutEnvironment (EnvDef: ARRAY OF CHAR) : BOOLEAN ;

  END Environment.

