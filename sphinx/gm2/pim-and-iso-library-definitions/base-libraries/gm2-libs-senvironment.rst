.. _gm2-libs-senvironment:

gm2-libs/SEnvironment
^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE SEnvironment ;

  FROM DynamicStrings IMPORT String ;
  EXPORT QUALIFIED GetEnvironment ;

  (*
     GetEnvironment - gets the environment variable Env and places
        	       	    a copy of its value into String, dest.
                      It returns TRUE if the string Env was found in
                      the processes environment.
  *)

  GetEnvironment
  PROCEDURE GetEnvironment (Env: String;
                            VAR dest: String) : BOOLEAN ;

  (*
     PutEnvironment - change or add an environment variable definition EnvDef.
                      TRUE is returned if the environment variable was
                      set or changed successfully.
  *)

  PutEnvironment
  PROCEDURE PutEnvironment (EnvDef: String) : BOOLEAN ;

  END SEnvironment.

