.. _gm2-libs-m2dependent:

gm2-libs/M2Dependent
^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE M2Dependent ;

  FROM SYSTEM IMPORT ADDRESS ;

  TYPE
  ArgCVEnvP (type)
     ArgCVEnvP = PROCEDURE (INTEGER, ADDRESS, ADDRESS) ;

  ConstructModules
  PROCEDURE ConstructModules (applicationmodule: ADDRESS;
                              argc: INTEGER; argv, envp: ADDRESS) ;

  DeconstructModules
  PROCEDURE DeconstructModules (applicationmodule: ADDRESS;
                                argc: INTEGER; argv, envp: ADDRESS) ;

  (*
     RegisterModule - adds module name to the list of outstanding
                      modules which need to have their dependencies
                      explored to determine initialization order.
  *)

  RegisterModule
  PROCEDURE RegisterModule (name: ADDRESS;
                            init, fini:  ArgCVEnvP;
                            dependencies: PROC) ;

  (*
     RequestDependant - used to specify that modulename is dependant upon
                        module dependantmodule.
  *)

  RequestDependant
  PROCEDURE RequestDependant (modulename, dependantmodule: ADDRESS) ;

  END M2Dependent.

