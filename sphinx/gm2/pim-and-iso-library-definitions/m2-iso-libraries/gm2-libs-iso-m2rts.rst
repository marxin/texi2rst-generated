.. _gm2-libs-iso-m2rts:

gm2-libs-iso/M2RTS
^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE M2RTS ;

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

  (*
     ExecuteTerminationProcedures - calls each installed termination
                                    procedure in reverse order.
  *)

  ExecuteTerminationProcedures
  PROCEDURE ExecuteTerminationProcedures ;

  (*
     InstallTerminationProcedure - installs a procedure, p, which will
                                   be called when the procedure
                                   ExecuteTerminationProcedures
                                   is invoked.  It returns TRUE is the
                                   procedure is installed.
  *)

  InstallTerminationProcedure
  PROCEDURE InstallTerminationProcedure (p: PROC) : BOOLEAN ;

  (*
     ExecuteInitialProcedures - executes the initial procedures installed
                                by InstallInitialProcedure.
  *)

  ExecuteInitialProcedures
  PROCEDURE ExecuteInitialProcedures ;

  (*
     InstallInitialProcedure - installs a procedure to be executed just
                               before the BEGIN code section of the main
                               program module.
  *)

  InstallInitialProcedure
  PROCEDURE InstallInitialProcedure (p: PROC) : BOOLEAN ;

  (*
     HALT - terminate the current program.  The procedure
            ExecuteTerminationProcedures
            is called before the program is stopped.  The parameter
            exitcode is optional.  If the parameter is not supplied
            HALT will call libc 'abort', otherwise it will exit with
            the code supplied.  Supplying a parameter to HALT has the
            same effect as calling ExitOnHalt with the same code and
            then calling HALT with no parameter.
  *)

  HALT
  PROCEDURE HALT ([exitcode: INTEGER = -1]) ;

  (*
     Halt - provides a more user friendly version of HALT, which takes
            four parameters to aid debugging.
  *)

  Halt
  PROCEDURE Halt (file: ARRAY OF CHAR; line: CARDINAL;
                  function: ARRAY OF CHAR; description: ARRAY OF CHAR) ;

  (*
     ExitOnHalt - if HALT is executed then call exit with the exit code, e.
  *)

  ExitOnHalt
  PROCEDURE ExitOnHalt (e: INTEGER) ;

  (*
     ErrorMessage - emits an error message to stderr and then calls exit (1).
  *)

  ErrorMessage
  PROCEDURE ErrorMessage (message: ARRAY OF CHAR;
                          file: ARRAY OF CHAR;
                          line: CARDINAL;
                          function: ARRAY OF CHAR) ;

  (*
     IsTerminating - Returns true if any coroutine has started program termination
                     and false otherwise.
  *)

  IsTerminating
  PROCEDURE IsTerminating () : BOOLEAN ;

  (*
     HasHalted - Returns true if a call to HALT has been made and false
                 otherwise.
  *)

  HasHalted
  PROCEDURE HasHalted () : BOOLEAN ;

  (*
     Length - returns the length of a string, a. This is called whenever
              the user calls LENGTH and the parameter cannot be calculated
              at compile time.
  *)

  Length
  PROCEDURE Length (a: ARRAY OF CHAR) : CARDINAL ;

  (*
     The following are the runtime exception handler routines.
  *)

  AssignmentException
  PROCEDURE AssignmentException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
  ReturnException
  PROCEDURE ReturnException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
  IncException
  PROCEDURE IncException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
  DecException
  PROCEDURE DecException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
  InclException
  PROCEDURE InclException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
  ExclException
  PROCEDURE ExclException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
  ShiftException
  PROCEDURE ShiftException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
  RotateException
  PROCEDURE RotateException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
  StaticArraySubscriptException
  PROCEDURE StaticArraySubscriptException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
  DynamicArraySubscriptException
  PROCEDURE DynamicArraySubscriptException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
  ForLoopBeginException
  PROCEDURE ForLoopBeginException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
  ForLoopToException
  PROCEDURE ForLoopToException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
  ForLoopEndException
  PROCEDURE ForLoopEndException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
  PointerNilException
  PROCEDURE PointerNilException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
  NoReturnException
  PROCEDURE NoReturnException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
  CaseException
  PROCEDURE CaseException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
  WholeNonPosDivException
  PROCEDURE WholeNonPosDivException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
  WholeNonPosModException
  PROCEDURE WholeNonPosModException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
  WholeZeroDivException
  PROCEDURE WholeZeroDivException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
  WholeZeroRemException
  PROCEDURE WholeZeroRemException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
  WholeValueException
  PROCEDURE WholeValueException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
  RealValueException
  PROCEDURE RealValueException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
  ParameterException
  PROCEDURE ParameterException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
  NoException
  PROCEDURE NoException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;

  END M2RTS.

