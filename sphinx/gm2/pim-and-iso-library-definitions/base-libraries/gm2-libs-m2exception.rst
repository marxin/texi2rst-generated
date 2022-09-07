.. _gm2-libs-m2exception:

gm2-libs/M2EXCEPTION
^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE M2EXCEPTION;

  (* This enumerated list of exceptions must match the exceptions in gm2-libs-iso to
     allow mixed module dialect projects.  *)

  TYPE
  M2Exceptions (type)
    M2Exceptions =
      (indexException,     rangeException,         caseSelectException,  invalidLocation,
       functionException,  wholeValueException,    wholeDivException,    realValueException,
       realDivException,   complexValueException,  complexDivException,  protException,
       sysException,       coException,            exException
      );

  (* If the program or coroutine is in the exception state then return the enumeration
     value representing the exception cause.  If it is not in the exception state then
     raises and exception (exException).  *)

  M2Exception
  PROCEDURE M2Exception () : M2Exceptions;

  (* Returns TRUE if the program or coroutine is in the exception state.
     Returns FALSE if the program or coroutine is not in the exception state.  *)

  IsM2Exception
  PROCEDURE IsM2Exception () : BOOLEAN;

  END M2EXCEPTION.

