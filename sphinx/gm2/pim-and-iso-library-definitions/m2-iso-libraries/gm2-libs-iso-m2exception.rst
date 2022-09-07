.. _gm2-libs-iso-m2exception:

gm2-libs-iso/M2EXCEPTION
^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE M2EXCEPTION;

  (* Provides facilities for identifying language exceptions *)

  TYPE
  M2Exceptions (type)
    M2Exceptions =
      (indexException,     rangeException,         caseSelectException,  invalidLocation,
       functionException,  wholeValueException,    wholeDivException,    realValueException,
       realDivException,   complexValueException,  complexDivException,  protException,
       sysException,       coException,            exException
      );

  M2Exception
  PROCEDURE M2Exception (): M2Exceptions;
    (* If the current coroutine is in the exceptional execution state because of the raising
       of a language exception, returns the corresponding enumeration value, and otherwise
       raises an exception.
    *)

  IsM2Exception
  PROCEDURE IsM2Exception (): BOOLEAN;
    (* If the current coroutine is in the exceptional execution state because of the raising
       of a language exception, returns TRUE, and otherwise returns FALSE.
    *)

  END M2EXCEPTION.

